#include "cent/util.h"

#include "cent/ast/as_expr.h"
#include "cent/ast/assignment.h"
#include "cent/ast/call_expr.h"
#include "cent/ast/identifier.h"
#include "cent/ast/literals.h"
#include "cent/ast/member_expr.h"
#include "cent/ast/method_expr.h"
#include "cent/ast/named_type.h"
#include "cent/ast/optional.h"
#include "cent/ast/pointer.h"
#include "cent/ast/return_stmt.h"
#include "cent/ast/unary_expr.h"
#include "cent/ast/var_decl.h"
#include "cent/ast/while_loop.h"

#include "cent/modules.h"

#include "cent/frontend/parser.h"

namespace cent::frontend {

std::unique_ptr<ast::Module> Parser::parse() noexcept {
    using enum Token::Type;

    auto skip_until_decl = [&] {
        while (!match(Eof, Fn, Struct, With)) {
            next();
        }
    };

    auto result = std::make_unique<ast::Module>();

    while (true) {
        if (match(Eof)) {
            break;
        }

        if (match(Semicolon)) {
            next();
            continue;
        }

        if (match(With)) {
            next();

            if (!parse_with(*result)) {
                skip_until_decl();
            }

            expect("';'", Token::Type::Semicolon);

            continue;
        }

        bool is_public = false;

        if (match(Pub)) {
            next();
            is_public = true;
        }

        if (match(Struct)) {
            next();

            if (!parse_struct(*result, is_public)) {
                skip_until_decl();
            }

            continue;
        }

        bool is_extern = false;

        if (match(Extern)) {
            next();
            is_extern = true;
        }

        if (match(Fn)) {
            next();

            if (!parse_fn(*result, is_public, is_extern)) {
                skip_until_decl();
            }

            continue;
        }

        error(get().span.begin, m_filename, "expected declaration");
        skip_until_decl();
    }

    return result;
}

void Parser::expect_stmt(ast::BlockStmt& block) noexcept {
    using enum Token::Type;

    switch (peek().type) {
    case LeftBrace:
        block.body.push_back(expect_block());
        return;
    case If:
        block.body.push_back(parse_if_else());
        return;
    case While:
        parse_while(block);
        return;
    case Let:
    case Mut:
        parse_var(block);
        break;
    case Return:
        parse_return(block);
        break;
    default:
        auto value = expect_expr();

        if (!value) {
            next();
            return;
        }

        if (!match(Equal, PlusEqual, MinusEqual, StarEqual, SlashEqual)) {
            block.body.push_back(std::move(value));
            break;
        }

        parse_assignment(block, std::move(value));

        break;
    }

    expect("';'", Token::Type::Semicolon);
}

std::vector<std::unique_ptr<ast::Expression>> Parser::parse_args() noexcept {
    std::vector<std::unique_ptr<ast::Expression>> result;

    if (!match(Token::Type::RightParen)) {
        result.push_back(expect_expr());

        while (match(Token::Type::Comma)) {
            next();
            result.push_back(expect_expr());
        }
    }

    return result;
}

std::vector<ast::StructLiteral::Field> Parser::parse_field_values() noexcept {
    std::vector<ast::StructLiteral::Field> result;

    auto parse_field = [&] {
        auto name = get();

        expect("':'", Token::Type::Colon);

        if (auto value = expect_expr()) {
            result.emplace_back(
                ast::SpanValue{name.value, name.span}, std::move(value));
        }
    };

    while (match(Token::Type::Identifier)) {
        parse_field();

        if (match(Token::Type::RightBrace)) {
            break;
        }

        expect("','", Token::Type::Comma);
    }

    return result;
}

std::unique_ptr<ast::Expression> Parser::expect_prefix() noexcept {
    using enum Token::Type;

    auto token = expect(
        "expression", IntLiteral, FloatLiteral, StrLiteral, True, False,
        Identifier, Minus, Bang, Star, And, LeftParen);

    if (!token) {
        return nullptr;
    }

    switch (token->type) {
    case IntLiteral:
        return std::make_unique<ast::IntLiteral>(token->span, token->value);
    case FloatLiteral:
        return std::make_unique<ast::FloatLiteral>(token->span, token->value);
    case StrLiteral:
        return std::make_unique<ast::StrLiteral>(token->span, token->value);
    case True:
        return std::make_unique<ast::BoolLiteral>(token->span, true);
    case False:
        return std::make_unique<ast::BoolLiteral>(token->span, false);
    case Identifier: {
        std::vector<ast::SpanValue<std::string>> value;
        value.push_back(ast::SpanValue{token->value, token->span});

        while (match(ColonColon)) {
            next();
            auto name = expect("name", Identifier);

            if (!name) {
                return nullptr;
            }

            value.push_back(ast::SpanValue{name->value, name->span});
        }

        auto type_end = peek().span.end;

        if (match(LeftBrace)) {
            next();

            auto fields = parse_field_values();
            auto end = peek().span.end;

            if (!expect("',' or '}'", RightBrace)) {
                return nullptr;
            }

            return std::make_unique<ast::StructLiteral>(
                Span{token->span.begin, end},
                std::make_unique<ast::NamedType>(
                    Span{token->span.begin, type_end}, std::move(value)),
                std::move(fields));
        }

        return std::make_unique<ast::Identifier>(
            Span{token->span.begin, type_end}, std::move(value));
    }
    case Minus:
    case Bang:
    case Star:
    case And:
        if (auto value = expect_member_expr()) {
            return std::make_unique<ast::UnaryExpr>(
                Span{token->span.begin, value->span.end},
                ast::SpanValue{token->type, token->span}, std::move(value));
        }

        return nullptr;
    case LeftParen: {
        auto value = expect_expr();
        expect("')'", RightParen);

        return value;
    }
    default:
        return nullptr;
    }
}

[[nodiscard]] std::unique_ptr<ast::Expression>
Parser::expect_member_expr() noexcept {
    auto expression = expect_prefix();

    if (!expression) {
        return nullptr;
    }

    if (!match(Token::Type::Dot)) {
        if (!match(Token::Type::LeftParen)) {
            return expression;
        }

        next();

        auto args = parse_args();
        auto end = peek().span.end;

        if (!expect("',' or ')'", Token::Type::RightParen)) {
            return nullptr;
        }

        return std::make_unique<ast::CallExpr>(
            Span{expression->span.begin, end}, std::move(expression),
            std::move(args));
    }

    std::vector<ast::SpanValue<std::string>> path;
    ast::SpanValue<std::string> name;

    auto end = peek().span.end;

    if (match(Token::Type::Dot)) {
        next();

        auto token = expect("member name", Token::Type::Identifier);

        if (!token) {
            return nullptr;
        }

        name = {token->value, token->span};
        end = token->span.end;
    }

    while (match(Token::Type::Dot)) {
        next();

        auto member = expect("member name", Token::Type::Identifier);

        if (!member) {
            return nullptr;
        }

        path.push_back(std::move(name));
        name = {member->value, member->span};

        end = member->span.end;
    }

    if (!match(Token::Type::LeftParen)) {
        path.push_back(std::move(name));

        return std::make_unique<ast::MemberExpr>(
            Span{expression->span.begin, end}, std::move(expression),
            std::move(path));
    }

    next();

    expression = std::make_unique<ast::MemberExpr>(
        Span{expression->span.begin, end}, std::move(expression),
        std::move(path));

    auto args = parse_args();
    end = peek().span.end;

    if (!expect("',' or ')'", Token::Type::RightParen)) {
        return nullptr;
    }

    return std::make_unique<ast::MethodExpr>(
        Span{expression->span.begin, end}, std::move(expression),
        std::move(name), std::move(args));
}

[[nodiscard]] std::unique_ptr<ast::Expression>
Parser::expect_as_expr() noexcept {
    auto expression = expect_member_expr();

    if (!match(Token::Type::As)) {
        return expression;
    }

    next();

    auto type = expect_type();

    if (!type) {
        return nullptr;
    }

    return std::make_unique<ast::AsExpr>(
        Span{expression->span.begin, type->span.end}, std::move(expression),
        std::move(type));
}

std::unique_ptr<ast::BinaryExpr>
Parser::expect_infix(std::unique_ptr<ast::Expression> lhs) noexcept {
    auto oper = get();
    auto rhs = expect_bin_expr(precedence_of(oper.type) + 1);

    if (!rhs) {
        return nullptr;
    }

    Span span{lhs->span.begin, rhs->span.end};

    return std::make_unique<ast::BinaryExpr>(
        span, ast::SpanValue{oper.type, oper.span}, std::move(lhs),
        std::move(rhs));
}

std::unique_ptr<ast::Expression>
Parser::expect_bin_expr(std::uint8_t precedence) noexcept {
    auto expression = expect_as_expr();

    while (precedence_of(peek().type) >= precedence) {
        if (!expression) {
            next();
            return nullptr;
        }

        expression = expect_infix(std::move(expression));
    }

    return expression;
}

std::unique_ptr<ast::Expression> Parser::expect_expr() noexcept {
    return expect_bin_expr();
}

std::unique_ptr<ast::BlockStmt> Parser::expect_block() noexcept {
    auto result = std::make_unique<ast::BlockStmt>(Span{peek().span.begin, {}});

    if (!expect("'{'", Token::Type::LeftBrace)) {
        return nullptr;
    }

    while (true) {
        if (match(Token::Type::Eof)) {
            result->span.end = peek().span.end;
            error(peek().span.begin, m_filename, "expected '}'");

            return result;
        }

        if (match(Token::Type::RightBrace)) {
            next();
            break;
        }

        if (match(Token::Type::Semicolon)) {
            next();
            continue;
        }

        expect_stmt(*result);
    }

    result->span.end = peek().span.end;
    return result;
}

std::unique_ptr<ast::IfElse> Parser::parse_if_else() noexcept {
    auto begin = get().span.begin;

    auto condition = expect_expr();

    if (!condition) {
        return nullptr;
    }

    auto if_block = expect_block();

    if (!if_block) {
        return nullptr;
    }

    if (!match(Token::Type::Else)) {
        return std::make_unique<ast::IfElse>(
            Span{condition->span.begin, if_block->span.end},
            std::move(condition), std::move(if_block));
    }

    next();

    if (match(Token::Type::If)) {
        auto else_block = parse_if_else();

        if (!else_block) {
            return nullptr;
        }

        return std::make_unique<ast::IfElse>(
            Span{condition->span.begin, else_block->span.end},
            std::move(condition), std::move(if_block), std::move(else_block));
    }

    auto else_block = expect_block();

    if (!else_block) {
        return nullptr;
    }

    return std::make_unique<ast::IfElse>(
        Span{begin, else_block->span.end}, std::move(condition),
        std::move(if_block), std::move(else_block));
}

std::unique_ptr<ast::Type> Parser::expect_var_type() noexcept {
    if (!expect("':'", Token::Type::Colon)) {
        return nullptr;
    }

    return expect_type();
}

std::unique_ptr<ast::Type> Parser::expect_type() noexcept {
    using enum Token::Type;

    auto begin = peek().span.begin;

    if (match(Star)) {
        next();

        bool is_mutable = false;

        if (match(Mut)) {
            is_mutable = true;
            next();
        }

        auto type = expect_type();

        if (!type) {
            return nullptr;
        }

        return std::make_unique<ast::Pointer>(
            Span{begin, type->span.end}, std::move(type), is_mutable);
    }

    if (match(QuestionMark)) {
        next();

        auto type = expect_type();

        if (!type) {
            return nullptr;
        }

        return std::make_unique<ast::Optional>(
            Span{begin, type->span.end}, std::move(type));
    }

    std::vector<ast::SpanValue<std::string>> value;
    auto token = expect("name", Identifier);

    if (!token) {
        return nullptr;
    }

    auto end = token->span.end;

    value.push_back(ast::SpanValue{token->value, token->span});

    while (match(ColonColon)) {
        next();
        auto name = expect("name", Identifier);

        if (!name) {
            return nullptr;
        }

        end = name->span.end;
        value.push_back(ast::SpanValue{name->value, name->span});
    }

    return std::make_unique<ast::NamedType>(Span{begin, end}, std::move(value));
}

void Parser::parse_var(ast::BlockStmt& block) noexcept {
    auto begin = peek().span.begin;
    auto is_mutable = get().type == Token::Type::Mut;

    auto name = expect("variable name", Token::Type::Identifier);

    if (!name) {
        return;
    }

    std::unique_ptr<ast::Type> type = nullptr;

    if (match(Token::Type::Colon)) {
        type = expect_var_type();

        if (!type) {
            return;
        }
    }

    if (!match(Token::Type::Equal)) {
        if (!type) {
            error(peek().span.begin, m_filename, "expected '=' or ':'");

            return;
        }

        block.body.push_back(std::make_unique<ast::VarDecl>(
            Span{begin, type->span.end}, is_mutable,
            ast::SpanValue{name->value, name->span}, std::move(type), nullptr));

        return;
    }

    next();

    auto value = expect_expr();

    if (!value) {
        return;
    }

    block.body.push_back(std::make_unique<ast::VarDecl>(
        Span{begin, value->span.end}, is_mutable,
        ast::SpanValue{name->value, name->span}, std::move(type),
        std::move(value)));
}

void Parser::parse_while(ast::BlockStmt& block) noexcept {
    auto begin = get().span.begin;

    auto condition = expect_expr();

    if (!condition) {
        return;
    }

    auto body = expect_block();

    if (!body) {
        return;
    }

    block.body.push_back(std::make_unique<ast::WhileLoop>(
        Span{begin, body->span.end}, std::move(condition), std::move(body)));
}

void Parser::parse_return(ast::BlockStmt& block) noexcept {
    auto span = get().span;

    std::unique_ptr<ast::Expression> value = nullptr;

    if (!match(Token::Type::Semicolon)) {
        value = expect_expr();
    }

    block.body.push_back(std::make_unique<ast::ReturnStmt>(
        Span{span.begin, value ? value->span.end : span.end},
        std::move(value)));
}

void Parser::parse_assignment(
    ast::BlockStmt& block, std::unique_ptr<ast::Expression> variable) noexcept {
    auto oper = get();
    auto value = expect_expr();

    if (!value) {
        return;
    }

    block.body.push_back(std::make_unique<ast::Assignment>(
        Span{variable->span.begin, value->span.end}, std::move(variable),
        std::move(value), ast::SpanValue{oper.type, oper.span}));
}

std::vector<ast::FnDecl::Param> Parser::parse_params() noexcept {
    std::vector<ast::FnDecl::Param> result;

    auto parse_param = [&] {
        auto name = expect("parameter name", Token::Type::Identifier);

        if (!name) {
            return;
        }

        if (auto type = expect_var_type()) {
            result.emplace_back(
                ast::SpanValue{name->value, name->span}, std::move(type));
        }
    };

    if (match(Token::Type::Identifier)) {
        parse_param();

        while (match(Token::Type::Comma)) {
            next();
            parse_param();
        }
    }

    return result;
}

std::vector<ast::Struct::Field> Parser::parse_fields() noexcept {
    std::vector<ast::Struct::Field> result;

    auto parse_field = [&] {
        auto name = get();

        if (auto type = expect_var_type()) {
            result.emplace_back(
                ast::SpanValue{name.value, name.span}, std::move(type));
        }
    };

    while (match(Token::Type::Identifier)) {
        parse_field();

        if (match(Token::Type::RightBrace)) {
            break;
        }

        expect("','", Token::Type::Comma);
    }

    return result;
}

bool Parser::parse_fn(
    ast::Module& module, bool is_public, bool is_extern) noexcept {
    auto name = expect("function name", Token::Type::Identifier);
    std::optional<ast::SpanValue<std::string>> type = std::nullopt;

    if (!name) {
        return false;
    }

    if (match(Token::Type::ColonColon)) {
        next();

        type = {name->value, name->span};
        name = expect("function name", Token::Type::Identifier);

        if (!name) {
            return false;
        }
    }

    if (!expect("'('", Token::Type::LeftParen)) {
        return false;
    }

    auto params = parse_params();

    if (!expect("')'", Token::Type::RightParen)) {
        return false;
    }

    std::unique_ptr<ast::Type> return_type = nullptr;

    if (!match(Token::Type::LeftBrace)) {
        return_type = expect_type();

        if (!return_type) {
            return false;
        }
    }

    std::unique_ptr<ast::BlockStmt> body = nullptr;

    if (match(Token::Type::LeftBrace)) {
        body = expect_block();
    } else if (match(Token::Type::Semicolon)) {
        next();
    } else {
        error(peek().span.begin, m_filename, "expected '{' or ';'");

        return false;
    }

    module.functions.push_back(std::make_unique<ast::FnDecl>(
        name->span,
        ast::FnDecl::Proto{
            std::move(type),
            {name->value, name->span},
            std::move(params),
            std::move(return_type)},
        std::move(body), is_public, is_extern));

    return true;
}

bool Parser::parse_struct(ast::Module& module, bool is_public) noexcept {
    auto name = expect("struct name", Token::Type::Identifier);

    if (!name) {
        return false;
    }

    if (!expect("'{'", Token::Type::LeftBrace)) {
        return false;
    }

    auto fields = parse_fields();

    auto end = peek().span.end;

    if (!expect("'}'", Token::Type::RightBrace)) {
        return false;
    }

    module.structs.push_back(std::make_unique<ast::Struct>(
        Span{name->span.begin, end}, ast::SpanValue{name->value, name->span},
        std::move(fields), is_public));

    return true;
}

bool Parser::parse_with(ast::Module& module) noexcept {
    auto name = expect("module name", Token::Type::Identifier);

    if (!name) {
        return false;
    }

    auto file = cent::find_module(name->value);

    if (!file) {
        error(
            name->span.begin, m_filename,
            fmt::format("could not find module '{}'", name->value));

        return false;
    }

    auto code = cent::read_file(*file);

    if (!code) {
        return false;
    }

    Parser parser{*code, file->relative_path().string()};
    auto submodule = parser.parse();

    if (!submodule) {
        return false;
    }

    module.submodules[name->value] = std::move(submodule);

    return true;
}

} // namespace cent::frontend
