#include "cent/ast/as_expr.h"
#include "cent/ast/assignment.h"
#include "cent/ast/call_expr.h"
#include "cent/ast/identifier.h"
#include "cent/ast/literals.h"
#include "cent/ast/member_expr.h"
#include "cent/ast/named_type.h"
#include "cent/ast/optional.h"
#include "cent/ast/pointer.h"
#include "cent/ast/return_stmt.h"
#include "cent/ast/unary_expr.h"
#include "cent/ast/var_decl.h"
#include "cent/ast/while_loop.h"

#include "cent/frontend/parser.h"

namespace cent::frontend {

std::unique_ptr<ast::Program> Parser::parse() noexcept {
    auto skip_until_decl = [&] {
        while (!match(Token::Type::Eof, Token::Type::Fn, Token::Type::Struct)) {
            next();
        }
    };

    auto result = std::make_unique<ast::Program>();

    while (true) {
        if (match(Token::Type::Eof)) {
            break;
        }

        if (match(Token::Type::Semicolon)) {
            next();
            continue;
        }

        if (match(Token::Type::Fn)) {
            next();

            if (!parse_fn(*result)) {
                skip_until_decl();
            }

            continue;
        }

        if (match(Token::Type::Struct)) {
            next();

            if (!parse_struct(*result)) {
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
    switch (peek().type) {
    case Token::Type::LeftBrace:
        block.body.push_back(expect_block());
        return;
    case Token::Type::If:
        block.body.push_back(parse_if_else());
        return;
    case Token::Type::While:
        parse_while(block);
        return;
    case Token::Type::Let:
    case Token::Type::Mut:
        parse_var(block);
        break;
    case Token::Type::Return:
        parse_return(block);
        break;
    default:
        auto value = expect_expr();

        if (!value) {
            next();
            return;
        }

        if (!match(
                Token::Type::Equal, Token::Type::PlusEqual,
                Token::Type::MinusEqual, Token::Type::StarEqual,
                Token::Type::SlashEqual)) {
            block.body.push_back(std::move(value));
            break;
        }

        parse_assignment(block, std::move(value), peek());

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
    auto token = expect(
        "expression", Token::Type::IntLiteral, Token::Type::FloatLiteral,
        Token::Type::True, Token::Type::False, Token::Type::Identifier,
        Token::Type::Minus, Token::Type::Bang, Token::Type::Star,
        Token::Type::And, Token::Type::LeftParen);

    if (!token) {
        return nullptr;
    }

    switch (token->type) {
    case Token::Type::IntLiteral:
        return std::make_unique<ast::IntLiteral>(token->span, token->value);
    case Token::Type::FloatLiteral:
        return std::make_unique<ast::FloatLiteral>(token->span, token->value);
    case Token::Type::True:
        return std::make_unique<ast::BoolLiteral>(token->span, true);
    case Token::Type::False:
        return std::make_unique<ast::BoolLiteral>(token->span, false);
    case Token::Type::Identifier: {
        if (match(Token::Type::LeftBrace)) {
            next();

            auto fields = parse_field_values();
            auto end = peek().span.end;

            if (!expect("',' or '}'", Token::Type::RightBrace)) {
                return nullptr;
            }

            return std::make_unique<ast::StructLiteral>(
                Span{token->span.begin, end},
                ast::SpanValue{token->value, token->span}, std::move(fields));
        }

        if (!match(Token::Type::LeftParen)) {
            return std::make_unique<ast::Identifier>(token->span, token->value);
        }

        next();

        auto args = parse_args();
        auto end = peek().span.end;

        if (!expect("',' or ')'", Token::Type::RightParen)) {
            return nullptr;
        }

        return std::make_unique<ast::CallExpr>(
            Span{token->span.begin, end},
            ast::SpanValue{token->value, token->span}, std::move(args));
    }
    case Token::Type::Minus:
    case Token::Type::Bang:
    case Token::Type::Star:
    case Token::Type::And:
        if (auto value = expect_member_expr()) {
            return std::make_unique<ast::UnaryExpr>(
                Span{token->span.begin, value->span.end},
                ast::SpanValue{token->type, token->span}, std::move(value));
        }

        return nullptr;
    case Token::Type::LeftParen: {
        auto value = expect_expr();
        expect("')'", Token::Type::RightParen);

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

    while (match(Token::Type::Dot)) {
        next();

        auto member = expect("member name", Token::Type::Identifier);

        if (!member) {
            return nullptr;
        }

        Span span{expression->span.begin, member->span.end};

        expression = std::make_unique<ast::MemberExpr>(
            span, std::move(expression),
            ast::SpanValue{member->value, member->span});
    }

    return expression;
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
    auto begin = peek().span.begin;

    if (match(Token::Type::Star)) {
        next();

        bool is_mutable = false;

        if (match(Token::Type::Mut)) {
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

    if (match(Token::Type::QuestionMark)) {
        next();

        auto type = expect_type();

        if (!type) {
            return nullptr;
        }

        return std::make_unique<ast::Optional>(
            Span{begin, type->span.end}, std::move(type));
    }

    auto type = expect("type", Token::Type::Identifier);

    if (!type) {
        return nullptr;
    }

    return std::make_unique<ast::NamedType>(Span{type->span}, type->value);
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
    ast::BlockStmt& block, std::unique_ptr<ast::Expression> variable,
    Token oper) noexcept {
    next();

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

bool Parser::parse_fn(ast::Program& program) noexcept {
    auto name = expect("function name", Token::Type::Identifier);

    if (!name) {
        return false;
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

    program.functions.push_back(std::make_unique<ast::FnDecl>(
        name->span,
        ast::FnDecl::Proto{
            {name->value, name->span},
            std::move(params),
            std::move(return_type)},
        std::move(body)));

    return true;
}

bool Parser::parse_struct(ast::Program& program) noexcept {
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

    program.structs.push_back(std::make_unique<ast::Struct>(
        Span{name->span.begin, end}, ast::SpanValue{name->value, name->span},
        std::move(fields)));

    return true;
}

} // namespace cent::frontend
