#include "cent/ast/assignment.h"
#include "cent/ast/call_expr.h"
#include "cent/ast/identifier.h"
#include "cent/ast/literals.h"
#include "cent/ast/member_expr.h"
#include "cent/ast/return_stmt.h"
#include "cent/ast/unary_expr.h"
#include "cent/ast/var_decl.h"
#include "cent/ast/while_loop.h"

#include "cent/frontend/parser.h"

namespace cent {

std::unique_ptr<Program> Parser::parse() noexcept {
    auto skip_until_decl = [&] {
        while (!match(Token::Type::Eof, Token::Type::Fn, Token::Type::Struct)) {
            next();
        }
    };

    auto result = std::make_unique<Program>();

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

void Parser::expect_stmt(BlockStmt& block) noexcept {
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

        if (!match(Token::Type::Equal)) {
            block.body.push_back(std::move(value));
        }

        parse_assignment(block, std::move(value));

        break;
    }

    expect("';'", Token::Type::Semicolon);
}

std::vector<std::unique_ptr<Expression>> Parser::parse_args() noexcept {
    std::vector<std::unique_ptr<Expression>> result;

    if (!match(Token::Type::RightParen)) {
        result.push_back(expect_expr());

        while (match(Token::Type::Comma)) {
            next();
            result.push_back(expect_expr());
        }
    }

    return result;
}

std::unique_ptr<Expression> Parser::expect_prefix() noexcept {
    auto token = expect(
        "expression", Token::Type::IntLiteral, Token::Type::FloatLiteral,
        Token::Type::True, Token::Type::False, Token::Type::Identifier,
        Token::Type::Minus, Token::Type::Bang, Token::Type::LeftParen);

    if (!token) {
        return nullptr;
    }

    switch (token->type) {
    case Token::Type::IntLiteral:
        return std::make_unique<IntLiteral>(token->span, token->value);
    case Token::Type::FloatLiteral:
        return std::make_unique<FloatLiteral>(token->span, token->value);
    case Token::Type::True:
        return std::make_unique<BoolLiteral>(token->span, true);
    case Token::Type::False:
        return std::make_unique<BoolLiteral>(token->span, false);
    case Token::Type::Identifier: {
        if (!match(Token::Type::LeftParen)) {
            return std::make_unique<Identifier>(token->span, token->value);
        }

        next();

        auto args = parse_args();
        auto end = peek().span.end;

        if (!expect("',' or ')'", Token::Type::RightParen)) {
            return nullptr;
        }

        return std::make_unique<CallExpr>(
            Span{token->span.begin, end}, SpanValue{token->value, token->span},
            std::move(args));
    }
    case Token::Type::Minus:
    case Token::Type::Bang:
        if (auto value = expect_prefix()) {
            return std::make_unique<UnaryExpr>(
                Span{token->span.begin, value->span.end},
                SpanValue{token->type, token->span}, std::move(value));
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

[[nodiscard]] std::unique_ptr<Expression>
Parser::expect_member_expr() noexcept {
    auto expression = expect_prefix();

    while (match(Token::Type::Dot)) {
        next();

        if (!expression) {
            return nullptr;
        }

        auto member = expect("member name", Token::Type::Identifier);

        if (!member) {
            return nullptr;
        }

        Span span{expression->span.begin, member->span.end};

        expression = std::make_unique<MemberExpr>(
            span, std::move(expression),
            SpanValue{member->value, member->span});
    }

    return expression;
}

std::unique_ptr<BinaryExpr>
Parser::expect_infix(std::unique_ptr<Expression> lhs) noexcept {
    auto oper = get();
    auto rhs = expect_bin_expr(precedence_of(oper.type) + 1);

    if (!rhs) {
        return nullptr;
    }

    Span span{lhs->span.begin, rhs->span.end};

    return std::make_unique<BinaryExpr>(
        span, SpanValue{oper.type, oper.span}, std::move(lhs), std::move(rhs));
}

std::unique_ptr<Expression>
Parser::expect_bin_expr(std::uint8_t precedence) noexcept {
    auto expression = expect_member_expr();

    while (precedence_of(peek().type) >= precedence) {
        if (!expression) {
            next();
            return nullptr;
        }

        expression = expect_infix(std::move(expression));
    }

    return expression;
}

std::unique_ptr<Expression> Parser::expect_expr() noexcept {
    return expect_bin_expr();
}

std::unique_ptr<BlockStmt> Parser::expect_block() noexcept {
    auto result = std::make_unique<BlockStmt>(Span{peek().span.begin, {}});

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

std::unique_ptr<IfElse> Parser::parse_if_else() noexcept {
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
        return std::make_unique<IfElse>(
            Span{condition->span.begin, if_block->span.end},
            std::move(condition), std::move(if_block));
    }

    next();

    if (match(Token::Type::If)) {
        auto else_block = parse_if_else();

        if (!else_block) {
            return nullptr;
        }

        return std::make_unique<IfElse>(
            Span{condition->span.begin, else_block->span.end},
            std::move(condition), std::move(if_block), std::move(else_block));
    }

    auto else_block = expect_block();

    if (!else_block) {
        return nullptr;
    }

    return std::make_unique<IfElse>(
        Span{begin, else_block->span.end}, std::move(condition),
        std::move(if_block), std::move(else_block));
}

std::optional<SpanValue<std::string_view>> Parser::expect_var_type() noexcept {
    if (!expect("':'", Token::Type::Colon)) {
        return std::nullopt;
    }

    auto token = expect("type", Token::Type::Identifier);

    if (!token) {
        return std::nullopt;
    }

    return {{token->value, token->span}};
}

void Parser::parse_var(BlockStmt& block) noexcept {
    auto begin = peek().span.begin;
    auto is_mutable = get().type == Token::Type::Mut;

    auto name = expect("variable name", Token::Type::Identifier);

    if (!name) {
        return;
    }

    auto type = expect_var_type();

    if (!type) {
        return;
    }

    if (!match(Token::Type::Equal)) {
        block.body.push_back(std::make_unique<VarDecl>(
            Span{begin, type->span.end}, is_mutable,
            SpanValue{name->value, name->span}, *type, nullptr));

        return;
    }

    next();

    auto value = expect_expr();

    if (!value) {
        return;
    }

    block.body.push_back(std::make_unique<VarDecl>(
        Span{begin, value->span.end}, is_mutable,
        SpanValue{name->value, name->span}, *type, std::move(value)));
}

void Parser::parse_while(BlockStmt& block) noexcept {
    auto begin = get().span.begin;

    auto condition = expect_expr();

    if (!condition) {
        return;
    }

    auto body = expect_block();

    if (!body) {
        return;
    }

    block.body.push_back(std::make_unique<WhileLoop>(
        Span{begin, body->span.end}, std::move(condition), std::move(body)));
}

void Parser::parse_return(BlockStmt& block) noexcept {
    auto span = get().span;

    std::unique_ptr<Expression> value = nullptr;

    if (!match(Token::Type::Semicolon)) {
        value = expect_expr();
    }

    block.body.push_back(std::make_unique<ReturnStmt>(
        Span{span.begin, value ? value->span.end : span.end},
        std::move(value)));
}

void Parser::parse_assignment(
    BlockStmt& block, std::unique_ptr<Expression> variable) noexcept {
    next();

    auto value = expect_expr();

    if (!value) {
        return;
    }

    block.body.push_back(std::make_unique<Assignment>(
        Span{variable->span.begin, value->span.end}, std::move(variable),
        std::move(value)));
}

std::vector<FnDecl::Param> Parser::parse_params() noexcept {
    std::vector<FnDecl::Param> result;

    auto parse_param = [&] {
        auto name = expect("parameter name", Token::Type::Identifier);

        if (!name) {
            return;
        }

        if (auto type = expect_var_type()) {
            result.emplace_back(
                SpanValue{name->value, name->span},
                SpanValue{type->value, type->span});
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

std::vector<Struct::Field> Parser::parse_fields() noexcept {
    std::vector<Struct::Field> result;

    auto parse_field = [&] {
        auto name = get();

        if (auto type = expect_var_type()) {
            result.emplace_back(
                SpanValue{name.value, name.span},
                SpanValue{type->value, type->span});
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

bool Parser::parse_fn(Program& program) noexcept {
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

    auto return_type = expect("return type", Token::Type::Identifier);

    if (!return_type) {
        return false;
    }

    std::unique_ptr<BlockStmt> body = nullptr;

    if (match(Token::Type::LeftBrace)) {
        body = expect_block();
    }

    program.functions.push_back(std::make_unique<FnDecl>(
        Span{name->span.begin, return_type->span.end},
        FnDecl::Proto{
            {name->value, name->span},
            std::move(params),
            {return_type->value, return_type->span}},
        std::move(body)));

    return true;
}

bool Parser::parse_struct(Program& program) noexcept {
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

    program.structs.push_back(std::make_unique<Struct>(
        Span{name->span.begin, end}, SpanValue{name->value, name->span},
        std::move(fields)));

    return true;
}

} // namespace cent
