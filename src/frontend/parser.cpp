#include "cent/ast/literals.h"
#include "cent/ast/unary_expr.h"

#include "cent/frontend/parser.h"

namespace cent {

std::unique_ptr<Program> Parser::parse() noexcept {
    auto result = std::make_unique<Program>();

    while (true) {
        if (match(Token::Type::Eof)) {
            break;
        }

        if (match(Token::Type::Semicolon)) {
            next();
            continue;
        }

        if (expect("function declaration", Token::Type::Fn)) {
            parse_fn(*result);
        } else {
            next();
            continue;
        }

        expect_semicolon();
    }

    return result;
}

void Parser::expect_semicolon() noexcept {
    expect("';'", Token::Type::Semicolon);
}

void Parser::expect_stmt(BlockStmt& block) noexcept {
    switch (peek().type) {
    case Token::Type::LeftBrace:
        block.body.push_back(expect_block());
        break;
    default:
        if (auto value = expect_expr()) {
            block.body.push_back(std::move(value));
        } else {
            next();
            return;
        }

        break;
    }

    expect_semicolon();
}

std::unique_ptr<Expression> Parser::expect_prefix() noexcept {
    auto token = expect(
        "expression", Token::Type::IntLiteral, Token::Type::FloatLiteral,
        Token::Type::True, Token::Type::False, Token::Type::Minus,
        Token::Type::LeftParen);

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
    case Token::Type::Minus:
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
    auto expression = expect_prefix();

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

std::vector<FnDecl::Param> Parser::parse_params() noexcept {
    std::vector<FnDecl::Param> result;

    auto parse_param = [&] {
        auto name = expect("parameter name", Token::Type::Identifier);

        if (!name) {
            return;
        }

        if (!expect("':'", Token::Type::Colon)) {
            return;
        }

        auto type = expect("parameter type", Token::Type::Identifier);

        if (!type) {
            return;
        }

        result.emplace_back(
            SpanValue{name->value, name->span},
            SpanValue{type->value, type->span});
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

void Parser::parse_fn(Program& program) noexcept {
    auto name = expect("function name", Token::Type::Identifier);

    if (!name) {
        return;
    }

    if (!expect("'('", Token::Type::LeftParen)) {
        return;
    }

    auto params = parse_params();

    if (!expect("')'", Token::Type::RightParen)) {
        return;
    }

    auto return_type = expect("return type", Token::Type::Identifier);

    if (!return_type) {
        return;
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
}

} // namespace cent
