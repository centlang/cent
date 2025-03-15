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

        expect("';'", Token::Type::Semicolon);
    }

    return result;
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
