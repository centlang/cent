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

void Parser::parse_fn(Program& program) noexcept {
    auto name = expect("function name", Token::Type::Identifier);

    if (!name) {
        return;
    }

    if (!expect("'('", Token::Type::LeftParen)) {
        return;
    }

    if (!expect("')'", Token::Type::RightParen)) {
        return;
    }

    auto return_type = expect("return type", Token::Type::Identifier);

    if (!return_type) {
        return;
    }

    program.functions.push_back(std::make_unique<FnDecl>(
        Span{name->span.begin, return_type->span.end},
        FnDecl::Proto{
            {name->value, name->span},
            {},
            {return_type->value, return_type->span}},
        nullptr));
}

} // namespace cent
