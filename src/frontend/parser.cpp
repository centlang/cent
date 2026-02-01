#include "unicode.h"

#include "ast/expr/as_expr.h"
#include "ast/expr/call_expr.h"
#include "ast/expr/identifier.h"
#include "ast/expr/index_expr.h"
#include "ast/expr/literals.h"
#include "ast/expr/member_expr.h"
#include "ast/expr/method_expr.h"
#include "ast/expr/sizeof_expr.h"
#include "ast/expr/slice_expr.h"
#include "ast/expr/unary_expr.h"
#include "ast/expr/unwrap_expr.h"

#include "ast/stmt/assignment.h"
#include "ast/stmt/break_stmt.h"
#include "ast/stmt/continue_stmt.h"
#include "ast/stmt/for_loop.h"
#include "ast/stmt/return_stmt.h"
#include "ast/stmt/switch.h"
#include "ast/stmt/unreachable.h"
#include "ast/stmt/while_loop.h"

#include "ast/decl/var_decl.h"

#include "ast/type/array_type.h"
#include "ast/type/fn_pointer.h"
#include "ast/type/named_type.h"
#include "ast/type/optional.h"
#include "ast/type/pointer.h"
#include "ast/type/slice_type.h"
#include "ast/type/tuple_type.h"

#include "modules.h"

#include "frontend/parser.h"

namespace cent::frontend {

std::unique_ptr<ast::Module> Parser::parse() {
    using enum Token::Type;

    auto skip_until_decl = [&] {
        while (!match(Eof, Type, Enum, Fn, Const, Mut, With, Pub, Bang)) {
            next();
        }
    };

    auto result = std::make_unique<ast::Module>(m_filename);

    m_submodules_visiting[m_filename] = true;

    auto handle_type = [&](std::vector<ast::Attribute> attrs, bool is_public) {
        if (match(1, Equal)) {
            auto type = parse_type_alias(std::move(attrs), is_public);

            if (type) {
                result->types.push_back(std::move(type));
            } else {
                skip_until_decl();
            }
        } else {
            auto struct_decl = parse_struct(std::move(attrs), is_public);

            if (struct_decl) {
                result->types.push_back(std::move(struct_decl));
            } else {
                skip_until_decl();
            }
        }
    };

    while (true) {
        if (match(Eof)) {
            break;
        }

        if (match_next(Semicolon)) {
            continue;
        }

        if (match_next(With)) {
            if (!parse_with(*result)) {
                skip_until_decl();
                continue;
            }

            expect("`;`", Token::Type::Semicolon);

            continue;
        }

        auto attrs = parse_attrs();
        bool is_public = false;

        if (match_next(Pub)) {
            is_public = true;
        }

        if (match(Const, Mut)) {
            auto variable = parse_var(std::move(attrs), is_public);

            if (!variable) {
                skip_until_decl();
                continue;
            }

            expect("`;`", Token::Type::Semicolon);

            result->variables.push_back(std::move(variable));

            continue;
        }

        if (match_next(Type)) {
            handle_type(std::move(attrs), is_public);

            continue;
        }

        if (match_next(Union)) {
            auto union_decl = parse_union(std::move(attrs), is_public);

            if (union_decl) {
                result->types.push_back(std::move(union_decl));
            } else {
                skip_until_decl();
            }

            continue;
        }

        if (match_next(Enum)) {
            auto enum_decl = parse_enum(std::move(attrs), is_public);

            if (enum_decl) {
                result->types.push_back(std::move(enum_decl));
            } else {
                skip_until_decl();
            }

            continue;
        }

        if (match_next(Fn)) {
            auto function = parse_fn(std::move(attrs), is_public);

            if (function) {
                result->functions.push_back(std::move(function));
            } else {
                skip_until_decl();
            }

            continue;
        }

        error("expected declaration");
        skip_until_decl();
    }

    return result;
}

void Parser::expect_stmt(ast::BlockStmt& block) {
    using enum Token::Type;

    auto expect_semicolon = [&] { expect("`;`", Token::Type::Semicolon); };

    switch (peek().type) {
    case LeftBrace:
        if (auto stmt = expect_block()) {
            block.body.push_back(std::move(stmt));
        }

        return;
    case If:
        if (auto stmt = parse_if_else()) {
            block.body.push_back(std::move(stmt));
        }

        return;
    case Switch:
        parse_switch(block);
        return;
    case While:
        parse_while(block);
        return;
    case For:
        parse_for(block);
        return;
    case Return:
        parse_return(block);
        expect_semicolon();
        return;
    case Break:
        block.body.push_back(std::make_unique<ast::BreakStmt>(get().offset));
        expect_semicolon();
        return;
    case Continue:
        block.body.push_back(std::make_unique<ast::ContinueStmt>(get().offset));
        expect_semicolon();
        return;
    case Unreachable:
        block.body.push_back(std::make_unique<ast::Unreachable>(get().offset));
        expect_semicolon();
        return;
    case Bang:
    case Type:
    case Enum:
    case Let:
    case Mut:
    case Const:
        break;
    default:
        auto value = expect_expr(false);

        if (!value) {
            next();
            return;
        }

        if (!match(
                Equal, PlusEqual, MinusEqual, StarEqual, SlashEqual,
                PercentEqual, AndEqual, OrEqual, XorEqual)) {
            block.body.push_back(std::move(value));
            expect_semicolon();

            return;
        }

        parse_assignment(block, std::move(value));
        expect_semicolon();

        return;
    }

    auto attrs = parse_attrs();

    switch (peek().type) {
    case Type:
        next();

        if (match(1, Equal)) {
            if (auto decl = parse_type_alias(std::move(attrs))) {
                block.body.push_back(std::move(decl));
            }
        } else if (auto decl = parse_struct(std::move(attrs))) {
            block.body.push_back(std::move(decl));
        }

        return;
    case Enum:
        next();

        if (auto decl = parse_enum(std::move(attrs))) {
            block.body.push_back(std::move(decl));
        }

        return;
    case Let:
    case Mut:
    case Const:
        if (auto decl = parse_var(std::move(attrs))) {
            block.body.push_back(std::move(decl));
        }

        expect_semicolon();
        return;
    default:
        return;
    }
}

std::vector<ast::Attribute> Parser::parse_attrs() {
    std::vector<ast::Attribute> result;

    if (!match_next(Token::Type::Hash)) {
        return {};
    }

    if (!expect("`(`", Token::Type::LeftParen)) {
        return {};
    }

    auto attribute = expect("attribute name", Token::Type::Identifier);

    if (!attribute) {
        return {};
    }

    result.emplace_back(attribute->offset, attribute->value);

    while (match_next(Token::Type::Comma)) {
        attribute = expect("attribute name", Token::Type::Identifier);

        if (!attribute) {
            return {};
        }

        result.emplace_back(attribute->offset, attribute->value);
    }

    if (!expect("`)`", Token::Type::RightParen)) {
        return {};
    }

    return result;
}

std::vector<std::unique_ptr<ast::Expression>> Parser::parse_args() {
    std::vector<std::unique_ptr<ast::Expression>> result;

    if (!match(Token::Type::RightParen)) {
        auto expr = expect_expr(false);

        if (!expr) {
            return {};
        }

        result.push_back(std::move(expr));

        while (match_next(Token::Type::Comma)) {
            expr = expect_expr(false);

            if (!expr) {
                return {};
            }

            result.push_back(std::move(expr));
        }
    }

    return result;
}

std::vector<ast::StructLiteral::Field> Parser::parse_field_values() {
    std::vector<ast::StructLiteral::Field> result;

    auto parse_field = [&] {
        auto name = get();

        expect("`:`", Token::Type::Colon);

        if (auto value = expect_expr(false)) {
            result.emplace_back(
                OffsetValue{.value = name.value, .offset = name.offset},
                std::move(value));
        }
    };

    while (match(Token::Type::Identifier)) {
        parse_field();

        if (match(Token::Type::RightBrace)) {
            break;
        }

        expect("`,`", Token::Type::Comma);
    }

    return result;
}

std::optional<ast::FnProto> Parser::parse_fn_proto() {
    using enum Token::Type;

    bool variadic = false;
    std::vector<ast::FnProto::Param> params;

    bool had_default = false;

    auto parse_param = [&] {
        if (match_next(Ellipsis)) {
            variadic = true;
            return;
        }

        bool is_mutable = false;

        if (match_next(Mut)) {
            is_mutable = true;
        }

        auto name = expect("parameter name", Identifier);

        if (!name) {
            return;
        }

        std::unique_ptr<ast::Type> type = expect_var_type();

        if (!type) {
            return;
        }

        std::unique_ptr<ast::Expression> value = nullptr;

        if (match_next(Equal)) {
            had_default = true;
            value = expect_expr(false);

            if (!value) {
                return;
            }
        } else if (had_default) {
            error(name->offset, "default parameters must be at the end");
            return;
        }

        params.emplace_back(
            OffsetValue{.value = name->value, .offset = name->offset},
            std::move(type), std::move(value), is_mutable);
    };

    auto parse_params = [&] {
        if (match(Mut, Identifier, Ellipsis)) {
            parse_param();

            while (!variadic && match_next(Comma)) {
                parse_param();
            }
        }
    };

    if (!expect("`(`", LeftParen)) {
        return std::nullopt;
    }

    parse_params();

    if (!expect("`)`", RightParen)) {
        return std::nullopt;
    }

    std::unique_ptr<ast::Type> return_type = nullptr;

    if (!match(Semicolon, LeftBrace, RightParen, Comma)) {
        return_type = expect_type();

        if (!return_type) {
            return std::nullopt;
        }
    }

    return ast::FnProto{
        .params = std::move(params),
        .return_type = std::move(return_type),
        .variadic = variadic};
}

std::unique_ptr<ast::Expression> Parser::expect_prefix(bool is_condition) {
    using enum Token::Type;

    if (match(LeftBracket)) {
        auto offset = peek().offset;
        auto type = parse_array_type();

        if (!type) {
            return nullptr;
        }

        std::vector<std::unique_ptr<ast::Expression>> elements;

        if (!expect("`{`", LeftBrace)) {
            return nullptr;
        }

        while (true) {
            if (auto value = expect_expr(false)) {
                elements.push_back(std::move(value));
            }

            if (match(Eof)) {
                expected("`}`");
                return nullptr;
            }

            if (match_next(RightBrace)) {
                return std::make_unique<ast::ArrayLiteral>(
                    offset, std::move(type), std::move(elements));
            }

            expect("`,`", Comma);
        }
    }

    if (match(Sizeof)) {
        auto offset = get().offset;

        if (!expect("`(`", LeftParen)) {
            return nullptr;
        }

        auto type = expect_type();

        if (!type) {
            return nullptr;
        }

        if (!expect("`)`", RightParen)) {
            return nullptr;
        }

        return std::make_unique<ast::SizeofExpr>(offset, std::move(type));
    }

    auto token = expect(
        "expression", IntLiteral, FloatLiteral, StrLiteral, RuneLiteral, True,
        False, Null, Undefined, Identifier, Minus, Bang, Star, And, Not,
        LeftParen);

    if (!token) {
        return nullptr;
    }

    switch (token->type) {
    case IntLiteral:
        return std::make_unique<ast::IntLiteral>(token->offset, token->value);
    case FloatLiteral:
        return std::make_unique<ast::FloatLiteral>(token->offset, token->value);
    case StrLiteral:
        return std::make_unique<ast::StrLiteral>(token->offset, token->value);
    case RuneLiteral:
        return std::make_unique<ast::RuneLiteral>(
            token->offset, get_utf8(token->value));
    case True:
        return std::make_unique<ast::BoolLiteral>(token->offset, true);
    case False:
        return std::make_unique<ast::BoolLiteral>(token->offset, false);
    case Null:
        return std::make_unique<ast::NullLiteral>(token->offset);
    case Undefined:
        return std::make_unique<ast::Undefined>(token->offset);
    case Identifier: {
        std::vector<OffsetValue<std::string>> value;
        value.push_back(
            OffsetValue{.value = token->value, .offset = token->offset});

        while (match_next(ColonColon)) {
            auto name = expect("name", Identifier);

            if (!name) {
                return nullptr;
            }

            value.push_back(
                OffsetValue{.value = name->value, .offset = name->offset});
        }

        if (!is_condition && match_next(LeftBrace)) {
            auto fields = parse_field_values();

            if (!expect("`,` or `}`", RightBrace)) {
                return nullptr;
            }

            return std::make_unique<ast::StructLiteral>(
                token->offset,
                std::make_unique<ast::NamedType>(
                    token->offset, std::move(value),
                    std::vector<std::unique_ptr<ast::Type>>()),
                std::move(fields));
        }

        if (!is_condition && match(LeftParen) && match(1, Less)) {
            auto template_args = parse_template_args();

            if (match_next(LeftParen)) {
                auto args = parse_args();

                if (!expect("`)`", RightParen)) {
                    return nullptr;
                }

                return std::make_unique<ast::CallExprGeneric>(
                    token->offset,
                    std::make_unique<ast::Identifier>(
                        token->offset, std::move(value)),
                    std::move(template_args), std::move(args));
            }

            if (!expect("`{` or `(`", LeftBrace)) {
                return nullptr;
            }

            auto fields = parse_field_values();

            if (!expect("`,` or `}`", RightBrace)) {
                return nullptr;
            }

            return std::make_unique<ast::StructLiteral>(
                token->offset,
                std::make_unique<ast::NamedType>(
                    token->offset, std::move(value), std::move(template_args)),
                std::move(fields));
        }

        return std::make_unique<ast::Identifier>(
            token->offset, std::move(value));
    }
    case Minus:
    case Bang:
    case Star:
    case And:
    case Not:
        if (auto value = expect_access_or_call_expr(is_condition)) {
            return std::make_unique<ast::UnaryExpr>(
                token->offset,
                OffsetValue{.value = token->type, .offset = token->offset},
                std::move(value));
        }

        return nullptr;
    case LeftParen: {
        auto value = expect_expr(false);

        if (!value) {
            return nullptr;
        }

        if (match_next(RightParen)) {
            return value;
        }

        if (!expect("`)` or `,`", Comma)) {
            return nullptr;
        }

        std::vector<std::unique_ptr<ast::Expression>> elements;
        elements.push_back(std::move(value));

        while (true) {
            if (auto value = expect_expr(false)) {
                elements.push_back(std::move(value));
            }

            if (match(Eof)) {
                expected("`)`");
                return nullptr;
            }

            if (match_next(RightParen)) {
                return std::make_unique<ast::TupleLiteral>(
                    token->offset, std::move(elements));
            }

            expect("`,`", Token::Type::Comma);
        }
    }
    default:
        return nullptr;
    }
}

[[nodiscard]] std::unique_ptr<ast::Expression>
Parser::expect_access_or_call_expr(bool is_condition) {
    using enum Token::Type;

    auto expression = expect_prefix(is_condition);

    if (!expression) {
        return nullptr;
    }

    while (true) {
        if (match_next(LeftParen)) {
            auto args = parse_args();

            if (!expect("`)`", RightParen)) {
                return nullptr;
            }

            expression = std::make_unique<ast::CallExpr>(
                expression->offset, std::move(expression), std::move(args));

            continue;
        }

        if (match_next(LeftBracket)) {
            auto offset = peek().offset;

            std::unique_ptr<ast::Expression> low = nullptr;

            if (!match(Colon)) {
                low = expect_expr(false);

                if (!low) {
                    return nullptr;
                }
            }

            if (match_next(RightBracket)) {
                if (!low) {
                    return nullptr;
                }

                expression = std::make_unique<ast::IndexExpr>(
                    expression->offset, std::move(expression), std::move(low));

                continue;
            }

            if (!expect("`]` or `:`", Colon)) {
                return nullptr;
            }

            std::unique_ptr<ast::Expression> high = nullptr;

            if (!match(RightBracket)) {
                high = expect_expr(false);

                if (!high) {
                    return nullptr;
                }
            }

            if (!expect("`]`", RightBracket)) {
                return nullptr;
            }

            if (!low && !high) {
                error(offset, "slice expressions require at least one index");

                return nullptr;
            }

            expression = std::make_unique<ast::SliceExpr>(
                expression->offset, std::move(expression), std::move(low),
                std::move(high));

            continue;
        }

        if (!match_next(Dot)) {
            return expression;
        }

        if (match_next(Bang)) {
            expression = std::make_unique<ast::UnwrapExpr>(
                expression->offset, std::move(expression));

            continue;
        }

        auto member = expect("member name", Identifier, IntLiteral);

        if (!member) {
            return nullptr;
        }

        if (match_next(LeftParen)) {
            auto args = parse_args();

            if (!expect("`)`", RightParen)) {
                return nullptr;
            }

            expression = std::make_unique<ast::MethodExpr>(
                expression->offset, std::move(expression),
                OffsetValue{.value = member->value, .offset = member->offset},
                std::move(args));

            continue;
        }

        expression = std::make_unique<ast::MemberExpr>(
            expression->offset, std::move(expression), *member);
    }
}

[[nodiscard]] std::unique_ptr<ast::Expression>
Parser::expect_as_expr(bool is_condition) {
    auto expression = expect_access_or_call_expr(is_condition);

    if (!expression) {
        return nullptr;
    }

    if (!match_next(Token::Type::As)) {
        return expression;
    }

    auto type = expect_type();

    if (!type) {
        return nullptr;
    }

    return std::make_unique<ast::AsExpr>(
        expression->offset, std::move(expression), std::move(type));
}

std::unique_ptr<ast::BinaryExpr>
Parser::expect_infix(std::unique_ptr<ast::Expression> lhs, bool is_condition) {
    auto oper = get();
    auto rhs = expect_bin_expr(is_condition, precedence_of(oper.type) + 1);

    if (!rhs) {
        return nullptr;
    }

    return std::make_unique<ast::BinaryExpr>(
        lhs->offset, OffsetValue{.value = oper.type, .offset = oper.offset},
        std::move(lhs), std::move(rhs));
}

std::unique_ptr<ast::Expression>
Parser::expect_bin_expr(bool is_condition, std::uint8_t precedence) {
    auto expression = expect_as_expr(is_condition);

    while (precedence_of(peek().type) >= precedence) {
        if (!expression) {
            next();
            return nullptr;
        }

        expression = expect_infix(std::move(expression), is_condition);
    }

    return expression;
}

std::unique_ptr<ast::Expression> Parser::expect_range_expr(bool is_condition) {
    auto begin = expect_bin_expr(is_condition);

    if (!begin) {
        next();
        return nullptr;
    }

    bool inclusive = false;

    if (match_next(Token::Type::DotDotEqual)) {
        inclusive = true;
    } else if (!match_next(Token::Type::DotDot)) {
        return begin;
    }

    auto end = expect_bin_expr(is_condition);

    if (!end) {
        return nullptr;
    }

    return std::make_unique<ast::RangeLiteral>(
        begin->offset, inclusive, std::move(begin), std::move(end));
}

std::unique_ptr<ast::Expression> Parser::expect_expr(bool is_condition) {
    return expect_range_expr(is_condition);
}

std::unique_ptr<ast::BlockStmt> Parser::expect_block() {
    auto result = std::make_unique<ast::BlockStmt>(peek().offset);

    if (!expect("`{`", Token::Type::LeftBrace)) {
        return nullptr;
    }

    while (true) {
        if (match(Token::Type::Eof)) {
            expected("`}`");
            return result;
        }

        if (match_next(Token::Type::RightBrace)) {
            break;
        }

        if (match_next(Token::Type::Semicolon)) {
            continue;
        }

        expect_stmt(*result);
    }

    return result;
}

std::unique_ptr<ast::IfElse> Parser::parse_if_else() {
    auto offset = get().offset;

    auto condition = expect_expr(true);

    if (!condition) {
        return nullptr;
    }

    auto if_block = expect_block();

    if (!if_block) {
        return nullptr;
    }

    if (!match_next(Token::Type::Else)) {
        return std::make_unique<ast::IfElse>(
            condition->offset, std::move(condition), std::move(if_block));
    }

    if (match(Token::Type::If)) {
        auto else_block = parse_if_else();

        if (!else_block) {
            return nullptr;
        }

        return std::make_unique<ast::IfElse>(
            condition->offset, std::move(condition), std::move(if_block),
            std::move(else_block));
    }

    auto else_block = expect_block();

    if (!else_block) {
        return nullptr;
    }

    return std::make_unique<ast::IfElse>(
        offset, std::move(condition), std::move(if_block),
        std::move(else_block));
}

std::unique_ptr<ast::Type> Parser::expect_var_type() {
    if (!expect("`:`", Token::Type::Colon)) {
        return nullptr;
    }

    return expect_type();
}

std::unique_ptr<ast::Type> Parser::parse_arraylike_type() {
    if (!match(1, Token::Type::RightBracket)) {
        return parse_array_type();
    }

    auto offset = get().offset;

    if (!expect("`]`", Token::Type::RightBracket)) {
        return nullptr;
    }

    std::size_t is_mutable_offset = 0;
    bool is_mutable = false;

    if (match(Token::Type::Mut)) {
        is_mutable_offset = get().offset;
        is_mutable = true;
    }

    auto type = expect_type();

    if (!type) {
        return nullptr;
    }

    return std::make_unique<ast::SliceType>(
        offset, std::move(type), is_mutable);
}

std::unique_ptr<ast::ArrayType> Parser::parse_array_type() {
    auto offset = get().offset;

    std::unique_ptr<ast::Expression> size = expect_expr(false);

    if (!size) {
        return nullptr;
    }

    if (!expect("`]`", Token::Type::RightBracket)) {
        return nullptr;
    }

    auto type = expect_type();

    if (!type) {
        return nullptr;
    }

    if (auto* identifier = dynamic_cast<ast::Identifier*>(size.get())) {
        if (identifier->value.size() == 1 &&
            identifier->value[0].value == "_") {
            return std::make_unique<ast::ArrayType>(
                offset, std::move(type), nullptr);
        }
    }

    return std::make_unique<ast::ArrayType>(
        offset, std::move(type), std::move(size));
}

std::unique_ptr<ast::Type> Parser::expect_type() {
    using enum Token::Type;

    auto offset = peek().offset;

    if (match_next(Star)) {
        bool is_mutable = false;

        if (match_next(Mut)) {
            is_mutable = true;
        }

        auto type = expect_type();

        if (!type) {
            return nullptr;
        }

        return std::make_unique<ast::Pointer>(
            offset, std::move(type), is_mutable);
    }

    if (match_next(Question)) {
        auto type = expect_type();

        if (!type) {
            return nullptr;
        }

        return std::make_unique<ast::Optional>(offset, std::move(type));
    }

    if (match_next(LeftParen)) {
        std::vector<std::unique_ptr<ast::Type>> types;

        auto parse_type = [&] {
            auto type = expect_type();

            if (!type) {
                return false;
            }

            types.push_back(std::move(type));

            return true;
        };

        if (!parse_type()) {
            return nullptr;
        }

        while (match_next(Comma)) {
            if (!parse_type()) {
                return nullptr;
            }
        }

        if (!expect("`)`", RightParen)) {
            return nullptr;
        }

        return std::make_unique<ast::TupleType>(offset, std::move(types));
    }

    if (match_next(Fn)) {
        auto proto = parse_fn_proto();

        if (!proto) {
            return nullptr;
        }

        return std::make_unique<ast::FnPointer>(offset, std::move(*proto));
    }

    if (match(LeftBracket)) {
        return parse_arraylike_type();
    }

    std::vector<OffsetValue<std::string>> value;
    auto token = expect("name", Identifier);

    if (!token) {
        return nullptr;
    }

    value.push_back(
        OffsetValue{.value = token->value, .offset = token->offset});

    while (match_next(ColonColon)) {
        auto name = expect("name", Identifier);

        if (!name) {
            return nullptr;
        }

        value.push_back(
            OffsetValue{.value = name->value, .offset = name->offset});
    }

    auto template_args = parse_template_args();

    return std::make_unique<ast::NamedType>(
        offset, std::move(value), std::move(template_args));
}

void Parser::parse_switch(ast::BlockStmt& block) {
    auto offset = get().offset;
    auto value = expect_expr(true);

    if (!value) {
        return;
    }

    if (!expect("`{`", Token::Type::LeftBrace)) {
        return;
    }

    std::vector<ast::Switch::Case> cases;
    std::unique_ptr<ast::BlockStmt> else_block = nullptr;

    while (!match(Token::Type::RightBrace, Token::Type::Else)) {
        std::vector<std::unique_ptr<ast::Expression>> values;

        auto value = expect_expr(true);

        if (!value) {
            return;
        }

        values.push_back(std::move(value));

        while (match_next(Token::Type::Comma)) {
            auto value = expect_expr(true);

            if (!value) {
                return;
            }

            values.push_back(std::move(value));
        }

        auto body = expect_block();

        if (!body) {
            return;
        }

        cases.emplace_back(std::move(values), std::move(body));
    }

    if (match_next(Token::Type::Else)) {
        else_block = expect_block();

        if (!else_block) {
            return;
        }
    }

    if (!expect("`}`", Token::Type::RightBrace)) {
        return;
    }

    block.body.push_back(std::make_unique<ast::Switch>(
        offset, std::move(value), std::move(cases), std::move(else_block)));
}

void Parser::parse_while(ast::BlockStmt& block) {
    auto offset = get().offset;
    auto condition = expect_expr(true);

    if (!condition) {
        return;
    }

    auto body = expect_block();

    if (!body) {
        return;
    }

    block.body.push_back(std::make_unique<ast::WhileLoop>(
        offset, std::move(condition), std::move(body)));
}

void Parser::parse_for(ast::BlockStmt& block) {
    auto offset = get().offset;

    bool is_mutable = false;

    if (match_next(Token::Type::Mut)) {
        is_mutable = true;
    }

    auto name = expect("variable name", Token::Type::Identifier);

    if (!name) {
        return;
    }

    if (!expect("`in`", Token::Type::In)) {
        return;
    }

    auto value = expect_expr(true);

    if (!value) {
        return;
    }

    auto body = expect_block();

    if (!body) {
        return;
    }

    block.body.push_back(std::make_unique<ast::ForLoop>(
        offset, is_mutable,
        OffsetValue{.value = std::move(name->value), .offset = name->offset},
        std::move(value), std::move(body)));
}

void Parser::parse_return(ast::BlockStmt& block) {
    auto offset = get().offset;

    std::unique_ptr<ast::Expression> value = nullptr;

    if (!match(Token::Type::Semicolon)) {
        value = expect_expr(false);

        if (!value) {
            return;
        }
    }

    block.body.push_back(
        std::make_unique<ast::ReturnStmt>(offset, std::move(value)));
}

void Parser::parse_assignment(
    ast::BlockStmt& block, std::unique_ptr<ast::Expression> variable) {
    auto oper = get();
    auto value = expect_expr(false);

    if (!value) {
        return;
    }

    block.body.push_back(std::make_unique<ast::Assignment>(
        variable->offset, std::move(variable), std::move(value),
        OffsetValue{.value = oper.type, .offset = oper.offset}));
}

std::vector<ast::Struct::Field> Parser::parse_fields() {
    std::vector<ast::Struct::Field> result;

    auto parse_field = [&] {
        auto name = get();

        if (auto type = expect_var_type()) {
            result.emplace_back(
                OffsetValue{.value = name.value, .offset = name.offset},
                std::move(type));
        }
    };

    while (match(Token::Type::Identifier)) {
        parse_field();

        if (match(Token::Type::RightBrace)) {
            break;
        }

        expect("`,`", Token::Type::Comma);
    }

    return result;
}

std::vector<std::unique_ptr<ast::Type>> Parser::parse_template_args() {
    std::vector<std::unique_ptr<ast::Type>> result;

    if (!match(Token::Type::LeftParen) || !match(1, Token::Type::Less)) {
        return result;
    }

    next();
    next();

    if (match_next(Token::Type::Greater)) {
        expect("`)`", Token::Type::RightParen);

        return result;
    }

    auto type = expect_type();

    if (!type) {
        return {};
    }

    result.push_back(std::move(type));

    while (match_next(Token::Type::Comma)) {
        type = expect_type();

        if (!type) {
            return {};
        }

        result.push_back(std::move(type));
    }

    if (!expect("`>`", Token::Type::Greater)) {
        return result;
    }

    expect("`)`", Token::Type::RightParen);
    return result;
}

std::vector<OffsetValue<std::string>> Parser::parse_template_params() {
    auto offset = peek().offset;

    std::vector<OffsetValue<std::string>> result;

    if (!match_next(Token::Type::LeftParen)) {
        return result;
    }

    if (!expect("`<`", Token::Type::Less)) {
        return result;
    }

    while (match(Token::Type::Identifier)) {
        auto param = get();
        result.emplace_back(param.value, param.offset);

        if (match(Token::Type::Greater)) {
            break;
        }

        expect("`,`", Token::Type::Comma);
    }

    next();
    expect("`)`", Token::Type::RightParen);

    return result;
}

std::vector<ast::EnumDecl::Field> Parser::parse_enum_fields() {
    std::vector<ast::EnumDecl::Field> result;

    auto parse_field = [&] {
        auto name = get();

        if (match_next(Token::Type::Equal)) {
            auto value = expect_expr(false);

            if (!value) {
                return;
            }

            result.emplace_back(
                OffsetValue{.value = name.value, .offset = name.offset},
                std::move(value));
        } else {
            result.emplace_back(
                OffsetValue{.value = name.value, .offset = name.offset});
        }
    };

    while (match(Token::Type::Identifier)) {
        parse_field();

        if (match(Token::Type::RightBrace)) {
            break;
        }

        expect("`,`", Token::Type::Comma);
    }

    return result;
}

std::unique_ptr<ast::VarDecl>
Parser::parse_var(std::vector<ast::Attribute> attrs, bool is_public) {
    auto offset = peek().offset;

    auto mutability = [&] {
        switch (get().type) {
        case Token::Type::Let:
            return ast::VarDecl::Mut::Immut;
        case Token::Type::Mut:
            return ast::VarDecl::Mut::Mut;
        case Token::Type::Const:
            return ast::VarDecl::Mut::Const;
        default:
            return ast::VarDecl::Mut::Mut;
        }
    }();

    auto name = expect("variable name", Token::Type::Identifier);

    if (!name) {
        return nullptr;
    }

    std::unique_ptr<ast::Type> type = nullptr;

    if (match(Token::Type::Colon)) {
        type = expect_var_type();

        if (!type) {
            return nullptr;
        }
    }

    if (!match_next(Token::Type::Equal)) {
        if (!type) {
            expected("`=` or `:`");

            return nullptr;
        }

        return std::make_unique<ast::VarDecl>(
            offset, mutability,
            OffsetValue{.value = name->value, .offset = name->offset},
            std::move(type), nullptr, std::move(attrs), is_public);
    }

    auto value = expect_expr(false);

    if (!value) {
        return nullptr;
    }

    return std::make_unique<ast::VarDecl>(
        offset, mutability,
        OffsetValue{.value = name->value, .offset = name->offset},
        std::move(type), std::move(value), std::move(attrs), is_public);
}

std::unique_ptr<ast::FnDecl>
Parser::parse_fn(std::vector<ast::Attribute> attrs, bool is_public) {
    auto template_params = parse_template_params();
    auto name = expect("function name", Token::Type::Identifier);

    std::optional<OffsetValue<std::string>> type = std::nullopt;

    if (!name) {
        return nullptr;
    }

    if (match_next(Token::Type::ColonColon)) {
        type = {.value = name->value, .offset = name->offset};
        name = expect("function name", Token::Type::Identifier);

        if (!name) {
            return nullptr;
        }
    }

    auto proto = parse_fn_proto();

    if (!proto) {
        return nullptr;
    }

    std::unique_ptr<ast::Type> return_type = nullptr;

    if (!match(Token::Type::LeftBrace, Token::Type::Semicolon)) {
        return_type = expect_type();

        if (!return_type) {
            return nullptr;
        }
    }

    std::unique_ptr<ast::BlockStmt> body = nullptr;

    if (match(Token::Type::LeftBrace)) {
        body = expect_block();

        if (!body) {
            return nullptr;
        }
    } else if (!match_next(Token::Type::Semicolon)) {
        expected("`{` or `;`");

        return nullptr;
    }

    return std::make_unique<ast::FnDecl>(
        name->offset, std::move(type),
        OffsetValue{.value = name->value, .offset = name->offset},
        std::move(*proto), std::move(body), std::move(template_params),
        std::move(attrs), is_public);
}

std::unique_ptr<ast::Struct>
Parser::parse_struct(std::vector<ast::Attribute> attrs, bool is_public) {
    auto template_params = parse_template_params();
    auto name = expect("struct name", Token::Type::Identifier);

    if (!name) {
        return nullptr;
    }

    if (!expect("`{`", Token::Type::LeftBrace)) {
        return nullptr;
    }

    auto fields = parse_fields();

    if (!expect("`}`", Token::Type::RightBrace)) {
        return nullptr;
    }

    return std::make_unique<ast::Struct>(
        name->offset, OffsetValue{.value = name->value, .offset = name->offset},
        std::move(fields), std::move(template_params), std::move(attrs),
        is_public);
}

std::unique_ptr<ast::Union>
Parser::parse_union(std::vector<ast::Attribute> attrs, bool is_public) {
    auto name = expect("union name", Token::Type::Identifier);

    if (!name) {
        return nullptr;
    }

    auto template_params = parse_template_params();

    if (!expect("`{`", Token::Type::LeftBrace)) {
        return nullptr;
    }

    auto fields = parse_fields();

    if (!expect("`}`", Token::Type::RightBrace)) {
        return nullptr;
    }

    return std::make_unique<ast::Union>(
        name->offset, OffsetValue{.value = name->value, .offset = name->offset},
        std::move(fields), std::move(template_params), std::move(attrs),
        is_public);
}

std::unique_ptr<ast::TypeAlias>
Parser::parse_type_alias(std::vector<ast::Attribute> attrs, bool is_public) {
    auto name = expect("type name", Token::Type::Identifier);

    if (!name) {
        return nullptr;
    }

    if (!expect("`=`", Token::Type::Equal)) {
        return nullptr;
    }

    auto type = expect_type();

    if (!type) {
        return nullptr;
    }

    return std::make_unique<ast::TypeAlias>(
        name->offset, OffsetValue{.value = name->value, .offset = name->offset},
        std::move(type), std::move(attrs), is_public);
}

std::unique_ptr<ast::EnumDecl>
Parser::parse_enum(std::vector<ast::Attribute> attrs, bool is_public) {
    auto name = expect("enum name", Token::Type::Identifier);

    if (!name) {
        return nullptr;
    }

    std::unique_ptr<ast::Type> type = nullptr;

    if (!match(Token::Type::LeftBrace)) {
        type = expect_type();

        if (!type) {
            return nullptr;
        }
    }

    if (!expect("`{`", Token::Type::LeftBrace)) {
        return nullptr;
    }

    auto fields = parse_enum_fields();

    if (!expect("`}`", Token::Type::RightBrace)) {
        return nullptr;
    }

    return std::make_unique<ast::EnumDecl>(
        name->offset, OffsetValue{.value = name->value, .offset = name->offset},
        std::move(type), std::move(fields), std::move(attrs), is_public);
}

std::unique_ptr<ast::Module> Parser::parse_submodule(
    const std::filesystem::path& path, std::string_view name) {
    auto& submodule_visiting = m_submodules_visiting[path];

    if (submodule_visiting) {
        error("recursive import");
        return nullptr;
    }

    submodule_visiting = true;

    auto code = read_file(path.string());

    if (!code) {
        return nullptr;
    }

    Parser parser{*code, path.string()};
    parser.m_submodules_visiting = m_submodules_visiting;

    auto submodule = parser.parse();

    if (parser.had_error()) {
        m_had_error = true;
    }

    submodule_visiting = false;

    submodule->name = name;
    return submodule;
}

bool Parser::parse_with(ast::Module& module) {
    auto name = expect("module name", Token::Type::Identifier);

    if (!name) {
        return false;
    }

    std::vector<std::string> path;
    path.push_back(name->value);

    while (match_next(Token::Type::ColonColon)) {
        name = expect("module name", Token::Type::Identifier);

        if (!name) {
            return false;
        }

        path.push_back(name->value);
    }

    auto module_name = path.back();

    if (match_next(Token::Type::As)) {
        auto token = expect("module name", Token::Type::Identifier);

        if (!token) {
            return false;
        }

        module_name = token->value;
    }

    SearchPath search_path = {
        std::filesystem::absolute(m_filename).parent_path()};

    if (const auto* cent_path = std::getenv("CENTPATH")) {
        auto paths = split(cent_path, ':');
        search_path.reserve(paths.size() + search_path.size());

        for (const auto& path : paths) {
            search_path.emplace_back(path);
        }
    } else {
        search_path.emplace_back(
            get_exe_path().parent_path().parent_path() / "lib" / "cent");
    }

    auto module_paths = find_module(path, search_path);

    if (module_paths.empty()) {
        error(
            name->offset, "could not find module {}", log::quoted(name->value));

        return false;
    }

    for (const auto& module_path : module_paths) {
        if (module_path == m_filename) {
            continue;
        }

        auto submodule = parse_submodule(module_path, module_name);

        if (!submodule) {
            return false;
        }

        module.submodules.push_back(std::move(submodule));
    }

    return true;
}

} // namespace cent::frontend
