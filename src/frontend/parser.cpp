#include "cent/ast/expr/as_expr.h"
#include "cent/ast/expr/call_expr.h"
#include "cent/ast/expr/identifier.h"
#include "cent/ast/expr/index_expr.h"
#include "cent/ast/expr/literals.h"
#include "cent/ast/expr/member_expr.h"
#include "cent/ast/expr/method_expr.h"
#include "cent/ast/expr/slice_expr.h"
#include "cent/ast/expr/unary_expr.h"

#include "cent/ast/stmt/assignment.h"
#include "cent/ast/stmt/break_stmt.h"
#include "cent/ast/stmt/continue_stmt.h"
#include "cent/ast/stmt/return_stmt.h"
#include "cent/ast/stmt/unreachable.h"
#include "cent/ast/stmt/while_loop.h"

#include "cent/ast/decl/var_decl.h"

#include "cent/ast/type/array_type.h"
#include "cent/ast/type/named_type.h"
#include "cent/ast/type/optional.h"
#include "cent/ast/type/pointer.h"
#include "cent/ast/type/slice_type.h"
#include "cent/ast/type/tuple_type.h"

#include "cent/modules.h"

#include "cent/frontend/parser.h"

namespace cent::frontend {

std::unique_ptr<ast::Module> Parser::parse() {
    using enum Token::Type;

    auto skip_until_decl = [&] {
        while (!match(Eof, Type, Enum, Fn, Const, Mut, With)) {
            next();
        }
    };

    auto result = std::make_unique<ast::Module>(ModulePath{.file = m_filename});

    while (true) {
        if (match(Eof)) {
            break;
        }

        if (match(Semicolon)) {
            next();
            continue;
        }

        if (match(Const, Mut)) {
            auto variable = parse_var();

            if (!variable) {
                skip_until_decl();
                continue;
            }

            expect("';'", Token::Type::Semicolon);

            result->variables.push_back(std::move(variable));

            continue;
        }

        if (match(With)) {
            next();

            if (!parse_with(*result)) {
                skip_until_decl();
                continue;
            }

            expect("';'", Token::Type::Semicolon);

            continue;
        }

        bool is_public = false;

        if (match(Pub)) {
            next();
            is_public = true;
        }

        if (match(Type)) {
            next();

            auto struct_decl = parse_struct(is_public);

            if (struct_decl) {
                result->structs.push_back(std::move(struct_decl));
            } else {
                skip_until_decl();
            }

            continue;
        }

        if (match(Enum)) {
            next();

            auto enum_decl = parse_enum(is_public);

            if (enum_decl) {
                result->enums.push_back(std::move(enum_decl));
            } else {
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

            auto function = parse_fn(is_public, is_extern);

            if (function) {
                result->functions.push_back(std::move(function));
            } else {
                skip_until_decl();
            }

            continue;
        }

        error("expected declaration");
        next();

        skip_until_decl();
    }

    return result;
}

void Parser::expect_stmt(ast::BlockStmt& block) {
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
    case Fn:
        next();
        block.body.push_back(parse_fn());

        return;
    case Type:
        next();
        block.body.push_back(parse_struct());

        return;
    case Enum:
        next();
        block.body.push_back(parse_enum());

        return;
    case Let:
    case Mut:
    case Const:
        block.body.push_back(parse_var());
        break;
    case Return:
        parse_return(block);
        break;
    case Break:
        block.body.push_back(std::make_unique<ast::BreakStmt>(get().offset));
        break;
    case Continue:
        block.body.push_back(std::make_unique<ast::ContinueStmt>(get().offset));
        break;
    case Unreachable:
        block.body.push_back(std::make_unique<ast::Unreachable>(get().offset));
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
            break;
        }

        parse_assignment(block, std::move(value));

        break;
    }

    expect("';'", Token::Type::Semicolon);
}

std::vector<std::unique_ptr<ast::Expression>> Parser::parse_args() {
    std::vector<std::unique_ptr<ast::Expression>> result;

    if (!match(Token::Type::RightParen)) {
        result.push_back(expect_expr(false));

        while (match(Token::Type::Comma)) {
            next();
            result.push_back(expect_expr(false));
        }
    }

    return result;
}

std::vector<ast::StructLiteral::Field> Parser::parse_field_values() {
    std::vector<ast::StructLiteral::Field> result;

    auto parse_field = [&] {
        auto name = get();

        expect("':'", Token::Type::Colon);

        if (auto value = expect_expr(false)) {
            result.emplace_back(
                ast::OffsetValue{name.value, name.offset}, std::move(value));
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

std::unique_ptr<ast::Expression> Parser::expect_prefix(bool is_condition) {
    using enum Token::Type;

    if (match(LeftBracket)) {
        auto offset = peek().offset;
        auto type = parse_array_type();

        if (!type) {
            return nullptr;
        }

        std::vector<std::unique_ptr<ast::Expression>> elements;

        if (!expect("'{'", LeftBrace)) {
            return nullptr;
        }

        while (true) {
            if (auto value = expect_expr(false)) {
                elements.push_back(std::move(value));
            }

            if (match(RightBrace)) {
                next();

                return std::make_unique<ast::ArrayLiteral>(
                    offset, std::move(type), std::move(elements));
            }

            expect("','", Token::Type::Comma);
        }
    }

    auto token = expect(
        "expression", IntLiteral, FloatLiteral, StrLiteral, True, False, Null,
        Undefined, Identifier, Minus, Bang, Star, And, Not, LeftParen);

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
    case True:
        return std::make_unique<ast::BoolLiteral>(token->offset, true);
    case False:
        return std::make_unique<ast::BoolLiteral>(token->offset, false);
    case Null:
        return std::make_unique<ast::NullLiteral>(token->offset);
    case Undefined:
        return std::make_unique<ast::Undefined>(token->offset);
    case Identifier: {
        std::vector<ast::OffsetValue<std::string>> value;
        value.push_back(ast::OffsetValue{token->value, token->offset});

        while (match(ColonColon)) {
            next();
            auto name = expect("name", Identifier);

            if (!name) {
                return nullptr;
            }

            value.push_back(ast::OffsetValue{name->value, name->offset});
        }

        if (!is_condition && match(LeftBrace)) {
            next();

            auto fields = parse_field_values();

            if (!expect("',' or '}'", RightBrace)) {
                return nullptr;
            }

            return std::make_unique<ast::StructLiteral>(
                token->offset,
                std::make_unique<ast::NamedType>(
                    token->offset, std::move(value)),
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
                token->offset, ast::OffsetValue{token->type, token->offset},
                std::move(value));
        }

        return nullptr;
    case LeftParen: {
        auto value = expect_expr(false);

        if (match(RightParen)) {
            next();
            return value;
        }

        if (!expect("')' or ','", Comma)) {
            return nullptr;
        }

        std::vector<std::unique_ptr<ast::Expression>> elements;
        elements.push_back(std::move(value));

        while (true) {
            if (auto value = expect_expr(false)) {
                elements.push_back(std::move(value));
            }

            if (match(RightParen)) {
                next();

                return std::make_unique<ast::TupleLiteral>(
                    token->offset, std::move(elements));
            }

            expect("','", Token::Type::Comma);
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
        if (match(LeftParen)) {
            next();

            auto args = parse_args();

            if (!expect("')'", RightParen)) {
                return nullptr;
            }

            expression = std::make_unique<ast::CallExpr>(
                expression->offset, std::move(expression), std::move(args));

            continue;
        }

        if (match(LeftBracket)) {
            next();

            auto low = expect_expr(false);

            if (!low) {
                return nullptr;
            }

            if (match(RightBracket)) {
                next();

                expression = std::make_unique<ast::IndexExpr>(
                    expression->offset, std::move(expression), std::move(low));

                continue;
            }

            if (!expect("']' or ':'", Colon)) {
                return nullptr;
            }

            auto high = expect_expr(false);

            if (!high) {
                return nullptr;
            }

            if (!expect("']'", RightBracket)) {
                return nullptr;
            }

            expression = std::make_unique<ast::SliceExpr>(
                expression->offset, std::move(expression), std::move(low),
                std::move(high));

            continue;
        }

        if (!match(Dot)) {
            return expression;
        }

        next();

        auto member = expect("member name", Identifier, IntLiteral);

        if (!member) {
            return nullptr;
        }

        if (match(LeftParen)) {
            next();

            auto args = parse_args();

            if (!expect("')'", RightParen)) {
                return nullptr;
            }

            expression = std::make_unique<ast::MethodExpr>(
                expression->offset, std::move(expression),
                ast::OffsetValue{member->value, member->offset},
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

    if (!match(Token::Type::As)) {
        return expression;
    }

    next();

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
        lhs->offset, ast::OffsetValue{oper.type, oper.offset}, std::move(lhs),
        std::move(rhs));
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

std::unique_ptr<ast::Expression> Parser::expect_expr(bool is_condition) {
    return expect_bin_expr(is_condition);
}

std::unique_ptr<ast::BlockStmt> Parser::expect_block() {
    auto result = std::make_unique<ast::BlockStmt>(peek().offset);

    if (!expect("'{'", Token::Type::LeftBrace)) {
        return nullptr;
    }

    while (true) {
        if (match(Token::Type::Eof)) {
            expected(fmt::format("{}", log::bold("'}'")));
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

    if (!match(Token::Type::Else)) {
        return std::make_unique<ast::IfElse>(
            condition->offset, std::move(condition), std::move(if_block));
    }

    next();

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
    if (!expect("':'", Token::Type::Colon)) {
        return nullptr;
    }

    return expect_type();
}

std::unique_ptr<ast::Type> Parser::parse_array_type() {
    auto offset = get().offset;

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

    if (match(Token::Type::RightBracket)) {
        next();

        return std::make_unique<ast::SliceType>(
            offset, std::move(type), is_mutable);
    }

    if (!expect("',' or ']'", Token::Type::Comma)) {
        return nullptr;
    }

    auto size = expect_expr(false);

    if (!size) {
        return nullptr;
    }

    if (!expect("']'", Token::Type::RightBracket)) {
        return nullptr;
    }

    if (is_mutable) {
        error(is_mutable_offset, "array type cannot be mutable");
    }

    return std::make_unique<ast::ArrayType>(
        offset, std::move(type), std::move(size));
}

std::unique_ptr<ast::Type> Parser::expect_type() {
    using enum Token::Type;

    auto offset = peek().offset;

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
            offset, std::move(type), is_mutable);
    }

    if (match(QuestionMark)) {
        next();

        auto type = expect_type();

        if (!type) {
            return nullptr;
        }

        return std::make_unique<ast::Optional>(offset, std::move(type));
    }

    if (match(LeftParen)) {
        next();

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

        while (match(Comma)) {
            next();

            if (!parse_type()) {
                return nullptr;
            }
        }

        if (!expect("')'", RightParen)) {
            return nullptr;
        }

        return std::make_unique<ast::TupleType>(offset, std::move(types));
    }

    if (match(LeftBracket)) {
        return parse_array_type();
    }

    std::vector<ast::OffsetValue<std::string>> value;
    auto token = expect("name", Identifier);

    if (!token) {
        return nullptr;
    }

    value.push_back(ast::OffsetValue{token->value, token->offset});

    while (match(ColonColon)) {
        next();
        auto name = expect("name", Identifier);

        if (!name) {
            return nullptr;
        }

        value.push_back(ast::OffsetValue{name->value, name->offset});
    }

    return std::make_unique<ast::NamedType>(offset, std::move(value));
}

std::unique_ptr<ast::VarDecl> Parser::parse_var() {
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

    if (!match(Token::Type::Equal)) {
        if (!type) {
            expected(
                fmt::format("{} or {}", log::bold("'='"), log::bold("':'")));

            return nullptr;
        }

        return std::make_unique<ast::VarDecl>(
            offset, mutability, ast::OffsetValue{name->value, name->offset},
            std::move(type), nullptr);
    }

    next();

    auto value = expect_expr(false);

    if (!value) {
        return nullptr;
    }

    return std::make_unique<ast::VarDecl>(
        offset, mutability, ast::OffsetValue{name->value, name->offset},
        std::move(type), std::move(value));
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

void Parser::parse_return(ast::BlockStmt& block) {
    auto offset = get().offset;

    std::unique_ptr<ast::Expression> value = nullptr;

    if (!match(Token::Type::Semicolon)) {
        value = expect_expr(false);
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
        ast::OffsetValue{oper.type, oper.offset}));
}

std::vector<ast::Struct::Field> Parser::parse_fields() {
    std::vector<ast::Struct::Field> result;

    auto parse_field = [&] {
        auto name = get();

        if (auto type = expect_var_type()) {
            result.emplace_back(
                ast::OffsetValue{name.value, name.offset}, std::move(type));
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

std::vector<ast::EnumDecl::Field> Parser::parse_enum_fields() {
    std::vector<ast::EnumDecl::Field> result;

    auto parse_field = [&] {
        auto name = get();

        if (match(Token::Type::Equal)) {
            next();
            auto value = expect_expr(false);

            result.emplace_back(
                ast::OffsetValue{name.value, name.offset}, std::move(value));
        } else {
            result.emplace_back(ast::OffsetValue{name.value, name.offset});
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

std::unique_ptr<ast::FnDecl> Parser::parse_fn(bool is_public, bool is_extern) {
    auto name = expect("function name", Token::Type::Identifier);
    std::optional<ast::OffsetValue<std::string>> type = std::nullopt;

    if (!name) {
        return nullptr;
    }

    bool variadic = false;
    std::vector<ast::FnDecl::Param> params;

    bool had_default = false;

    auto parse_param = [&] {
        if (match(Token::Type::Ellipsis)) {
            next();
            variadic = true;

            return;
        }

        auto name = expect("parameter name", Token::Type::Identifier);

        if (!name) {
            return;
        }

        std::unique_ptr<ast::Type> type = expect_var_type();

        if (!type) {
            return;
        }

        std::unique_ptr<ast::Expression> value = nullptr;

        if (match(Token::Type::Equal)) {
            had_default = true;

            next();
            value = expect_expr(false);
        } else if (had_default) {
            error(name->offset, "default parameters must be at the end");
            return;
        }

        params.emplace_back(
            ast::OffsetValue{name->value, name->offset}, std::move(type),
            std::move(value));
    };

    auto parse_params = [&] {
        if (match(Token::Type::Identifier, Token::Type::Ellipsis)) {
            parse_param();

            while (!variadic && match(Token::Type::Comma)) {
                next();
                parse_param();
            }
        }
    };

    if (match(Token::Type::ColonColon)) {
        next();

        type = {name->value, name->offset};
        name = expect("function name", Token::Type::Identifier);

        if (!name) {
            return nullptr;
        }
    }

    if (!expect("'('", Token::Type::LeftParen)) {
        return nullptr;
    }

    parse_params();

    if (!expect("')'", Token::Type::RightParen)) {
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
    } else if (match(Token::Type::Semicolon)) {
        next();
    } else {
        expected(fmt::format("{} or {}", log::bold("'{'"), log::bold("';'")));

        return nullptr;
    }

    return std::make_unique<ast::FnDecl>(
        name->offset,
        ast::FnDecl::Proto{
            std::move(type),
            {name->value, name->offset},
            std::move(params),
            std::move(return_type),
            variadic},
        std::move(body), is_public, is_extern);
}

std::unique_ptr<ast::Struct> Parser::parse_struct(bool is_public) {
    auto name = expect("struct name", Token::Type::Identifier);

    if (!name) {
        return nullptr;
    }

    if (!expect("'{'", Token::Type::LeftBrace)) {
        return nullptr;
    }

    auto fields = parse_fields();

    if (!expect("'}'", Token::Type::RightBrace)) {
        return nullptr;
    }

    return std::make_unique<ast::Struct>(
        name->offset, ast::OffsetValue{name->value, name->offset},
        std::move(fields), is_public);
}

std::unique_ptr<ast::EnumDecl> Parser::parse_enum(bool is_public) {
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

    if (!expect("'{'", Token::Type::LeftBrace)) {
        return nullptr;
    }

    auto fields = parse_enum_fields();

    if (!expect("'}'", Token::Type::RightBrace)) {
        return nullptr;
    }

    return std::make_unique<ast::EnumDecl>(
        name->offset, ast::OffsetValue{name->value, name->offset},
        std::move(type), std::move(fields), is_public);
}

bool Parser::parse_submodule(
    ast::Module& module, const std::filesystem::path& path) {
    auto code = cent::read_file(path);

    if (!code) {
        return false;
    }

    Parser parser{*code, path.relative_path().string()};
    auto submodule = parser.parse();

    if (!submodule) {
        return false;
    }

    module.functions.reserve(
        module.functions.size() + submodule->functions.size());

    module.structs.reserve(module.structs.size() + submodule->structs.size());
    module.enums.reserve(module.enums.size() + submodule->enums.size());

    for (auto& function : submodule->functions) {
        module.functions.push_back(std::move(function));
    }

    for (auto& struct_decl : submodule->structs) {
        module.structs.push_back(std::move(struct_decl));
    }

    for (auto& enum_decl : submodule->enums) {
        module.enums.push_back(std::move(enum_decl));
    }

    for (auto& submodule : submodule->submodules) {
        module.submodules.insert(std::move(submodule));
    }

    return true;
}

bool Parser::parse_submodule_dir(
    ast::Module& module, const std::filesystem::path& path) {
    for (const auto& entry :
         std::filesystem::recursive_directory_iterator{path}) {
        auto name = entry.path().stem().string();

        auto& submodule = module.submodules[name];

        if (!submodule) {
            submodule = std::make_unique<ast::Module>(ModulePath{});
        }

        if (entry.is_directory()) {
            submodule->path.directory = entry;

            if (!parse_submodule_dir(*submodule, entry)) {
                return false;
            }

            continue;
        }

        if (entry.path().extension() != ".cn") {
            continue;
        }

        submodule->path.file = entry;

        if (!parse_submodule(*submodule, entry)) {
            return false;
        }
    }

    return true;
}

bool Parser::parse_with(ast::Module& module) {
    auto name = expect("module name", Token::Type::Identifier);

    if (!name) {
        return false;
    }

    std::vector<std::string> path;
    path.push_back(name->value);

    while (match(Token::Type::ColonColon)) {
        next();

        name = expect("module name", Token::Type::Identifier);

        if (!name) {
            return false;
        }

        path.push_back(name->value);
    }

    auto module_name = path.back();

    if (match(Token::Type::As)) {
        next();

        auto token = expect("module name", Token::Type::Identifier);

        if (!token) {
            return false;
        }

        module_name = token->value;
    }

    std::array<std::filesystem::path, 1> search_paths = {
        std::filesystem::path{m_filename}.parent_path()};

    auto module_path = cent::find_module(path, search_paths);

    if (!module_path.directory && !module_path.file) {
        error(
            name->offset, fmt::format(
                              "could not find module {}",
                              log::bold(log::quoted(name->value))));

        return false;
    }

    if (!module.submodules[module_name]) {
        module.submodules[module_name] =
            std::make_unique<ast::Module>(module_path);
    }

    auto& submodule = module.submodules[module_name];

    if (module_path.file) {
        if (!parse_submodule(*submodule, *module_path.file)) {
            return false;
        }
    }

    if (!module_path.directory) {
        return true;
    }

    return parse_submodule_dir(*submodule, *module_path.directory);
}

} // namespace cent::frontend
