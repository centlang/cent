#ifndef CENT_BACKEND_CODEGEN_H
#define CENT_BACKEND_CODEGEN_H

#include <memory>
#include <utility>

#include <llvm/IR/Module.h>

namespace cent {

struct Program;

struct Assignment;
struct BlockStmt;
struct IfElse;
struct ReturnStmt;
struct WhileLoop;

struct BinaryExpr;
struct UnaryExpr;
struct IntLiteral;
struct FloatLiteral;
struct BoolLiteral;
struct Identifier;
struct CallExpr;

struct FnDecl;
struct VarDecl;

class Codegen {
public:
    [[nodiscard]] Codegen(std::unique_ptr<Program> program) noexcept
    : m_program{std::move(program)} {}

    [[nodiscard]] std::unique_ptr<llvm::Module> generate() noexcept;

    llvm::Value* generate(Assignment& stmt) noexcept;
    llvm::Value* generate(BlockStmt& stmt) noexcept;
    llvm::Value* generate(IfElse& stmt) noexcept;
    llvm::Value* generate(ReturnStmt& stmt) noexcept;
    llvm::Value* generate(WhileLoop& stmt) noexcept;

    llvm::Value* generate(BinaryExpr& expr) noexcept;
    llvm::Value* generate(UnaryExpr& expr) noexcept;
    llvm::Value* generate(IntLiteral& expr) noexcept;
    llvm::Value* generate(FloatLiteral& expr) noexcept;
    llvm::Value* generate(BoolLiteral& expr) noexcept;
    llvm::Value* generate(Identifier& expr) noexcept;
    llvm::Value* generate(CallExpr& expr) noexcept;

    llvm::Value* generate(FnDecl& decl) noexcept;
    llvm::Value* generate(VarDecl& decl) noexcept;

private:
    std::unique_ptr<Program> m_program;
};

} // namespace cent

#endif
