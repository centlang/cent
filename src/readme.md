# `centc` structure

## Frontend

In the `Lexer`, `.token()` returns the current token, and `.next_token()`
advances the lexer.

In the `Parser`, `.parse()` builds an AST. The parser has a small lookahead
table. It primarily uses recursive-descent, except for binary expressions, for
which it uses Pratt parsing.

## Abstract syntax tree

The base type in the AST is `Node`. `Module`, `Type`, and `Statement` inherit
from it directly, while `Expression` and `Declaration` inherit from `Statement`.

## Backend

The `Codegen` class has a `.generate()` method to compile the program to an
`llvm::Module`. There is a `.generate(Node&)` for every AST `Node`.

### `Value`

The `Value` structure represents a Cent value. It contains type information,
`llvm::Value*`, indirection depth, mutability, and whether the variable was
stack allocated to avoid creating an extra `alloca` and `store`-ing the value if
it's already an `alloca`. A value can be _poisoned_.

### `Type`

All types inherit from the base `Type`. `.to_string()` can be used to get the
string representation of a `Type`. It uses LLVM-style RTTI (`isa`, `dyn_cast`).

### `Scope`

A `Scope` maps names to values with additional information such as visibility
and the translation unit of each element.

### `TranslationUnit`

A `TranslationUnit` is just a 16-bit index into the list of all units in the
program. Each directory is considered a separate translation unit.
