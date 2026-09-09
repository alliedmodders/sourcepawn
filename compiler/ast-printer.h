// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
//
#pragma once
#include <stdio.h>
#include <vector>
#include "ast-types.h"

namespace sp {
namespace cc {

struct ConstVal;

class ParseTree;
class Stmt;
class Expr;
struct typeinfo_t;

namespace ir {
class Value;
} // namespace ir

#define _(Name) class Name;
AST_STMT_TYPE_LIST(_)
#undef _

#define _(Name) class Name;
AST_EXPR_TYPE_LIST(_)
#undef _

class StructInitFieldExpr;

class AstPrinter
{
  public:
    explicit AstPrinter(FILE* out);

    void Print(ParseTree* tree);
    void Print(Stmt* stmt, bool is_last = true);
    void Print(Expr* expr, bool is_last = true);

  private:
    void PrintIndent(bool is_last);
    void PrintType(const typeinfo_t& type);
    void PrintExprInline(Expr* expr);
    void PrintIr(ir::Value* expr, bool is_last);
    void PrintEscapedString(const char* s);
    void PrintConstVal(const ConstVal& cv);

#define _(Name) void Print##Name(Name* node, bool is_last);
    AST_STMT_TYPE_LIST(_)
    AST_EXPR_TYPE_LIST(_)
#undef _

    void PrintFunctionBody(FunctionDecl* node, bool is_last);

  private:
    FILE* out_;
    std::vector<bool> stack_;
};
} // namespace cc
} // namespace sp
