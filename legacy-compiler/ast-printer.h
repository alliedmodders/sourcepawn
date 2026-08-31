// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
//  Copyright (c) AlliedModders 2026
//
//  This software is provided "as-is", without any express or implied warranty.
//  In no event will the authors be held liable for any damages arising from
//  the use of this software.
//
//  Permission is granted to anyone to use this software for any purpose,
//  including commercial applications, and to alter it and redistribute it
//  freely, subject to the following restrictions:
//
//  1.  The origin of this software must not be misrepresented; you must not
//      claim that you wrote the original software. If you use this software in
//      a product, an acknowledgment in the product documentation would be
//      appreciated but is not required.
//  2.  Altered source versions must be plainly marked as such, and must not be
//      misrepresented as being the original software.
//  3.  This notice may not be removed or altered from any source distribution.
#pragma once
#include <stdio.h>
#include <vector>
#include "ast-types.h"

namespace sp {
namespace cc {

class ParseTree;
class Stmt;
class Expr;
struct typeinfo_t;

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

#define _(Name) void Print##Name(Name* node, bool is_last);
    AST_STMT_TYPE_LIST(_)
    AST_EXPR_TYPE_LIST(_)
#undef _

  private:
    FILE* out_;
    std::vector<bool> stack_;
};
} // namespace cc
} // namespace sp
