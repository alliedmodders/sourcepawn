// vim: set ts=8 sts=4 sw=4 tw=99 et:
//  Pawn compiler - Recursive descend expresion parser
//
//  Copyright (c) ITB CompuPhase, 1997-2005
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
//
//  Version: $Id$

#include "amx.h"
#include "expressions.h"
#include "parse-node.h"
#include "sc.h"

class Parser : public ExpressionParser
{
  public:
    int expression(value* lval);

  private:
    typedef int (Parser::*HierFn)(value*);
    typedef Expr* (Parser::*NewHierFn)();

    Expr* hier14();
    Expr* parse_view_as();
    Expr* plnge(int* opstr, NewHierFn hier);
    Expr* plnge_rel(int* opstr, NewHierFn hier);
    Expr* hier13();
    Expr* hier12();
    Expr* hier11();
    Expr* hier10();
    Expr* hier9();
    Expr* hier8();
    Expr* hier7();
    Expr* hier6();
    Expr* hier5();
    Expr* hier4();
    Expr* hier3();
    Expr* hier2();
    Expr* hier1();
    Expr* primary();
    Expr* constant();
    CallExpr* parse_call(const token_pos_t& pos, int tok, Expr* target);
};
