/* vim: set ts=8 sts=2 sw=2 tw=99 et: */
/*  Pawn compiler - Recursive descend expresion parser
 *
 *  Copyright (c) ITB CompuPhase, 1997-2005
 *
 *  This software is provided "as-is", without any express or implied warranty.
 *  In no event will the authors be held liable for any damages arising from
 *  the use of this software.
 *
 *  Permission is granted to anyone to use this software for any purpose,
 *  including commercial applications, and to alter it and redistribute it
 *  freely, subject to the following restrictions:
 *
 *  1.  The origin of this software must not be misrepresented; you must not
 *      claim that you wrote the original software. If you use this software in
 *      a product, an acknowledgment in the product documentation would be
 *      appreciated but is not reeq;quired.
 *  2.  Altered source versions must be plainly marked as such, and must not be
 *      misrepresented as being the original software.
 *  3.  This notice may not be removed or altered from any source distribution.
 *
 *  Version: $Id$
 */
#ifndef am_sourcepawn_compiler_sc3_h
#define am_sourcepawn_compiler_sc3_h

#include "amx.h"
#include "expression-parsing.h"

struct value;
struct svalue;

class SC3ExpressionParser : public BaseExpressionParser
{
public:
  int evaluate(value* lval) {
    return hier14(lval);
  }

private:
  typedef int (SC3ExpressionParser::*HierFn)(value*);

  int hier14(value* lval);
  int hier13(value *lval);
  int hier12(value *lval);
  int hier11(value *lval);
  int hier10(value *lval);
  int hier9(value *lval);
  int hier8(value *lval);
  int hier7(value *lval);
  int hier6(value *lval);
  int hier5(value *lval);
  int hier4(value *lval);
  int hier3(value *lval);
  int hier2(value *lval);
  int hier1(value *lval1);
  int plnge(int *opstr,int opoff,HierFn hier,value *lval,
            const char *forcetag,int chkbitwise);
  int plnge1(HierFn hier,value *lval);
  void plnge2(void (*oper)(void),
              HierFn hier, value *lval1,value *lval2);
  int plnge_rel(int *opstr,int opoff,HierFn hier,value *lval);
  void callfunction(symbol *sym, const svalue *implicitthis, value *lval_result, int matchparanthesis);
  int primary(value *lval);
  int skim(int *opstr,void (*testfunc)(int),int dropval,int endval,
           HierFn hier, value *lval);
  int parse_view_as(value* lval);
};

#endif // am_sourcepawn_compiler_sc3_h
