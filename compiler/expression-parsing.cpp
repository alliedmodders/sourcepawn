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
#include "expression-parsing.h"
#include "sc.h"
#include <assert.h>
#include <string.h>

// The "op1" array in sc3.cpp must have the same ordering as if these lists
// were flattened.
int ExpressionParser::list3[]  = {'*','/','%',0};
int ExpressionParser::list4[]  = {'+','-',0};
int ExpressionParser::list5[]  = {tSHL,tSHR,tSHRU,0};
int ExpressionParser::list6[]  = {'&',0};
int ExpressionParser::list7[]  = {'^',0};
int ExpressionParser::list8[]  = {'|',0};
int ExpressionParser::list9[]  = {tlLE,tlGE,'<','>',0};
int ExpressionParser::list10[] = {tlEQ,tlNE,0};
int ExpressionParser::list11[] = {tlAND,0};
int ExpressionParser::list12[] = {tlOR,0};

ExpressionParser::ExpressionParser()
 : bitwise_opercount_(0)
{
}

/*
 *  Searches for a binary operator a list of operators. The list is stored in
 *  the array "list". The last entry in the list should be set to 0.
 *
 *  The index of an operator in "list" (if found) is returned in "opidx". If
 *  no operator is found, nextop() returns 0.
 *
 *  If an operator is found in the expression, it cannot be used in a function
 *  call with omitted parantheses. Mark this...
 */
int
ExpressionParser::nextop(int *opidx,int *list)
{
  *opidx=0;
  while (*list){
    if (matchtoken(*list)){
      return TRUE;      /* found! */
    } else {
      list+=1;
      *opidx+=1;
    } /* if */
  } /* while */
  return FALSE;         /* entire list scanned, nothing found */
}

int
ExpressionParser::findnamedarg(arginfo *arg,char *name)
{
  int i;

  for (i=0; arg[i].ident!=0 && arg[i].ident!=iVARARGS; i++)
    if (strcmp(arg[i].name,name)==0)
      return i;
  return -1;
}

cell
ExpressionParser::array_totalsize(symbol *sym)
{
  cell length;

  assert(sym!=NULL);
  assert(sym->ident==iARRAY || sym->ident==iREFARRAY);
  length=sym->dim.array.length;
  if (sym->dim.array.level > 0) {
    cell sublength=array_totalsize(finddepend(sym));
    if (sublength>0)
      length=length+length*sublength;
    else
      length=0;
  } /* if */
  return length;
}

cell
ExpressionParser::array_levelsize(symbol *sym,int level)
{
  assert(sym!=NULL);
  assert(sym->ident==iARRAY || sym->ident==iREFARRAY);
  assert(level <= sym->dim.array.level);
  while (level-- > 0) {
    sym=finddepend(sym);
    assert(sym!=NULL);
  } /* if */
  return (sym->dim.array.slength ? sym->dim.array.slength : sym->dim.array.length);
}
