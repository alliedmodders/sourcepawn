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
#ifndef am_sourcepawn_compiler_expression_parser_h
#define am_sourcepawn_compiler_expression_parser_h

#include "amx.h"

struct arginfo;
struct symbol;

class BaseExpressionParser
{
public:
  BaseExpressionParser();

protected:
  static int nextop(int *opidx,int *list);
  static int findnamedarg(arginfo *arg,char *name);

  cell array_levelsize(symbol *sym,int level);
  cell array_totalsize(symbol *sym);
  cell parse_defined();
  cell parse_sizeof();
  cell parse_cellsof();
  cell parse_tagof();

  // Each of these lists is an operator precedence level, and each list is a
  // zero-terminated list of operators in that level (in precedence order).
  static int list3[];
  static int list4[];
  static int list5[];
  static int list6[];
  static int list7[];
  static int list8[];
  static int list9[];
  static int list10[];
  static int list11[];
  static int list12[];

protected:
  // Count of bitwise operators in an expression.
  int bitwise_opercount_;
};

#endif // am_sourcepawn_compiler_expression_parser_h
