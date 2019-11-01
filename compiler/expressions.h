/* vim: set ts=8 sts=4 sw=4 tw=99 et: */
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
 *      appreciated but is not required.
 *  2.  Altered source versions must be plainly marked as such, and must not be
 *      misrepresented as being the original software.
 *  3.  This notice may not be removed or altered from any source distribution.
 *
 *  Version: $Id$
 */
#ifndef am_sourcepawn_compiler_sc3_h
#define am_sourcepawn_compiler_sc3_h

#include "amx.h"
#include "parse-node.h"
#include "sc.h"

struct value;
struct svalue;

class ExpressionParser
{
  protected:
    static int nextop(int* opidx, int* list);

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
};

class SC3ExpressionParser : public ExpressionParser
{
  public:
    int evaluate(value* lval) {
        return hier14(lval);
    }

  private:
    typedef int (SC3ExpressionParser::*HierFn)(value*);

    int hier14(value* lval);
    int hier13(value* lval);
    int hier12(value* lval);
    int hier11(value* lval);
    int hier10(value* lval);
    int hier9(value* lval);
    int hier8(value* lval);
    int hier7(value* lval);
    int hier6(value* lval);
    int hier5(value* lval);
    int hier4(value* lval);
    int hier3(value* lval);
    int hier2(value* lval);
    int hier1(value* lval1);
    int plnge(int* opstr, int opoff, HierFn hier, value* lval, const char* forcetag,
              int chkbitwise);
    int plnge1(HierFn hier, value* lval);
    void plnge2(void (*oper)(void), HierFn hier, value* lval1, value* lval2);
    int plnge_rel(int* opstr, int opoff, HierFn hier, value* lval);
    void callfunction(symbol* sym, const svalue* implicitthis, value* lval_result,
                      int matchparanthesis);
    int primary(value* lval);
    int skim(int* opstr, void (*testfunc)(int), int dropval, int endval, HierFn hier, value* lval);
    int parse_view_as(value* lval);
    cell parse_defined();
    cell parse_sizeof();
    cell sizeof_impl();

  private:
    // Count of bitwise operators in an expression.
    int bitwise_opercount_ = 0;
};

#define MATCHTAG_COERCE 0x1      // allow coercion
#define MATCHTAG_SILENT 0x2      // silence the error(213) warning
#define MATCHTAG_COMMUTATIVE 0x4 // order does not matter
#define MATCHTAG_DEDUCE 0x8      // correct coercion

bool find_userop(void (*oper)(), int tag1, int tag2, int numparam, const value* lval, UserOperation* op);
void emit_userop(const UserOperation& user_op, value* lval);

int findnamedarg(arginfo* arg, const char* name);
cell array_totalsize(symbol* sym);
cell array_levelsize(symbol* sym, int level);
int commutative(void (*oper)());
cell calc(cell left, void (*oper)(), cell right, char* boolresult);
bool is_valid_index_tag(int tag);
int check_userop(void (*oper)(void), int tag1, int tag2, int numparam, value* lval, int* resulttag);
int matchtag(int formaltag, int actualtag, int allowcoerce);
int expression(cell* val, int* tag, symbol** symptr, int chkfuncresult, value* _lval);
int matchtag_string(int ident, int tag);
int checkval_string(const value* sym1, const value* sym2);
int checktag_string(int tag, const value* sym1);
int lvalexpr(svalue* sval);
void user_inc();
void user_dec();
int checktag(int tag, int exprtag);
void setdefarray(cell* string, cell size, cell array_sz, cell* dataaddr, int fconst);

#endif // am_sourcepawn_compiler_sc3_h
