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

#include "lexer.h"
#include "parse-node.h"
#include "sc.h"

class SemaContext;
struct value;

int NextExprOp(Lexer* lexer, int* opidx, int* list);

#define MATCHTAG_COERCE 0x1      // allow coercion
#define MATCHTAG_SILENT 0x2      // silence the error(213) warning
#define MATCHTAG_DEDUCE 0x4      // correct coercion
#define MATCHTAG_FUNCARG 0x8     // argument in a function signature
#define MATCHTAG_ENUM_ASSN 0x10  // enum assignment

struct UserOperation;
bool find_userop(SemaContext& sc, int oper, int tag1, int tag2, int numparam,
                 const value* lval, UserOperation* op);
void emit_userop(const UserOperation& user_op, value* lval);

int commutative(int oper);
cell calc(cell left, int oper_tok, cell right, char* boolresult);
bool is_valid_index_tag(int tag);
int matchtag(int formaltag, int actualtag, int flags);
int matchtag_commutative(int formaltag, int actualtag, int flags);
int matchtag_string(int ident, int tag);
int checkval_string(const value* sym1, const value* sym2);
int checktag_string(int tag, const value* sym1);
void user_inc();
void user_dec();
int checktag(int tag, int exprtag);

#endif // am_sourcepawn_compiler_sc3_h
