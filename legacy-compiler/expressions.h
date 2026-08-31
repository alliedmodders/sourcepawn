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
#pragma once

#include "lexer.h"
#include "parse-node.h"
#include "sc.h"

namespace sp {
namespace cc {

class SemaContext;
struct value;

int NextExprOp(Lexer* lexer, int* opidx, int* list);

#define MATCHTAG_COERCE 0x1      // allow coercion
#define MATCHTAG_SILENT 0x2      // silence the error(213) warning
#define MATCHTAG_DEDUCE 0x4      // correct coercion
#define MATCHTAG_FUNCARG 0x8     // argument in a function signature
#define MATCHTAG_ENUM_ASSN 0x10  // enum assignment

struct UserOperation;
bool find_userop(SemaContext& sc, int oper, Type* type1, Type* type2, int numparam,
                 const value* lval, UserOperation* op);
bool find_userop(SemaContext& sc, int oper, int tag1, int tag2, int numparam,
                 const value* lval, UserOperation* op);

int commutative(int oper);
cell calc(cell left, int oper_tok, cell right, char* boolresult);
bool IsValidIndexType(Type* type);
bool matchtag(int formaltag, int actualtag, int flags);
bool matchtag(Type* formaltag, Type* actualtag, int flags);
bool matchtag_commutative(Type* formal, Type* actual, int flags);
bool matchtag_commutative(int formaltag, int actualtag, int flags);
bool matchtag_string(int ident, int tag);
bool matchtag_string(int ident, Type* type);
bool checkval_string(const value* sym1, const value* sym2);
bool checktag_string(Type* type, const value* sym1);
bool checktag(Type* type, Type* expr_type);
bool checktag_string(int tag, const value* sym1);
bool checktag(int tag, int exprtag);
bool HasTagOnInheritanceChain(Type* type, Type* other);
bool functag_compare(FunctionType* formal, FunctionType* actual);

} // namespace cc
} // namespace sp
