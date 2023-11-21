// vim: set ts=8 sts=4 sw=4 tw=99 et:
//  Pawn compiler - Recursive descend expresion parser
//
//  Copyright (c) ITB CompuPhase, 1997-2005
//  Copyright (c) AlliedModders LLC 2021
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

#pragma once

#include <functional>
#include <unordered_map>

#include "label.h"
#include "lexer.h"
#include "sc.h"
#include "source-location.h"
#include "stl/stl-unordered-map.h"

namespace sp {

class CompileContext;
class Decl;
class FunctionDecl;
class MethodmapPropertyDecl;
class SemaContext;
struct token_pos_t;

/*  Symbol table format
 *
 *  The symbol name read from the input file is stored in "name", the
 *  value of "addr" is written to the output file. The address in "addr"
 *  depends on the class of the symbol:
 *      global          offset into the data segment
 *      local           offset relative to the stack frame
 *      label           generated hexadecimal number
 *      function        offset into code segment
 */
struct symbol : public PoolObject
{
    symbol(IdentifierKind ident, int vclass, int tag);

    char vclass() const { return vclass_; }
    char vclass_;   /* sLOCAL if "addr" refers to a local symbol */
    int tag() const { return tag_; }
    void set_tag(int tag) { tag_ = tag; }
    int tag_;       /* tagname id */

    IdentifierKind ident() const { return ident_; }
    void set_ident(IdentifierKind ident) { ident_ = ident; }
    IdentifierKind ident_ : 6;    /* see below for possible values */

    // Variable: the variable is defined in the source file.
    // Function: the function is defined ("implemented") in the source file
    // Constant: the symbol is defined in the source file.
    bool is_const() const { return is_const_; }
    bool is_const_ : 1;
    void set_is_const(bool is_const) { is_const_ = is_const; }

    int semantic_tag() const { return semantic_tag_; }
    void set_semantic_tag(int semantic_tag) { semantic_tag_ = semantic_tag; }
    int semantic_tag_;

    int* dim_data_;     /* -1 = dim count, 0..n = dim sizes */

    int dim_count() const { return dim_data_ ? dim_data_[-1] : 0; }
    void set_dim_count(int dim_count);
    int dim(int n) const {
        assert(n < dim_count());
        return dim_data_[n];
    }
    void set_dim(int n, int size) {
        assert(n < dim_count());
        dim_data_[n] = size;
    }
};

enum ScopeKind {
    sGLOBAL = 0,      /* global variable/constant class (no states) */
    sLOCAL = 1,       /* local variable/constant */
    sSTATIC = 2,      /* global lifetime, local or global scope */
    sARGUMENT = 3,    /* function argument (this is never stored anywhere) */
    sENUMFIELD = 4,   /* for analysis purposes only (not stored anywhere) */
    sFILE_STATIC = 5, /* only appears on SymbolScope, to clarify sSTATIC */
};

struct value {
    value() : ident(iINVALID), sym(nullptr), tag(0) {}

    IdentifierKind ident : 6;
    Decl* sym;
    int tag;

    // Returns whether the value can be rematerialized based on static
    // information, or whether it is the result of an expression.
    bool canRematerialize() const;

    MethodmapPropertyDecl* accessor() const {
        if (ident != iACCESSOR)
            return nullptr;
        return accessor_;
    }
    void set_accessor(MethodmapPropertyDecl* accessor) {
        ident = iACCESSOR;
        accessor_ = accessor;
    }
    cell constval() const {
        assert(ident == iCONSTEXPR);
        return constval_;
    }
    void set_constval(cell val) {
        ident = iCONSTEXPR;
        constval_ = val;
    }
    void set_array(IdentifierKind ident, Decl* sym, int level) {
        assert(ident == iARRAY || ident == iREFARRAY || ident == iARRAYCELL ||
               ident == iARRAYCHAR);
        this->ident = ident;
        this->sym = sym;
        this->array_level_ = level;
    }
    void set_array(IdentifierKind ident, int size) {
        assert(ident == iARRAY || ident == iREFARRAY);
        this->ident = ident;
        this->sym = nullptr;
        this->array_level_ = size;
    }

    int array_size() const;
    int array_level() const;
    int array_dim_count() const;
    int array_dim(int n) const;

    union {
        // when ident == iACCESSOR
        MethodmapPropertyDecl* accessor_;
        // when ident == iCONSTEXPR
        cell constval_;
        // when ident == iARRAY
        int array_level_;
    };

    static value ErrorValue() {
        value v = {};
        v.ident = iCONSTEXPR;
        return v;
    }
};

void DefineSymbol(SemaContext& sc, Decl* decl, int vclass);
symbol* DefineConstant(SemaContext& sc, Decl* decl, const token_pos_t& pos,
                       int vclass, int tag);
bool CheckNameRedefinition(SemaContext& sc, Atom* name, const token_pos_t& pos, int vclass);

void markusage(Decl* decl, int usage);
symbol* NewVariable(Decl* decl, IdentifierKind ident, int vclass, int tag,
                    int dim[], int numdim, int semantic_tag);
Decl* FindEnumStructField(Type* type, Atom* name);

} // namespace sp
