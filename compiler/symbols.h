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
class SemaContext;
struct token_pos_t;

struct ReturnArrayInfo : public PoolObject {
    cell_t iv_size;
    cell_t dat_addr;
    cell_t zeroes;
};

class FunctionData final : public SymbolData
{
  public:
    FunctionData();
    FunctionData* asFunction() override {
        return this;
    }

    tr::vector<tr::string>* dbgstrs = nullptr;
    ReturnArrayInfo* return_array = nullptr;
    symbol* array_return = nullptr;
    Label label;     // modern replacement for addr
    Label funcid;
    int max_local_stack = 0;
    int max_callee_stack = 0;
    bool checked_one_signature SP_BITFIELD(1);
    bool compared_prototype_args SP_BITFIELD(1);
    bool is_member_function SP_BITFIELD(1);

    // Other symbols that this symbol refers to.
    PoolForwardList<FunctionDecl*> refers_to;
};

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
    symbol(const symbol& other);
    symbol(Decl* decl, Atom* name, cell addr, IdentifierKind ident, int vclass, int tag);

    cell codeaddr; /* address (in the code segment) where the symbol declaration starts */
    char vclass;   /* sLOCAL if "addr" refers to a local symbol */
    int tag;       /* tagname id */

    IdentifierKind ident : 6;    /* see below for possible values */

    // See uREAD/uWRITTEN above.
    uint8_t usage : 3;

    // Variable: the variable is defined in the source file.
    // Function: the function is defined ("implemented") in the source file
    // Constant: the symbol is defined in the source file.
    bool is_const : 1;

    // Functions only.
    bool returns_value : 1; // whether any path returns a value
    bool always_returns: 1; // whether all paths have an explicit return statement

    // Constants only.
    bool enumroot : 1;      // the constant is the "root" of an enumeration

    int semantic_tag;
    int* dim_data;     /* -1 = dim count, 0..n = dim sizes */
    Decl* decl;

    int dim_count() const { return dim_data ? dim_data[-1] : 0; }
    void set_dim_count(int dim_count);
    int dim(int n) const {
        assert(n < dim_count());
        return dim_data[n];
    }
    void set_dim(int n, int size) {
        assert(n < dim_count());
        dim_data[n] = size;
    }

    int addr() const {
        return addr_;
    }
    void setAddr(int addr) {
        addr_ = addr;
    }
    Atom* nameAtom() const {
        return name_;
    }
    const char* name() const {
        return name_ ? name_->chars() : "";
    }
    void setName(Atom* name) {
        name_ = name;
    }
    FunctionData* function() const {
        assert(ident == iFUNCTN);
        return data_->asFunction();
    }

    symbol* array_return() const {
        return function()->array_return;
    }
    void set_array_return(symbol* child) {
        assert(!function()->array_return);
        function()->array_return = child;
    }
    SymbolData* data() const {
        return data_;
    }
    void set_data(SymbolData* data) {
        data_ = std::move(data);
    }

    void add_reference_to(FunctionDecl* other);

    bool used() const {
        assert(ident == iFUNCTN);
        return (usage & uLIVE) == uLIVE;
    }
    bool unused() const {
        return !used();
    }

  private:
    cell addr_; /* address or offset (or value for constant, index for native function) */
    Atom* name_;
    SymbolData* data_;
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
    symbol* sym;
    int tag;

    // Returns whether the value can be rematerialized based on static
    // information, or whether it is the result of an expression.
    bool canRematerialize() const {
        switch (ident) {
            case iVARIABLE:
            case iCONSTEXPR:
                return true;
            case iREFERENCE:
                return sym->vclass == sARGUMENT || sym->vclass == sLOCAL;
            default:
                return false;
        }
    }

    methodmap_method_t* accessor() const {
        if (ident != iACCESSOR)
            return nullptr;
        return accessor_;
    }
    void set_accessor(methodmap_method_t* accessor) {
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
    void set_array(IdentifierKind ident, symbol* sym, int level) {
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
    int array_size() const {
        assert(ident == iARRAY || ident == iREFARRAY);
        if (sym)
            return sym->dim(array_level_);
        return array_level_;
    }
    int array_level() const {
        assert(ident == iARRAY || ident == iREFARRAY);
        if (sym)
            return array_level_;
        return 0;
    }
    int array_dim_count() const {
        if (ident == iARRAYCHAR || ident == iARRAYCELL)
            return 1;
        assert(ident == iARRAY || ident == iREFARRAY);
        if (sym)
            return sym->dim_count() - array_level_;
        return 1;
    }
    int array_dim(int n) const {
        if (ident == iARRAYCHAR || ident == iARRAYCELL)
            return 0;

        assert(ident == iARRAY || ident == iREFARRAY);

        if (sym)
            return sym->dim(array_level_ + n);

        assert(n == 0);
        return array_size();
    }

    union {
        // when ident == iACCESSOR
        methodmap_method_t* accessor_;
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

void AddGlobal(CompileContext& cc, symbol* sym);

void DefineSymbol(SemaContext& sc, Decl* decl, int vclass);
symbol* DefineConstant(SemaContext& sc, Decl* decl, Atom* name, const token_pos_t& pos, cell val,
                       int vclass, int tag);
bool CheckNameRedefinition(SemaContext& sc, Atom* name, const token_pos_t& pos, int vclass);

void markusage(symbol* sym, int usage);
symbol* NewVariable(Decl* decl, Atom* name, cell addr, IdentifierKind ident, int vclass, int tag,
                    int dim[], int numdim, int semantic_tag);
symbol* FindEnumStructField(Type* type, Atom* name);
void deduce_liveness(CompileContext& cc);
Decl* declare_methodmap_symbol(CompileContext& cc, Decl* decl, methodmap_t* map);

} // namespace sp
