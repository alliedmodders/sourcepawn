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

#include "sc.h"
#include "types.h"

namespace sp {
namespace cc {

class Decl;
class FunctionDecl;
class LayoutFieldDecl;
class PropertyDecl;
class UpvarDecl;
class VarDeclBase;

struct ExprVal {
    ExprVal() : ident(iINVALID), type_(nullptr) {}
    explicit ExprVal(VarDeclBase* decl);

    IdentifierKind ident : 6;
    QualType type_;

    Type* type() const { return *type_; }
    QualType qualified() const { return type_; }
    void set_type(Type* type) { type_ = QualType(type); }
    void set_type(QualType type) { type_ = type; }

    VarDeclBase* sym() const {
        if (ident != iVARIABLE)
            return nullptr;
        return sym_;
    }

    // Returns whether the value can be rematerialized based on static
    // information, or whether it is the result of an expression.
    bool canRematerialize() const {
        switch (ident) {
            case iVARIABLE:
            case iCONSTEXPR:
            case iUPVAR:
                return true;
            default:
                return false;
        }
    }

    void set_variable(VarDeclBase* decl, QualType type) {
        this->ident = iVARIABLE;
        this->sym_ = decl;
        set_type(type);
    }
    void set_expr(QualType type) {
        this->ident = iEXPRESSION;
        set_type(type);
    }
    void set_typename(Decl* decl) {
        this->ident = iTYPENAME;
        this->decl_ = decl;
    }
    void set_field(LayoutFieldDecl* field, QualType type) {
        this->ident = iFIELD;
        this->field_ = field;
        set_type(type);
    }

    PropertyDecl* accessor() const {
        if (ident != iACCESSOR)
            return nullptr;
        return accessor_;
    }
    void set_accessor(PropertyDecl* accessor) {
        ident = iACCESSOR;
        accessor_ = accessor;
    }
    cell const_i32() const {
        assert(ident == iCONSTEXPR &&
               !type()->isFloat() &&
               !type()->isWideType() &&
               !type()->isHeapItem());
        return const_i32_;
    }
    void set_constval(cell val) {
        ident = iCONSTEXPR;
        const_i32_ = val;
    }
    void set_constval(QualType type, cell val) {
        set_type(type);
        set_constval(val);
    }
    cell const_intptr() const {
        assert(ident == iCONSTEXPR && type()->isIntPtr());
        return const_i32_;
    }
    cell const_cell() const {
        assert(ident == iCONSTEXPR && !type()->isWideType() && !type()->isHeapItem());
        return const_i32_;
    }
    float const_float() const {
        assert(ident == iCONSTEXPR && type()->isFloat());
        return const_float_;
    }
    void set_const_float(float val) {
        ident = iCONSTEXPR;
        const_float_ = val;
    }
    void set_const_float(QualType type, float val) {
        set_type(type);
        set_const_float(val);
    }
    void set_const_intptr(QualType type, cell val) {
        assert(type->isIntPtr());
        set_type(type);
        const_i32_ = val;
        ident = iCONSTEXPR;
    }
    int64_t const_int64() const {
        assert(ident == iCONSTEXPR && type()->isInt64());
        return const_int64_;
    }
    void set_const_int64(QualType type, int64_t val) {
        assert(type->isInt64());
        set_type(type);
        const_int64_ = val;
        ident = iCONSTEXPR;
    }
    void set_slice(IdentifierKind ident, QualType type) {
        assert(ident == iARRAYELEM);
        this->ident = ident;
        set_type(type);
    }
    void set_function(FunctionDecl* fun) {
        this->ident = iFUNCTN;
        this->fun_ = fun;
    }
    double const_double() const {
        assert(ident == iCONSTEXPR && type()->isDouble());
        return const_double_;
    }
    void set_const_double(QualType type, double val) {
        assert(type->isDouble());
        set_type(type);
        const_double_ = val;
        ident = iCONSTEXPR;
    }
    Decl* typename_decl() const {
        assert(ident == iTYPENAME);
        return decl_;
    }
    FunctionDecl* fun() const {
        assert(ident == iFUNCTN);
        return fun_;
    }
    LayoutFieldDecl* field() const {
        assert(ident == iFIELD);
        return field_;
    }
    UpvarDecl* upvar() const {
        assert(ident == iUPVAR);
        return upvar_;
    }
    void set_upvar(UpvarDecl* upvar, QualType type) {
        this->ident = iUPVAR;
        this->upvar_ = upvar;
        set_type(type);
    }

    union {
        // when ident == iACCESSOR
        PropertyDecl* accessor_;
        // when ident == iCONSTEXPR
        float const_float_;
        cell const_i32_;
        double const_double_;
        int64_t const_int64_;
        // when ident == iVARIABLE
        VarDeclBase* sym_;
        // when ident == iFUNCTN
        FunctionDecl* fun_;
        // when ident == iTYPENAME
        Decl* decl_;
        // when ident == iFIELD
        LayoutFieldDecl* field_;
        // when ident == iUPVAR
        UpvarDecl* upvar_;
    };

    static ExprVal ErrorValue() {
        ExprVal v = {};
        v.ident = iCONSTEXPR;
        return v;
    }
};

} // namespace cc
} // namespace sp
