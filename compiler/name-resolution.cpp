// vim: set ts=8 sts=4 sw=4 tw=99 et:
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

#include <unordered_set>

#include <amtl/am-raii.h>

#include "array-helpers.h"
#include "errors.h"
#include "expressions.h"
#include "parse-node.h"
#include "parser.h"
#include "sc.h"
#include "sclist.h"
#include "sctracker.h"
#include "scvars.h"
#include "semantics.h"
#include "symbols.h"

bool
StmtList::Bind(SemaContext& sc)
{
    bool ok = true;
    for (const auto& stmt : stmts_) {
        errorset(sRESET, 0);

        ok &= stmt->Bind(sc);
    }
    return ok;
}

bool
BlockStmt::Bind(SemaContext& sc)
{
    if (stmts_.empty())
        return true;

    AutoEnterScope enter_scope(sc, sLOCAL);
    scope_ = sc.scope();

    return StmtList::Bind(sc);
}

bool
EnumDecl::Bind(SemaContext& sc)
{
    int tag = 0;
    if (label_) {
        if (pc_findtag(label_->chars()) == 0) {
            // No implicit-int allowed.
            error(pos_, 169);
            label_ = nullptr;
        } else {
            tag = gTypes.defineEnumTag(label_->chars())->tagid();
        }
    }

    if (name_) {
        if (label_)
            error(pos_, 168);
        tag = gTypes.defineEnumTag(name_->chars())->tagid();
    } else {
        // The name is automatically the label.
        name_ = label_;
    }

    if (tag) {
        auto spec = deduce_layout_spec_by_tag(tag);
        if (!can_redef_layout_spec(spec, Layout_Enum))
            report(pos_, 110) << name_ << layout_spec_name(spec);
    }

    symbol* enumsym = nullptr;
    PoolList<symbol*>* enumroot = nullptr;
    if (name_) {
        if (vclass_ == sGLOBAL) {
            if ((enumsym = findglb(sc.cc(), name_, pos_.file)) != NULL) {
                // If we were previously defined as a methodmap, don't overwrite the
                // symbol. Otherwise, flow into add_constant where we will error.
                if (enumsym->ident != iMETHODMAP)
                    enumsym = nullptr;
            }
        }

        if (!enumsym) {
            // create the root symbol, so the fields can have it as their "parent"
            enumsym = add_constant(sc.cc(), sc.scope(), name_, 0, vclass_, tag, pos_.file);
            if (enumsym)
                enumsym->enumroot = true;
            // start a new list for the element names
            enumroot = gPoolAllocator.alloc<PoolList<symbol*>>();
        }
    }

    // If this enum is for a methodmap, forget the symbol so code below doesn't
    // build an enum struct.
    if (enumsym && enumsym->ident == iMETHODMAP)
        enumsym = NULL;

    cell value = 0;
    for (const auto& field : fields_ ) {
        if (findconst(sc.cc(), sc.scope(), field.name, pos_.file))
            report(field.pos, 50) << field.name;

        if (field.value && field.value->Bind(sc) && field.value->Analyze(sc)) {
            int field_tag;
            if (field.value->EvalConst(&value, &field_tag)) {
                matchtag(tag, field_tag, MATCHTAG_COERCE | MATCHTAG_ENUM_ASSN);
            } else {
                error(field.pos, 80);
            }
        }

        symbol* sym = add_constant(sc.cc(), sc.scope(), field.name, value, vclass_, tag, pos_.file);
        if (!sym)
            continue;

        // set the item tag and the item size, for use in indexing arrays
        sym->set_parent(enumsym);
        // add the constant to a separate list as well
        if (enumroot) {
            sym->enumfield = true;
            enumroot->emplace_back(sym);
        }

        if (multiplier_ == 1)
            value += increment_;
        else
            value *= increment_ * multiplier_;
    }

    // set the enum name to the "next" value (typically the last value plus one)
    if (enumsym) {
        assert(enumsym->enumroot);
        enumsym->setAddr(value);
        // assign the constant list
        assert(enumroot);
        enumsym->dim.enumlist = enumroot;
    }
    return true;
}

bool
PstructDecl::Bind(SemaContext& sc)
{
    const char* name = name_->chars();
    auto spec = deduce_layout_spec_by_name(name);
    if (!can_redef_layout_spec(spec, Layout_PawnStruct)) {
        report(pos_, 110) << name_ << layout_spec_name(spec);
        return false;
    }
    if (!isupper(*name)) {
        error(pos_, 109, "struct");
        return false;
    }

    pstruct_t* pstruct = pstructs_add(name_);
    gTypes.definePStruct(pstruct->name->chars(), pstruct);

    for (const auto& field : fields_) {
        structarg_t arg;
        arg.type = field.type;
        arg.name = field.name;
        if (arg.type.ident == iARRAY)
            arg.type.ident = iREFARRAY;

        if (field.type.numdim() > 1 || (field.type.numdim() == 1 && field.type.dim[0] != 0)) {
            error(field.pos, 69);
            return false;
        }

        if (!pstructs_addarg(pstruct, &arg)) {
            report(field.pos, 103) << arg.name << layout_spec_name(Layout_PawnStruct);
            return false;
        }
    }
    return true;
}

bool
TypedefDecl::Bind(SemaContext& sc)
{
    Type* prev_type = gTypes.find(name_->chars());
    if (prev_type && prev_type->isDefinedType()) {
        report(pos_, 110) << name_ << prev_type->kindName();
        return false;
    }

    funcenum_t* def = funcenums_add(name_);
    functags_add(def, type_->ToFunctag(sc));
    return true;
}

bool
UsingDecl::Bind(SemaContext& sc)
{
    declare_handle_intrinsics();
    return true;
}

functag_t*
TypedefInfo::ToFunctag(SemaContext& sc) const
{
    auto ft = new functag_t();
    ft->ret_tag = ret_tag;

    for (auto& arg : args) {
        if (arg->type.ident == iARRAY)
            ResolveArraySize(sc, pos, &arg->type, sARGUMENT);

        funcarg_t fa = {};
        fa.type = arg->type;
        if (fa.type.ident == iARRAY)
            fa.type.ident = iREFARRAY;
        if (fa.type.ident != iREFARRAY && fa.type.ident != iARRAY)
            assert(fa.type.dim.empty());
        ft->args.emplace_back(fa);
    }
    return ft;
}

bool
TypesetDecl::Bind(SemaContext& sc)
{
    Type* prev_type = gTypes.find(name_->chars());
    if (prev_type && prev_type->isDefinedType()) {
        report(pos_, 110) << name_ << prev_type->kindName();
        return false;
    }

    funcenum_t* def = funcenums_add(name_);
    for (const auto& type : types_)
        functags_add(def, type->ToFunctag(sc));
    return true;
}

bool
ConstDecl::Bind(SemaContext& sc)
{
    if (!expr_->Bind(sc))
        return false;
    if (!expr_->Analyze(sc))
        return false;

    int tag;
    cell value;
    if (!expr_->EvalConst(&value, &tag))
        return false;

    AutoErrorPos aep(pos_);
    matchtag(type_.tag, tag, 0);

    sym_ = add_constant(sc.cc(), sc.scope(), name_, value, vclass_, type_.tag, pos_.file);
    return true;
}

bool
IsShadowedName(SemaContext& sc, sp::Atom* name)
{
    SymbolScope* scope;
    if (symbol* sym = findloc(sc.scope(), name, &scope)) {
        if (scope != sc.scope())
            return true;
    }
    // ignore implicitly prototyped names.
    if (symbol* sym = findglb(sc.cc(), name, -1))
        return !(sym->ident == iFUNCTN && !sym->defined);
    return false;
}


bool
VarDecl::Bind(SemaContext& sc)
{
    // |int x = x| should bind to outer x, not inner.
    if (init_)
        init_rhs()->Bind(sc);

    if (type_.ident == iARRAY)
        ResolveArraySize(sc, this);

    if (type_.tag == pc_tag_void)
        error(pos_, 144);

    // :TODO: introduce find-by-atom to improve compiler speed
    bool should_define = false;
    if (vclass_ == sGLOBAL) {
        sym_ = findconst(sc.cc(), sc.scope(), name_, pos_.file);
        if (!sym_)
            sym_ = findglb(sc.cc(), name_, pos_.file);

        if (sym_ && sym_->defined) {
            // Can't redefine a global or in-scope static variable.
            if ((!is_static_ && !sym_->is_static) ||
                (sym_->is_static && sym_->fnumber == pos_.file))
            {
                report(pos_, 21) << name_;
                return false;
            }
        }

        if (type_.ident == iREFARRAY) {
            // Dynamic array in global scope.
            assert(type_.is_new);
            error(pos_, 162);
        }

        should_define = !!sym_;
    }

    if (vclass_ != sGLOBAL) {
        // Note: block locals may be named identical to locals at higher
        // compound blocks (as with standard C); so we must check (and add)
        // the "nesting level" of local variables to verify the
        // multi-definition of symbols.
        SymbolScope* scope;
        symbol* sym = findloc(sc.scope(), name_, &scope);
        if (sym && scope == sc.scope())
            report(pos_, 21) << name_;

        // Although valid, a local variable whose name is equal to that
        // of a global variable or to that of a local variable at a lower
        // level might indicate a bug.
        if (vclass_ == sARGUMENT) {
            auto sym = findglb(sc.cc(), name_, pos_.file);
            if (sym && sym->ident != iFUNCTN)
                report(pos_, 219) << name_;
        } else {
            if (IsShadowedName(sc, name_))
                report(pos_, 219) << name_;
        }

        if (vclass_ == sSTATIC && type_.ident == iREFARRAY)
            error(pos_, 165);
    }

    if (gTypes.find(type_.tag)->kind() == TypeKind::Struct) {
        if (!sym_) {
            sym_ = new symbol(name_, 0, iVARIABLE, sGLOBAL, type_.tag);
            AddGlobal(sc.cc(), sym_);
        } else {
            assert(sym_->is_struct);
        }
        sym_->is_struct = true;
        sym_->stock = is_stock_;
        sym_->is_const = true;
    } else {
        if (!sym_) {
            int ident = type_.ident;
            if (vclass_ == sARGUMENT && ident == iARRAY)
                ident = iREFARRAY;

            auto dim = type_.dim.empty() ? nullptr : &type_.dim[0];
            sym_ = NewVariable(name_, 0, ident, vclass_, type_.tag, dim,
                               type_.numdim(), type_.enum_struct_tag());
            sym_->defined = true;
            if (vclass_ == sGLOBAL)
                AddGlobal(sc.cc(), sym_);
            else
                sc.scope()->Add(sym_);

            if (ident == iVARARGS)
                markusage(sym_, uREAD);
        }
        sym_->is_const = type_.is_const;
        sym_->stock = is_stock_;
    }

    if (vclass_ == sGLOBAL && should_define)
        sym_->defined = true;

    if (is_public_) {
        sym_->is_public = true;
        sym_->usage |= uREAD;
    }

    // Note: fnumber implies static scoping for symbol lookup, so until that's
    // fixed we only set it for globals.
    if (vclass_ == sGLOBAL && is_static_)
        sym_->fnumber = pos_.file;

    // LHS bind should now succeed.
    if (init_)
        init_->left()->Bind(sc);
    return true;
}

bool
SymbolExpr::Bind(SemaContext& sc)
{
    return DoBind(sc, false);
}

bool
SymbolExpr::BindLval(SemaContext& sc)
{
    return DoBind(sc, true);
}

bool
SymbolExpr::DoBind(SemaContext& sc, bool is_lval)
{
    AutoErrorPos aep(pos_);

    if (Parser::sInPreprocessor) {
        Parser::sDetectedIllegalPreprocessorSymbols = true;
        if (sc_status == statFIRST) {
            ke::SaveAndSet<bool> restore(&sc_enable_first_pass_error_display, true);
            report(pos_, 230) << name_;
        }
    }

    sym_ = findconst(sc.cc(), sc.scope(), name_, pos_.file);
    if (!sym_)
        sym_ = findloc(sc.scope(), name_);
    if (!sym_)
        sym_ = findglb(sc.cc(), name_, pos_.file);

    if (!sym_) {
        // We assume this is a function that hasn't been seen yet. We should
        // either be in the first pass, or the second pass and skipping writes.
        // If we're writing, then this is an error.
        if (sc_status != statFIRST) {
            report(pos_, 17) << name_;
            return false;
        }

        sym_ = fetchfunc(sc.cc(), name_, pos_.file);
    }

    /* if the function is only in the table because it was inserted as a
     * stub in the first pass (i.e. it was "used" but never declared or
     * implemented, issue an error
     */
    if (sc_status != statFIRST && sym_->ident == iFUNCTN && !sym_->prototyped &&
        sym_ != sc.func())
    {
        report(pos_, 17) << name_;
        return false;
    }

    // Aggressively mark all symbols as read. This is a workaround until the
    // two-pass system can be removed. In the old parser, it was necessary
    // to aggressively generate as much code as early as possible, because
    // marking global functions as read produced correct output in the next
    // pass. However, our new parser short-circuits during the analysis phase
    // if an error occurs, which means we won't have our normal opportunities
    // to call markusage().
    //
    // As a workaround, we always call markusage() during binding. Note that
    // we preserve some old behavior where functions are not marked if being
    // skipped.
    if (!is_lval && !(sym_->ident == iFUNCTN && sc_status == statSKIP))
        markusage(sym_, uREAD);
    return true;
}

bool
ThisExpr::Bind(SemaContext& sc)
{
    AutoErrorPos aep(pos_);

    sym_ = findloc(sc.scope(), gAtoms.add("this"));
    if (!sym_) {
        error(pos_, 166);
        return false;
    }
    return true;
}

bool
CallExpr::Bind(SemaContext& sc)
{
    AutoErrorPos aep(pos_);

    if (!target_->Bind(sc))
        return false;

    bool ok = true;
    for (const auto& arg : args_) {
        if (arg.expr)
           ok &= arg.expr->Bind(sc);
    }
    return ok;
}

bool
CommaExpr::Bind(SemaContext& sc)
{
    AutoErrorPos aep(pos_);

    bool ok = true;
    for (const auto& expr : exprs_) {
       ok &= expr->Bind(sc);
    }
    return ok;
}

bool
ArrayExpr::Bind(SemaContext& sc)
{
    AutoErrorPos aep(pos_);

    bool ok = true;
    for (const auto& expr : exprs_) {
       ok &= expr->Bind(sc);
    }
    return ok;
}

bool
IsDefinedExpr::Bind(SemaContext& sc)
{
    AutoErrorPos aep(pos_);

    symbol* sym = findloc(sc.scope(), name_);
    if (!sym)
        sym = findglb(sc.cc(), name_, pos_.file);
    if (sym && sym->ident == iFUNCTN && !sym->defined)
        sym = nullptr;
    value_ = sym ? 1 : 0;
    if (!value_ && find_subst(name_->chars(), name_->length(), nullptr))
        value_ = 1;
    return true;
}

bool
SizeofExpr::Bind(SemaContext& sc)
{
    AutoErrorPos aep(pos_);

    sym_ = findloc(sc.scope(), ident_);
    if (!sym_)
        sym_ = findglb(sc.cc(), ident_, pos_.file);
    if (!sym_) {
        report(pos_, 17) << ident_;
        return false;
    }
    markusage(sym_, uREAD);
    return true;
}

bool
ChainedCompareExpr::Bind(SemaContext& sc)
{
    AutoErrorPos aep(pos_);

    bool ok = first_->Bind(sc);
    for (const auto& op : ops_)
        ok &= op.expr->Bind(sc);
    return ok;
}

bool
BinaryExprBase::Bind(SemaContext& sc)
{
    bool ok = true;
    if (IsAssignOp(token_))
        ok &= left_->BindLval(sc);
    else
        ok &= left_->Bind(sc);
    ok &= right_->Bind(sc);
    return ok;
}

bool
NewArrayExpr::Bind(SemaContext& sc)
{
    if (already_analyzed_)
        return true;

    bool ok = true;
    for (const auto& expr : exprs_)
        ok &= expr->Bind(sc);
    return ok;
}

bool
StructExpr::Bind(SemaContext& sc)
{
    bool ok = true;
    for (const auto& field : fields_)
        ok &= field.value->Bind(sc);
    return ok;
}

bool
IfStmt::Bind(SemaContext& sc)
{
    bool ok = cond_->Bind(sc);
    ok &= on_true_->Bind(sc);
    if (on_false_)
        ok &= on_false_->Bind(sc);
    return ok;
}

bool
ReturnStmt::Bind(SemaContext& sc)
{
    if (!expr_)
        return true;
    return expr_->Bind(sc);
}

bool
ExitStmt::Bind(SemaContext& sc)
{
    if (!expr_)
        return true;
    return expr_->Bind(sc);
}

bool
DoWhileStmt::Bind(SemaContext& sc)
{
    bool ok = cond_->Bind(sc);
    ok &= body_->Bind(sc);
    return ok;
}

bool
ForStmt::Bind(SemaContext& sc)
{
    bool ok = true;

    ke::Maybe<AutoEnterScope> enter_scope;
    if (init_) {
        if (!init_->IsExprStmt()) {
            enter_scope.init(sc, sLOCAL);
            scope_ = sc.scope();
        }

        ok &= init_->Bind(sc);
    }

    if (cond_)
        ok &= cond_->Bind(sc);
    if (advance_)
        ok &= advance_->Bind(sc);
    ok &= body_->Bind(sc);
    return ok;
}

bool
SwitchStmt::Bind(SemaContext& sc)
{
    bool ok = expr_->Bind(sc);

    if (default_case_)
        ok &= default_case_->Bind(sc);

    for (const auto& pair : cases_) {
        for (const auto& expr : pair.first)
            ok &= expr->Bind(sc);
        ok &= pair.second->Bind(sc);
    }
    return ok;
}

bool
FunctionInfo::Bind(SemaContext& outer_sc)
{
    if (name_ && name_->chars()[0] == PUBLIC_CHAR) {
        // :TODO: deprecate this syntax.
        is_public_ = true;  // implicit public function
        if (is_stock_)
            error(42);      // invalid combination of class specifiers.
    }

    {
        AutoErrorPos error_pos(pos_);

        if (decl_.opertok)
            name_ = NameForOperator();

        sym_ = fetchfunc(outer_sc.cc(), name_, pos_.file);
    }

    SemaContext sc(sym_);
    auto guard = ke::MakeScopeGuard([&]() -> void {
        sc.set_func(nullptr);
    });

    if (strcmp(sym_->name(), uMAINFUNC) == 0) {
        if (!args_.empty())
            error(pos_, 5);     /* "main()" functions may not have any arguments */
        sym_->usage |= uREAD;   /* "main()" is the program's entry point: always used */
        is_public_ = true;
    }

    if (is_public_)
        sym_->is_public = true;
    if (is_static_)
        sym_->is_static = true;
    if (is_stock_)
        sym_->stock = true;
    if (is_forward_)
        sym_->forward = true;
    if (is_native_)
        sym_->native = true;

    sym_->fnumber = pos_.file;
    sym_->lnumber = pos_.line;

    bool ok = true;

    if (this_tag_) {
        Type* type = gTypes.find(*this_tag_);

        typeinfo_t typeinfo = {};
        if (symbol* enum_type = type->asEnumStruct()) {
            typeinfo.tag = 0;
            typeinfo.ident = iREFARRAY;
            typeinfo.declared_tag = *this_tag_;
            typeinfo.dim.emplace_back(enum_type->addr());
        } else {
            typeinfo.tag = *this_tag_;
            typeinfo.ident = iVARIABLE;
            typeinfo.is_const = true;
        }

        auto decl = new VarDecl(pos_, gAtoms.add("this"), typeinfo, sARGUMENT, false,
                                false, false, nullptr);
        args_.emplace(args_.begin(), FunctionArg{decl});
    }


    ke::Maybe<AutoEnterScope> enter_scope;
    if (!args_.empty()) {
        enter_scope.init(sc, sARGUMENT);
        scope_ = sc.scope();

        for (const auto& arg : args_)
            ok &= arg.decl->Bind(sc);

        if (this_tag_ && ok)
            markusage(args_[0].decl->sym(), uREAD);
    }

    if ((sym_->native || sym_->is_public || is_forward_) && decl_.type.numdim() > 0)
        error(pos_, 141);

    // :TODO: remove this. errors are errors.
    if (sc_status == statWRITE && (sym_->usage & uREAD) == 0 && !sym_->is_public && !is_native_) {
        sym_->skipped = true;

        if (!this_tag_) {
            if (body_)
                sym_->defined = true;
            return true;
        }

        // always error on inline methods
        sc_err_status = TRUE;
    }

    if (body_)
        ok &= body_->Bind(sc);

    if (sym_->native && alias_)
        insert_alias(sym_->name(), alias_);

    sc_err_status = FALSE;
    return ok;
}

sp::Atom*
FunctionInfo::NameForOperator()
{
    int count = 0;
    int tags[2] = {0, 0};
    for (const auto& arg : args_) {
        auto var = arg.decl;
        if (count < 2)
            tags[count] = var->type().tag;
        if (var->type().ident != iVARIABLE)
            report(pos_, 66) << var->name();
        if (var->init_rhs())
            report(pos_, 59) << var->name();
        count++;
    }

    /* for '!', '++' and '--', count must be 1
     * for '-', count may be 1 or 2
     * for '=', count must be 1, and the resulttag is also important
     * for all other (binary) operators and the special '~' operator, count must be 2
     */
    switch (decl_.opertok) {
        case '!':
        case '=':
        case tINC:
        case tDEC:
            if (count != 1)
                error(pos_, 62);
            break;
        case '-':
            if (count != 1 && count != 2)
                error(pos_, 62);
            break;
        default:
            if (count != 2)
                error(pos_, 62);
            break;
    }
    if (decl_.type.ident != iVARIABLE)
        error(pos_, 62);

    return operator_symname(decl_.name->chars(), tags[0], tags[1], count, decl_.type.tag);
}

bool
FunctionDecl::Bind(SemaContext& sc)
{
    if (!info_->Bind(sc))
        return false;

    if (deprecate_) {
        symbol* sym = info_->sym();
        sym->documentation = new PoolString(deprecate_->chars(), deprecate_->length());
        sym->deprecated = true;
    }
    return true;
}

bool
PragmaUnusedStmt::Bind(SemaContext& sc)
{
    for (const auto& name : names_) {
        symbol* sym = findloc(sc.scope(), name);
        if (!sym)
            sym = findglb(sc.cc(), name, pos_.file);
        if (!sym) {
            report(pos_, 17) << name;
            continue;
        }
        symbols_.emplace_back(sym);
    }
    return names_.size() == symbols_.size();
}

bool
EnumStructDecl::Bind(SemaContext& sc)
{
    AutoCountErrors errors;
    auto values = gPoolAllocator.alloc<PoolList<symbol*>>();

    if (findglb(sc.cc(), name_, pos_.file) || findconst(sc.cc(), sc.scope(), name_, pos_.file))
        report(pos_, 21) << name_;

    AutoErrorPos error_pos(pos_);
    symbol* root = add_constant(sc.cc(), sc.scope(), name_, 0, sGLOBAL, 0, pos_.file);
    root->tag = gTypes.defineEnumStruct(name_->chars(), root)->tagid();
    root->enumroot = true;
    root->ident = iENUMSTRUCT;

    cell position = 0;
    for (auto& field : fields_) {
        // It's not possible to have circular references other than this, because
        // Pawn is inherently forward-pass only.
        if (field.decl.type.semantic_tag() == root->tag) {
            report(field.pos, 87) << name_;
            continue;
        }

        if (field.decl.type.is_const)
            report(field.pos, 94) << field.decl.name;

        if (field.decl.type.numdim()) {
            if (field.decl.type.ident == iARRAY) {
                ResolveArraySize(sc, field.pos, &field.decl.type, sENUMFIELD);

                if (field.decl.type.numdim() > 1) {
                    error(field.pos, 65);
                    continue;
                }
            } else {
                error(field.pos, 81);
                continue;
            }
        }

        auto field_name = DecorateInnerName(field.pos, field.decl.name);
        if (!field_name)
            continue;

        if (findconst(sc.cc(), sc.scope(), field_name, pos_.file)) {
            report(field.pos, 103) << field.decl.name << "enum struct";
            continue;
        }

        symbol* child = add_constant(sc.cc(), sc.scope(), field_name, position, sGLOBAL,
                                     root->tag, pos_.file);
        if (!child)
            continue;
        child->x.tags.index = field.decl.type.semantic_tag();
        child->x.tags.field = 0;
        child->dim.array.length = field.decl.type.numdim() ? field.decl.type.dim[0] : 0;
        child->dim.array.level = 0;
        child->set_parent(root);
        if (values) {
            child->enumfield = true;
            values->emplace_back(child);
        }

        // Override the name now that it's in the hashtable. We never use the
        // fully decorated name aside from lookups.
        child->setName(field.decl.name);

        cell size = 1;
        if (field.decl.type.numdim()) {
            size = field.decl.type.tag == pc_tag_string
                   ? char_array_cells(field.decl.type.dim[0])
                   : field.decl.type.dim[0];
        }
        position += size;
    }

    if (!position)
        report(pos_, 119) << name_;

    assert(root->enumroot);
    root->setAddr(position);
    root->dim.enumlist = values;

    for (const auto& fun : methods_) {
        auto inner_name = DecorateInnerName(fun->pos(), fun->info()->decl().name);
        if (!inner_name)
            continue;

        fun->info()->set_name(inner_name);
        fun->info()->set_this_tag(root->tag);
        fun->Bind(sc);
    }

    return errors.ok();
}

sp::Atom*
EnumStructDecl::DecorateInnerName(const token_pos_t& pos, sp::Atom* field_name)
{
    auto full_name = ke::StringPrintf("%s::%s", name_->chars(), field_name->chars());
    return gAtoms.add(full_name);
}

bool
MethodmapDecl::Bind(SemaContext& sc)
{
    AutoCountErrors errors;

    AutoErrorPos error_pos(pos_);
    declare_methodmap_symbol(sc.cc(), map_, true);

    std::unordered_set<sp::Atom*> seen;
    for (const auto& prop : properties_) {
        if (seen.count(prop->name)) {
            report(prop->pos, 103) << prop->name << "methodmap";
            continue;
        }
        seen.emplace(prop->name);

        if (prop->type.numdim() > 0) {
            report(prop->pos, 82);
            continue;
        }

        auto method = new methodmap_method_t(map_);
        method->name = prop->name;

        if (prop->getter && BindGetter(sc, prop))
            method->getter = prop->getter->sym();
        if (prop->setter && BindSetter(sc, prop))
            method->setter = prop->setter->sym();

        if (method->getter || method->setter)
            map_->methods.emplace(prop->name, method);
    }

    for (const auto& method : methods_) {
        if (seen.count(method->decl->name())) {
            report(method->decl->pos(), 103) << method->decl->name() << "methodmap";
            continue;
        }
        seen.emplace(method->decl->name());

        bool is_ctor = false;
        if (method->decl->name() == map_->name) {
            // Constructors may not be static.
            if (method->is_static) {
                report(method->decl->pos(), 175);
                continue;
            }
            if (map_->ctor) {
                report(method->decl->pos(), 113) << method->decl->name();
                continue;
            }
            is_ctor = true;

            auto& type = method->decl->info()->mutable_type();
            type.tag = map_->tag;
            type.ident = iVARIABLE;
            type.is_new = true;
        } else if (method->decl->info()->type().ident == 0) {
            // Parsed as a constructor, but not using the map name. This is illegal.
            report(method->decl->pos(), 114) << "constructor" << "methodmap" << map_->name;
            continue;
        }

        if (!method->is_static && !is_ctor)
            method->decl->info()->set_this_tag(map_->tag);

        if (!method->decl->Bind(sc))
            continue;

        auto m = new methodmap_method_t(map_);
        m->name = method->decl->name();
        m->target = method->decl->sym();

        if (is_ctor)
            map_->ctor = m;
        if (method->is_static)
            m->is_static = true;

        map_->methods.emplace(m->name, m);
    }

    map_->keyword_nullable = nullable_;

    return errors.ok();
}

bool
MethodmapDecl::BindGetter(SemaContext& sc, MethodmapProperty* prop)
{
    auto fun = prop->getter;

    // There should be no extra arguments.
    if (fun->args().size() > 0) {
        report(fun->pos(), 127);
        return false;
    }

    fun->set_this_tag(map_->tag);

    return fun->Bind(sc);
}

bool
MethodmapDecl::BindSetter(SemaContext& sc, MethodmapProperty* prop)
{
    auto fun = prop->setter;

    // Must have one extra argument taking the return type.
    if (fun->args().size() > 1) {
        report(150) << pc_tagname(prop->type.tag);
        return false;
    }

    auto decl = fun->args()[0].decl;
    if (decl->type().ident != iVARIABLE || decl->init_rhs() || decl->type().tag != prop->type.tag)
    {
        report(150) << pc_tagname(prop->type.tag);
        return false;
    }

    fun->set_this_tag(map_->tag);

    return fun->Bind(sc);
}

bool
StaticAssertStmt::Bind(SemaContext& sc)
{
    return expr_->Bind(sc);
}
