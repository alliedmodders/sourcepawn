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

    scope_ = CreateScope();

    AutoEnterScope enter_scope(scope_);
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
            error(pos_, 110, name_->chars(), layout_spec_name(spec));
    }

    symbol* enumsym = nullptr;
    constvalue* enumroot = nullptr;
    if (name_) {
        if (vclass_ == sGLOBAL) {
            if ((enumsym = findglb(name_->chars())) != NULL) {
                // If we were previously defined as a methodmap, don't overwrite the
                // symbol. Otherwise, flow into add_constant where we will error.
                if (enumsym->ident != iMETHODMAP)
                    enumsym = nullptr;
            }
        }

        if (!enumsym) {
            // create the root symbol, so the fields can have it as their "parent"
            enumsym = add_constant(name_->chars(), 0, vclass_, tag);
            if (enumsym)
                enumsym->enumroot = true;
            // start a new list for the element names
            if ((enumroot = (constvalue*)calloc(1, sizeof(constvalue))) == NULL)
                error(pos_, FATAL_ERROR_OOM); /* insufficient memory (fatal error) */
        }
    }

    // If this enum is for a methodmap, forget the symbol so code below doesn't
    // build an enum struct.
    if (enumsym && enumsym->ident == iMETHODMAP)
        enumsym = NULL;

    cell value = 0;
    for (const auto& field : fields_ ) {
        if (findconst(field.name->chars()))
            error(field.pos, 50, field.name->chars());

        if (field.value && field.value->Bind(sc) && field.value->Analyze(sc)) {
            int field_tag;
            if (field.value->EvalConst(&value, &field_tag)) {
                matchtag(tag, field_tag, MATCHTAG_COERCE | MATCHTAG_ENUM_ASSN);
            } else {
                error(field.pos, 80);
            }
        }

        symbol* sym = add_constant(field.name->chars(), value, vclass_, tag);
        if (!sym)
            continue;

        // set the item tag and the item size, for use in indexing arrays
        sym->set_parent(enumsym);
        // add the constant to a separate list as well
        if (enumroot) {
            sym->enumfield = true;
            append_constval(enumroot, field.name->chars(), value, tag);
        }

        if (multiplier_ == 1)
            value += increment_;
        else
            value *= increment_ * multiplier_;
    }

    // set the enum name to the "next" value (typically the last value plus one)
    if (enumsym) {
        assert(enumsym->enumroot);
        enumsym->setAddr(0);
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
        error(pos_, 110, name, layout_spec_name(spec));
        return false;
    }
    if (!isupper(*name)) {
        error(pos_, 109, "struct");
        return false;
    }

    pstruct_t* pstruct = pstructs_add(name);
    gTypes.definePStruct(pstruct->name, pstruct);

    for (const auto& field : fields_) {
        structarg_t arg;
        arg.tag = field.type.tag;
        arg.name = field.name;
        arg.fconst = field.type.is_const;
        arg.ident = field.type.ident;
        if (arg.ident == iARRAY)
            arg.ident = iREFARRAY;

        if (field.type.numdim > 1 || (field.type.numdim == 1 && field.type.dim[0] != 0)) {
            error(field.pos, 69);
            return false;
        }

        if (!pstructs_addarg(pstruct, &arg)) {
            error(field.pos, 103, arg.name, layout_spec_name(Layout_PawnStruct));
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
        error(pos_, 110, name_->chars(), prev_type->kindName());
        return false;
    }

    funcenum_t* def = funcenums_add(name_->chars());
    functags_add(def, type_);
    return true;
}

bool
UsingDecl::Bind(SemaContext& sc)
{
    declare_handle_intrinsics();
    return true;
}

bool
TypesetDecl::Bind(SemaContext& sc)
{
    Type* prev_type = gTypes.find(name_->chars());
    if (prev_type && prev_type->isDefinedType()) {
        error(pos_, 110, name_->chars(), prev_type->kindName());
        return false;
    }

    funcenum_t* def = funcenums_add(name_->chars());
    for (const auto& type : types_)
        functags_add(def, type);
    return true;
}

bool
ConstDecl::Bind(SemaContext& sc)
{
    AutoErrorPos aep(pos_);

    sym_ = add_constant(name_->chars(), value_, vclass_, type_.tag);
    return true;
}

bool
is_shadowed_name(sp::Atom* name)
{
    symbol* scope;
    if (symbol* sym = findloc(name, &scope)) {
        if (scope != GetScopeChain())
            return true;
    }
    // ignore implicitly prototyped names.
    if (symbol* sym = findglb(name))
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
        sym_ = findconst(name_->chars());
        if (!sym_)
            sym_ = findglb(name_);

        // This will go away when we remove the two-pass system.
        if (sym_ && sym_->defined) {
            error(pos_, 21, name_->chars());
            return false;
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
        symbol* scope;
        symbol* sym = findloc(name_->chars(), &scope);
        if (sym && scope == GetScopeChain())
            error(pos_, 21, name_->chars());

        // Although valid, a local variable whose name is equal to that
        // of a global variable or to that of a local variable at a lower
        // level might indicate a bug.
        if (vclass_ == sARGUMENT) {
            auto sym = findglb(name_);
            if (sym && sym->ident != iFUNCTN)
                error(pos_, 219, name_->chars());
        } else {
            if (is_shadowed_name(name_))
                error(pos_, 219, name_->chars());
        }

        if (vclass_ == sSTATIC && type_.ident == iREFARRAY)
            error(pos_, 165);
    }

    if (gTypes.find(type_.tag)->kind() == TypeKind::Struct) {
        if (!sym_)
            sym_ = addsym(name_->chars(), 0, iVARIABLE, sGLOBAL, type_.tag);
        else
            assert(sym_->is_struct);
        sym_->is_struct = true;
        sym_->stock = is_stock_;
        sym_->is_const = true;
    } else {
        if (!sym_) {
            int ident = type_.ident;
            if (vclass_ == sARGUMENT && ident == iARRAY)
                ident = iREFARRAY;

            sym_ = addvariable(name_->chars(), 0, ident, vclass_, type_.tag, type_.dim,
                               type_.numdim, type_.idxtag);

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
            error(pos_, 230, name_->chars());
        }
    }

    sym_ = findconst(name_->chars());
    if (!sym_)
        sym_ = findloc(name_);
    if (!sym_)
        sym_ = findglb(name_);

    if (!sym_) {
        // We assume this is a function that hasn't been seen yet. We should
        // either be in the first pass, or the second pass and skipping writes.
        // If we're writing, then this is an error.
        if (sc_status != statFIRST) {
            error(pos_, 17, name_->chars());
            return false;
        }

        sym_ = fetchfunc(name_->chars());
    }

    /* if the function is only in the table because it was inserted as a
     * stub in the first pass (i.e. it was "used" but never declared or
     * implemented, issue an error
     */
    if (sc_status != statFIRST && sym_->ident == iFUNCTN && !sym_->prototyped &&
        sym_ != sc.func())
    {
        error(pos_, 17, name_->chars());
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

    sym_ = findloc("this");
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

    symbol* sym = findloc(name_->chars());
    if (!sym)
        sym = findglb(name_);
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

    sym_ = findloc(ident_);
    if (!sym_)
        sym_ = findglb(ident_);
    if (!sym_) {
        error(pos_, 17, ident_->chars());
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
            scope_ = CreateScope();
            enter_scope.init(scope_);
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
FunctionInfo::Bind(SemaContext& sc)
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

        sym_ = fetchfunc(name_->chars());
    }

    sc.set_func(sym_);
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

    ke::Maybe<AutoEnterScope> enter_scope;
    if (!args_.empty()) {
        scope_ = CreateScope();
        enter_scope.init(scope_);

        for (const auto& arg : args_)
            ok &= arg.decl->Bind(sc);

        if (this_tag_ && ok)
            markusage(args_[0].decl->sym(), uREAD);
    }

    if ((sym_->native || sym_->is_public || is_forward_) && decl_.type.numdim > 0)
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
            error(pos_, 66, var->name()->chars());
        if (var->init_rhs())
            error(pos_, 59, var->name()->chars());
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

    char opername[sNAMEMAX + 1];
    operator_symname(opername, decl_.name, tags[0], tags[1], count, decl_.type.tag);
    return gAtoms.add(opername);
}

bool
FunctionDecl::Bind(SemaContext& sc)
{
    if (!info_->Bind(sc))
        return false;

    if (deprecate_) {
        symbol* sym = info_->sym();
        sym->documentation = std::string(deprecate_->chars(), deprecate_->length());
        sym->deprecated = true;
    }
    return true;
}

bool
PragmaUnusedStmt::Bind(SemaContext& sc)
{
    for (const auto& name : names_) {
        symbol* sym = findloc(name);
        if (!sym)
            sym = findglb(name);
        if (!sym) {
            error(pos_, 17, name->chars());
            continue;
        }
        symbols_.emplace_back(sym);
    }
    return names_.size() == symbols_.size();
}
