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
#include "sctracker.h"
#include "scvars.h"
#include "semantics.h"
#include "symbols.h"

bool
SemaContext::BindType(const token_pos_t& pos, TypenameInfo* ti)
{
    if (ti->has_tag())
        return true;

    if (!BindType(pos, ti->type_atom, ti->is_label(), &ti->resolved_tag))
        return false;
    return true;
}

bool
SemaContext::BindType(const token_pos_t& pos, typeinfo_t* ti)
{
    if (ti->has_tag())
        return true;

    int tag;
    if (!BindType(pos, ti->type_atom, ti->is_label, &tag))
        return false;

    auto type = gTypes.find(tag);
    if (auto enum_type = type->asEnumStruct()) {
        if (ti->ident == iREFERENCE) {
            report(pos, 136);
            return false;
        }

        ti->set_tag(0);
        ti->declared_tag = tag;
        ti->dim.emplace_back(enum_type->addr());
        if (!ti->dim_exprs.empty())
            ti->dim_exprs.emplace_back(nullptr);

        if (ti->ident != iARRAY && ti->ident != iREFARRAY) {
            ti->ident = iARRAY;
            ti->has_postdims = true;
        }
    } else {
        ti->set_tag(tag);
    }
    return true;
}

bool
SemaContext::BindType(const token_pos_t& pos, sp::Atom* atom, bool is_label, int* tag)
{
    auto types = &gTypes;
    if (is_label) {
        *tag = types->defineTag(atom->chars())->tagid();
        return true;
    }

    Type* type = types->find(atom);
    if (!type) {
        report(pos, 139) << atom;
        return false;
    }

    if (type->tagid() != types->tag_any() && type->isDeclaredButNotDefined())
        report(pos, 139) << atom;

    *tag = type->tagid();
    return true;
}

bool
ParseTree::ResolveNames(SemaContext& sc)
{
    bool ok = true;
    ok &= StmtList::EnterNames(sc);
    ok &= StmtList::Bind(sc);
    return ok;
}

bool
StmtList::EnterNames(SemaContext& sc)
{
    bool ok = true;
    for (const auto& stmt : stmts_)
        ok &= stmt->EnterNames(sc);
    return ok;
}

bool
StmtList::Bind(SemaContext& sc)
{
    bool ok = true;
    for (const auto& stmt : stmts_) {
        sc.cc().reports()->ResetErrorFlag();

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
EnumDecl::EnterNames(SemaContext& sc)
{
    AutoErrorPos error_pos(pos_);

    int tag = 0;
    if (label_) {
        auto type = gTypes.find(label_);
        if (type && type->tagid() == 0) {
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
        auto spec = deduce_layout_spec_by_tag(sc, tag);
        if (!can_redef_layout_spec(spec, Layout_Enum))
            report(pos_, 110) << name_ << layout_spec_name(spec);
    }

    symbol* enumsym = nullptr;
    EnumData* enumroot = nullptr;
    if (name_) {
        if (vclass_ == sGLOBAL) {
            if ((enumsym = FindSymbol(sc, name_)) != nullptr) {
                // If we were previously defined as a methodmap, don't overwrite the
                // symbol. Otherwise, flow into DefineConstant where we will error.
                if (enumsym->ident != iMETHODMAP)
                    enumsym = nullptr;
            }
        }

        if (!enumsym) {
            // create the root symbol, so the fields can have it as their "parent"
            enumsym = DefineConstant(sc, name_, pos_, 0, vclass_, tag);
            if (enumsym)
                enumsym->enumroot = true;
            // start a new list for the element names
            enumroot = new EnumData();
        }
    }

    // If this enum is for a methodmap, forget the symbol so code below doesn't
    // build an enum struct.
    if (enumsym && enumsym->ident == iMETHODMAP)
        enumsym = NULL;

    std::vector<symbol*> children;

    cell value = 0;
    for (const auto& field : fields_ ) {
        AutoErrorPos error_pos(field.pos);

        if (field.value && field.value->Bind(sc) && sc.sema()->CheckExpr(field.value)) {
            int field_tag;
            if (field.value->EvalConst(&value, &field_tag))
                matchtag(tag, field_tag, MATCHTAG_COERCE | MATCHTAG_ENUM_ASSN);
            else
                error(field.pos, 80);
        }

        symbol* sym = DefineConstant(sc, field.name, field.pos, value, vclass_, tag);
        if (!sym)
            continue;

        // set the item tag and the item size, for use in indexing arrays
        sym->set_parent(enumsym);
        // add the constant to a separate list as well
        if (enumroot) {
            sym->enumfield = true;
            children.emplace_back(sym);
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
        enumsym->set_data(enumroot);
    }

    if (enumroot)
        new (&enumroot->children) PoolArray<symbol*>(children);
    return true;
}

bool
EnumDecl::Bind(SemaContext& sc)
{
    if (vclass_ == sLOCAL)
        return EnterNames(sc);
    return true;
}

bool
PstructDecl::EnterNames(SemaContext& sc)
{
    auto spec = deduce_layout_spec_by_name(sc, name_);
    if (!can_redef_layout_spec(spec, Layout_PawnStruct)) {
        report(pos_, 110) << name_ << layout_spec_name(spec);
        return false;
    }
    if (!isupper(*name_->chars())) {
        error(pos_, 109, "struct");
        return false;
    }

    ps_ = pstructs_add(name_);
    gTypes.definePStruct(ps_->name->chars(), ps_);

    std::vector<structarg_t*> args;
    for (auto& field : fields_) {
        if (pstructs_getarg(ps_, field.name)) {
            report(field.pos, 103) << field.name << layout_spec_name(Layout_PawnStruct);
            return false;
        }

        auto arg = new structarg_t;
        arg->type = field.type;
        arg->name = field.name;
        if (arg->type.ident == iARRAY)
            arg->type.ident = iREFARRAY;
        arg->offs = args.size() * sizeof(cell_t);
        arg->index = (int)args.size();
        args.emplace_back(arg);

        if (field.type.numdim() > 1 || (field.type.numdim() == 1 && field.type.dim[0] != 0)) {
            error(field.pos, 69);
            return false;
        }

        field.field = arg;
    }

    new (&ps_->args) PoolArray<structarg_t*>(args);
    return true;
}

bool
PstructDecl::Bind(SemaContext& sc)
{
    bool ok = true;
    for (const auto& field : fields_) {
        ok &= field.field && sc.BindType(field.pos, &field.field->type);
    }
    return ok;
}

bool
TypedefDecl::EnterNames(SemaContext& sc)
{
    if (Type* prev_type = gTypes.find(name_)) {
        report(pos_, 110) << name_ << prev_type->kindName();
        return false;
    }

    fe_ = funcenums_add(name_);
    return true;
}

bool
TypedefDecl::Bind(SemaContext& sc)
{
    auto ft = type_->Bind(sc);
    if (!ft)
        return false;

    new (&fe_->entries) PoolArray<functag_t*>({ft});
    return true;
}

bool
UsingDecl::EnterNames(SemaContext& sc)
{
    declare_handle_intrinsics();
    return true;
}

functag_t*
TypedefInfo::Bind(SemaContext& sc)
{
    if (!sc.BindType(pos, &ret_type))
        return nullptr;

    auto ft = new functag_t();
    ft->ret_tag = ret_type.tag();

    std::vector<funcarg_t> ft_args;
    for (auto& arg : args) {
        if (!sc.BindType(pos, &arg->type))
            return nullptr;

        if (arg->type.ident == iARRAY)
            ResolveArraySize(sc.sema(), pos, &arg->type, sARGUMENT);

        funcarg_t fa = {};
        fa.type = arg->type;
        if (fa.type.ident == iARRAY)
            fa.type.ident = iREFARRAY;
        if (fa.type.ident != iREFARRAY && fa.type.ident != iARRAY)
            assert(fa.type.dim.empty());
        ft_args.emplace_back(fa);
    }
    new (&ft->args) PoolArray<funcarg_t>(ft_args);

    return ft;
}

bool
TypesetDecl::EnterNames(SemaContext& sc)
{
    if (Type* prev_type = gTypes.find(name_)) {
        report(pos_, 110) << name_ << prev_type->kindName();
        return false;
    }

    fe_ = funcenums_add(name_);
    return true;
}

bool
TypesetDecl::Bind(SemaContext& sc)
{
    bool ok = true;

    std::vector<functag_t*> tags;
    for (const auto& type : types_) {
        auto ft = type->Bind(sc);
        if (!ft) {
            ok = false;
            continue;
        }
        tags.emplace_back(ft);
    }

    new (&fe_->entries) PoolArray<functag_t*>(tags);
    return ok;
}

bool
ConstDecl::EnterNames(SemaContext& sc)
{
    sym_ = DefineConstant(sc, name_, pos_, 0, vclass_, 0);
    return !!sym_;
}

bool
ConstDecl::Bind(SemaContext& sc)
{
    if (sc.func() && !EnterNames(sc))
        return false;

    if (!sym_)
        return false;

    if (!sc.BindType(pos_, &type_))
        return false;

    if (!expr_->Bind(sc))
        return false;
    if (!sc.sema()->CheckExpr(expr_))
        return false;

    int tag;
    cell value;
    if (!expr_->EvalConst(&value, &tag))
        return false;

    AutoErrorPos aep(pos_);
    matchtag(type_.tag(), tag, 0);

    sym_->setAddr(value);
    sym_->tag = type_.tag();
    return true;
}

bool
VarDecl::Bind(SemaContext& sc)
{
    if (!sc.BindType(pos(), &type_))
        return false;

    // |int x = x| should bind to outer x, not inner.
    if (init_)
        init_rhs()->Bind(sc);

    if (type_.ident == iARRAY)
        ResolveArraySize(sc.sema(), this);

    auto types = &gTypes;
    if (type_.tag() == types->tag_void())
        error(pos_, 144);

    if (vclass_ == sGLOBAL) {
        if (type_.ident == iREFARRAY) {
            // Dynamic array in global scope.
            assert(type_.is_new);
            error(pos_, 162);
        }
    }

    bool def_ok = CheckNameRedefinition(sc, name_, pos_, vclass_);

    // REFARRAY is invalid in both file and local static contexts.
    if ((vclass_ == sSTATIC || vclass_ == sGLOBAL) && type_.ident == iREFARRAY)
        error(pos_, 165);

    if (gTypes.find(type_.tag())->kind() == TypeKind::Struct) {
        sym_ = new symbol(name_, 0, iVARIABLE, sGLOBAL, type_.tag());
        sym_->is_struct = true;
        sym_->stock = is_stock_;
        sym_->is_const = true;
    } else {
        int ident = type_.ident;
        if (vclass_ == sARGUMENT && ident == iARRAY)
            ident = iREFARRAY;

        auto dim = type_.dim.empty() ? nullptr : &type_.dim[0];
        sym_ = NewVariable(name_, 0, ident, vclass_, type_.tag(), dim,
                           type_.numdim(), type_.enum_struct_tag());
        sym_->defined = true;
        sym_->is_static = is_static_;

        if (ident == iVARARGS)
            markusage(sym_, uREAD);

        sym_->is_const = type_.is_const;
        sym_->stock = is_stock_;
    }

    if (vclass_ == sGLOBAL)
        sym_->defined = true;

    if (is_public_) {
        sym_->is_public = true;
        sym_->usage |= uREAD;
    }

    sym_->fnumber = pos_.file;
    sym_->lnumber = pos_.line;

    if (def_ok)
        DefineSymbol(sc, sym_);

    // LHS bind should now succeed.
    if (init_)
        init_->left()->BindLval(sc);
    return true;
}

bool
VarDecl::BindType(SemaContext& sc)
{
    return sc.BindType(pos(), &type_);
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

        report(pos_, 230) << name_;
    }

    sym_ = FindSymbol(sc, name_);
    if (!sym_) {
        report(pos_, 17) << name_;
        return false;
    }

    if (!is_lval)
        markusage(sym_, uREAD);
    return true;
}

bool
ThisExpr::Bind(SemaContext& sc)
{
    AutoErrorPos aep(pos_);

    sym_ = FindSymbol(sc, gAtoms.add("this"));
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

    auto sym = FindSymbol(sc, name_);
    if (sym && sym->ident == iFUNCTN && !sym->defined)
        sym = nullptr;
    value_ = sym ? 1 : 0;
    if (!value_ && sc.cc().lexer()->HasMacro(name_))
        value_ = 1;
    return true;
}

bool
SizeofExpr::Bind(SemaContext& sc)
{
    AutoErrorPos aep(pos_);

    sym_ = FindSymbol(sc, ident_);
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
    if (analyzed())
        return analysis_result();

    if (!sc.BindType(pos_, &type_))
        return false;

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

    if (!expr_->Bind(sc))
        return false;

    // Do some peeking to see if this really returns an array. This is a
    // compatibility hack.
    if (auto sym_expr = expr_->as<SymbolExpr>()) {
        if (auto sym = sym_expr->sym()) {
            if (sym->ident == iARRAY || sym->ident == iREFARRAY)
                sc.func_node()->set_maybe_returns_array();
        }
    }

    return true;
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
        if (!init_->is(AstKind::ExprStmt)) {
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
FunctionDecl::EnterNames(SemaContext& sc)
{
    sp::Atom* name = info_->decl().name;

    symbol* sym = nullptr;
    if (!info_->decl().opertok) {
        // Handle forwards.
        sym = FindSymbol(sc, name);
        if (sym && !CanRedefine(sym))
            return false;
    }

    if (!sym) {
        auto scope = info_->is_static() ? sSTATIC : sGLOBAL;
        sym = new symbol(name, 0, iFUNCTN, scope, 0);
        if (info_->decl().opertok)
            sym->is_operator = true;

        DefineSymbol(sc, sym);
    }

    if (info_->body())
        sym->function()->node = info_;
    else if (info_->is_forward())
        sym->function()->forward = info_;

    info_->set_sym(sym);
    return true;
}

bool
FunctionDecl::CanRedefine(symbol* sym)
{
    if (sym->ident != iFUNCTN) {
        report(pos_, 21) << name_;
        return false;
    }

    auto data = sym->function();
    if (data->forward && !info_->is_forward() && !info_->is_native() && !info_->is_stock() &&
        !info_->is_static())
    {
        if (info_->is_public())
            return true;

        report(pos_, 245) << sym->name();

        info_->set_is_public();
        return true;
    }

    if (data->node) {
        if (info_->is_forward() && !data->node->is_public()) {
            report(pos_, 412) << name_;
            return false;
        }
        if (info_->body() && !data->forward) {
            report(pos_, 21) << name_;
            return false;
        }
        return true;
    }

    report(pos(), 21) << sym->name();
    return false;
}

bool
FunctionInfo::Bind(SemaContext& outer_sc)
{
    if (!outer_sc.BindType(pos_, &decl_.type))
        return false;

    // Only named functions get an early symbol in EnterNames.
    if (!sym_)
        sym_ = new symbol(decl_.name, 0, iFUNCTN, sGLOBAL, 0);

    // This may not be set if EnterNames wasn't called (eg not a global).
    if (body_ && !sym_->function()->node)
        sym_->function()->node = this;

    // The forward's prototype is canonical. If this symbol has a forward, we
    // don't set or override the return type when we see the public
    // implementation. Note that args are checked similarly in BindArgs.
    if (!sym_->function()->forward || is_forward_) {
        sym_->tag = decl_.type.tag();
        sym_->explicit_return_type = decl_.type.is_new;
    }

    // But position info belongs to the implementation.
    if (!sym_->function()->forward || is_public_) {
        sym_->fnumber = pos_.file;
        sym_->lnumber = pos_.line;
    }

    // Ensure |this| argument exists.
    if (this_tag_) {
        Type* type = gTypes.find(*this_tag_);

        typeinfo_t typeinfo = {};
        if (symbol* enum_type = type->asEnumStruct()) {
            typeinfo.set_tag(0);
            typeinfo.ident = iREFARRAY;
            typeinfo.declared_tag = *this_tag_;
            typeinfo.dim.emplace_back(enum_type->addr());
        } else {
            typeinfo.set_tag(*this_tag_);
            typeinfo.ident = iVARIABLE;
            typeinfo.is_const = true;
        }

        auto decl = new VarDecl(pos_, gAtoms.add("this"), typeinfo, sARGUMENT, false,
                                false, false, nullptr);
        assert(args_[0] == nullptr);
        args_[0] = decl;
    }

    // Bind all argument types, so we can get an operator name if needed.
    bool ok = true;
    for (const auto& decl : args_)
        ok &= decl->BindType(outer_sc);

    if (!ok)
        return false;

    if (name_ && name_->chars()[0] == PUBLIC_CHAR) {
        // :TODO: deprecate this syntax.
        is_public_ = true;  // implicit public function
        if (is_stock_)
            error(42);      // invalid combination of class specifiers.
    }

    if (decl_.opertok)
        name_ = NameForOperator();

    SemaContext sc(outer_sc, sym_, this);
    auto restore_sc = ke::MakeScopeGuard([&outer_sc]() {
        outer_sc.sema()->set_context(&outer_sc);
    });
    sc.sema()->set_context(&sc);

    if (strcmp(sym_->name(), uMAINFUNC) == 0) {
        if (!args_.empty())
            error(pos_, 5);     /* "main()" functions may not have any arguments */
        sym_->usage |= uREAD;   /* "main()" is the program's entry point: always used */
        is_public_ = true;
    }

    if (body_ || is_native_)
        sym_->defined = true;
    if (is_public_)
        sym_->is_public = true;
    if (is_static_)
        sym_->is_static = true;
    if (is_stock_)
        sym_->stock = true;
    if (is_native_) {
        sym_->native = true;
        sym_->setAddr(-1);
    }

    ke::Maybe<AutoEnterScope> enter_scope;
    if (!args_.empty()) {
        enter_scope.init(sc, sARGUMENT);
        scope_ = sc.scope();

        for (const auto& arg : args_)
            ok &= arg->Bind(sc);

        if (this_tag_ && ok)
            markusage(args_[0]->sym(), uREAD);
    }

    if ((sym_->native || sym_->is_public || is_forward_) && decl_.type.numdim() > 0)
        error(pos_, 141);

    ok &= BindArgs(sc);

    if (body_)
        ok &= body_->Bind(sc);

    if (sym_->native && alias_) {
        auto alias_sym = FindSymbol(sc, alias_);
        if (!alias_sym)
            report(pos_, 17) << alias_;
        sym_->function()->alias = alias_sym;
    }
    return ok;
}

bool
FunctionInfo::BindArgs(SemaContext& sc)
{
    std::vector<arginfo> arglist;

    AutoCountErrors errors;
    for (const auto& var : args_) {
        const auto& typeinfo = var->type();
        symbol* argsym = var->sym();

        AutoErrorPos pos(var->pos());

        if (typeinfo.ident == iVARARGS) {
            /* redimension the argument list, add the entry iVARARGS */
            arglist.emplace_back();
            arglist.back().type.ident = iVARARGS;
            arglist.back().type.set_tag(typeinfo.tag());
            continue;
        }

        Type* type = gTypes.find(typeinfo.semantic_tag());
        if (type->isEnumStruct()) {
            if (sym_->native)
                error(135, type->name());
        }

        /* Stack layout:
         *   base + 0*sizeof(cell)  == previous "base"
         *   base + 1*sizeof(cell)  == function return address
         *   base + 2*sizeof(cell)  == number of arguments
         *   base + 3*sizeof(cell)  == first argument of the function
         * So the offset of each argument is "(argcnt+3) * sizeof(cell)".
         *
         * Since arglist has an empty terminator at the end, we actually add 2.
         */
        argsym->setAddr(static_cast<cell>((arglist.size() + 3) * sizeof(cell)));

        arglist.emplace_back();
        arginfo& arg = arglist.back();
        arg.name = var->name();
        arg.type.ident = argsym->ident;
        arg.type.is_const = argsym->is_const;
        arg.type.set_tag(argsym->tag);
        arg.type.dim = typeinfo.dim;
        arg.type.declared_tag = typeinfo.enum_struct_tag();

        if (typeinfo.ident == iREFARRAY || typeinfo.ident == iARRAY) {
            if (sc.sema()->CheckVarDecl(var) && var->init_rhs())
                fill_arg_defvalue(sc.cc(), var, &arg);
        } else {
            Expr* init = var->init_rhs();
            if (init && sc.sema()->CheckExpr(init)) {
                assert(typeinfo.ident == iVARIABLE || typeinfo.ident == iREFERENCE);
                arg.def = new DefaultArg();

                int tag;
                cell val;
                if (!init->EvalConst(&val, &tag)) {
                    error(8);

                    // Populate to avoid errors.
                    val = 0;
                    tag = typeinfo.tag();
                }
                arg.def->tag = tag;
                arg.def->val = ke::Some(val);

                matchtag(arg.type.tag(), arg.def->tag, MATCHTAG_COERCE);
            }
        }

        if (arg.type.ident == iREFERENCE)
            argsym->usage |= uREAD; /* because references are passed back */
        if (sym_->callback || sym_->stock || sym_->is_public)
            argsym->usage |= uREAD; /* arguments of public functions are always "used" */

        /* arguments of a public function may not have a default value */
        if (sym_->is_public && arg.def)
            error(59, var->name()->chars());
    }

    // Now, see if the function already had an argument list.
    auto& prev_args = sym_->function()->args;
    if (prev_args.empty()) {
        // No, replace it with the new list.
        new (&sym_->function()->args) PoolArray<arginfo>(std::move(arglist));
        return errors.ok();
    }

    // If we get here, we're a public and forward pair, and we need to compare
    // to make sure the argument lists are compatible.
    assert(sym_->function()->forward);
    assert(sym_->function()->node);
    token_pos_t error_pos = sym_->function()->node->pos();

    size_t fwd_argc = prev_args.size();
    size_t impl_argc = arglist.size();
    if (is_forward_)
        std::swap(fwd_argc, impl_argc);

    // We allow forwards to omit arguments in their signature, so this is not
    // a straight-up equality test.
    if (impl_argc > fwd_argc) {
        report(error_pos, 25);
        return false;
    }

    for (size_t i = 0; i < impl_argc; i++) {
        if (!argcompare(&arglist[i], &prev_args[i]))
            report(error_pos, 181) << arglist[i].name;
    }

    // No matter what, always replace the canonical argument list with the
    // implementation's. This is so names will bind correctly in CallExpr,
    // though we should really kill off arglist entirely and use VarDecls.
    if (is_public_)
        new (&sym_->function()->args) PoolArray<arginfo>(std::move(arglist));

    return errors.ok();
}

sp::Atom*
FunctionInfo::NameForOperator()
{
    std::vector<std::string> params;

    int count = 0;
    int tags[2] = {0, 0};
    for (const auto& var : args_) {
        if (count < 2)
            tags[count] = var->type().tag();
        if (var->type().ident != iVARIABLE)
            report(pos_, 66) << var->name();
        if (var->init_rhs())
            report(pos_, 59) << var->name();
        count++;

        auto type = gTypes.find(var->type().tag());
        params.emplace_back(type->name());
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

    std::string name =
        "operator" + get_token_string(decl_.opertok) + "(" + ke::Join(params, ",") + ")";
    return gAtoms.add(name);
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
    std::vector<symbol*> symbols;
    for (const auto& name : names_) {
        symbol* sym = FindSymbol(sc, name);
        if (!sym) {
            report(pos_, 17) << name;
            continue;
        }
        symbols.emplace_back(sym);
    }

    new (&symbols_) PoolArray<symbol*>(symbols);

    return names_.size() == symbols_.size();
}

bool
EnumStructDecl::EnterNames(SemaContext& sc)
{
    AutoCountErrors errors;

    AutoErrorPos error_pos(pos_);
    root_ = DefineConstant(sc, name_, pos_, 0, sGLOBAL, 0);
    root_->tag = gTypes.defineEnumStruct(name_->chars(), root_)->tagid();
    root_->enumroot = true;
    root_->ident = iENUMSTRUCT;

    auto data = new EnumStructData;
    root_->set_data(data);

    std::unordered_set<sp::Atom*> seen;
    std::vector<symbol*> fields;

    cell position = 0;
    for (auto& field : fields_) {
        if (!sc.BindType(field.pos, &field.decl.type))
            continue;

        // It's not possible to have circular references other than this, because
        // Pawn is inherently forward-pass only.
        if (field.decl.type.semantic_tag() == root_->tag) {
            report(field.pos, 87) << name_;
            continue;
        }

        if (field.decl.type.is_const)
            report(field.pos, 94) << field.decl.name;

        if (field.decl.type.numdim()) {
            if (field.decl.type.ident == iARRAY) {
                ResolveArraySize(sc.sema(), field.pos, &field.decl.type, sENUMFIELD);

                if (field.decl.type.numdim() > 1) {
                    error(field.pos, 65);
                    continue;
                }
            } else {
                error(field.pos, 81);
                continue;
            }
        }

        if (seen.count(field.decl.name)) {
            report(field.pos, 103) << field.decl.name << "enum struct";
            continue;
        }
        seen.emplace(field.decl.name);

        symbol* child = new symbol(field.decl.name, position, iCONSTEXPR, sGLOBAL, root_->tag);
        child->x.tags.index = field.decl.type.semantic_tag();
        child->x.tags.field = 0;
        child->dim.array.length = field.decl.type.numdim() ? field.decl.type.dim[0] : 0;
        child->dim.array.level = 0;
        child->set_parent(root_);
        child->enumfield = true;
        fields.emplace_back(child);

        cell size = 1;
        if (field.decl.type.numdim()) {
            size = field.decl.type.tag() == pc_tag_string
                   ? char_array_cells(field.decl.type.dim[0])
                   : field.decl.type.dim[0];
        }
        position += size;
    }

    if (!position)
        report(pos_, 119) << name_;

    std::vector<symbol*> methods;
    for (const auto& decl : methods_) {
        auto info = decl->info();
        if (seen.count(decl->name())) {
            report(decl->pos(), 103) << decl->name() << "enum struct";
            continue;
        }
        seen.emplace(decl->name());

        auto sym = new symbol(decl->name(), 0, iFUNCTN, sGLOBAL, 0);
        sym->set_parent(root_);
        info->set_sym(sym);
        methods.emplace_back(sym);
    }

    new (&data->fields) PoolArray<symbol*>(fields);
    new (&data->methods) PoolArray<symbol*>(methods);

    assert(root_->enumroot);
    root_->setAddr(position);

    return errors.ok();
}

bool
EnumStructDecl::Bind(SemaContext& sc)
{
    if (!root_)
        return false;

    AutoCountErrors errors;
    for (const auto& fun : methods_) {
        auto inner_name = DecorateInnerName(name_, fun->info()->decl().name);
        if (!inner_name)
            continue;

        fun->info()->set_name(inner_name);
        fun->info()->set_this_tag(root_->tag);
        fun->Bind(sc);
    }
    return errors.ok();
}

sp::Atom*
Decl::DecorateInnerName(sp::Atom* parent_name, sp::Atom* field_name)
{
    auto full_name = ke::StringPrintf("%s.%s", parent_name->chars(), field_name->chars());
    return gAtoms.add(full_name);
}

bool
MethodmapDecl::EnterNames(SemaContext& sc)
{
    AutoErrorPos error_pos(pos_);

    auto old_spec = deduce_layout_spec_by_name(sc, name_);
    if (!can_redef_layout_spec(Layout_MethodMap, old_spec))
        report(110) << name_ << layout_spec_name(old_spec);

    map_ = methodmap_add(nullptr, Layout_MethodMap, name_);
    gTypes.defineMethodmap(name_->chars(), map_);

    sym_ = declare_methodmap_symbol(sc.cc(), map_);
    if (!sym_)
        return false;

    for (auto& prop : properties_) {
        if (map_->methods.count(prop->name)) {
            report(prop->pos, 103) << prop->name << "methodmap";
            continue;
        }

        auto method = new methodmap_method_t(map_);
        method->name = prop->name;
        map_->methods.emplace(prop->name, method);

        prop->entry = method;
    }

    for (auto& method : methods_) {
        if (map_->methods.count(method->decl->name())) {
            report(method->decl->pos(), 103) << method->decl->name() << "methodmap";
            continue;
        }

        auto m = new methodmap_method_t(map_);
        m->name = method->decl->name();
        if (m->name == map_->name) {
            if (map_->ctor) {
                report(method->decl->pos(), 113) << method->decl->name();
                continue;
            }
            map_->ctor = m;
        }
        map_->methods.emplace(m->name, m);

        method->entry = m;
    }
    return true;
}

bool
MethodmapDecl::Bind(SemaContext& sc)
{
    AutoCountErrors errors;

    methodmap_t* extends_map = nullptr;
    if (extends_) {
        if ((extends_map = methodmap_find_by_name(extends_)) == nullptr)
            report(pos_, 102) << "methodmap" << extends_;
    }

    if (extends_map) {
        if (!extends_map->is_bound)
            report(pos_, 409) << extends_;

        for (auto iter = extends_map; iter; iter = iter->parent) {
            if (iter == map_) {
                extends_map = nullptr;
                report(pos_, 410);
            }
        }
    }

    map_->parent = extends_map;
    if (map_->parent)
        map_->nullable = map_->parent->nullable;

    map_->is_bound = true;

    for (const auto& prop : properties_) {
        if (!sc.BindType(prop->pos, &prop->type))
            return false;

        if (prop->type.numdim() > 0) {
            report(prop->pos, 82);
            continue;
        }

        auto method = prop->entry;

        if (prop->getter && BindGetter(sc, prop)) {
            method->getter = prop->getter->sym();
            method->getter->set_parent(sym_);

            auto name = ke::StringPrintf("%s.%s.get", name_->chars(), prop->name->chars());
            method->getter->setName(gAtoms.add(name));
        }
        if (prop->setter && BindSetter(sc, prop)) {
            method->setter = prop->setter->sym();
            method->setter->set_parent(sym_);

            auto name = ke::StringPrintf("%s.%s.set", name_->chars(), prop->name->chars());
            method->setter->setName(gAtoms.add(name));
        }
    }

    for (const auto& method : methods_) {
        bool is_ctor = false;
        if (method->decl->name() == map_->name) {
            // Constructors may not be static.
            if (method->is_static) {
                report(method->decl->pos(), 175);
                continue;
            }
            is_ctor = true;

            auto& type = method->decl->info()->mutable_type();
            type.set_tag(map_->tag);
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

        method->decl->sym()->set_parent(sym_);
        method->decl->sym()->setName(DecorateInnerName(name_, method->decl->name()));

        auto m = method->entry;
        m->target = method->decl->sym();
        if (method->is_static)
            m->is_static = true;
    }

    map_->keyword_nullable = nullable_;
    return errors.ok();
}

bool
MethodmapDecl::BindGetter(SemaContext& sc, MethodmapProperty* prop)
{
    auto fun = prop->getter;

    // There should be no extra arguments.
    if (fun->args().size() > 1) {
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
    if (fun->args().size() > 2) {
        report(prop->pos, 150) << pc_tagname(prop->type.tag());
        return false;
    }

    fun->set_this_tag(map_->tag);

    if (!fun->Bind(sc))
        return false;

    if (fun->args().size() <= 1) {
        report(prop->pos, 150) << pc_tagname(prop->type.tag());
        return false;
    }

    auto decl = fun->args()[1];
    if (decl->type().ident != iVARIABLE || decl->init_rhs() ||
        decl->type().tag() != prop->type.tag())
    {
        report(prop->pos, 150) << pc_tagname(prop->type.tag());
        return false;
    }
    return true;
}

bool
CastExpr::Bind(SemaContext& sc)
{
    bool ok = true;
    ok &= sc.BindType(pos_, &type_);
    ok &= expr_->Bind(sc);
    return ok;
}

bool
StaticAssertStmt::Bind(SemaContext& sc)
{
    return expr_->Bind(sc);
}

bool
ChangeScopeNode::EnterNames(SemaContext& sc)
{
    sc.set_scope(scope_);
    return true;
}

bool
ChangeScopeNode::Bind(SemaContext& sc)
{
    sc.set_scope(scope_);
    return true;
}
