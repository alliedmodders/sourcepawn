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
#include "semantics.h"
#include "symbols.h"

AutoEnterScope::AutoEnterScope(SemaContext& sc, SymbolScope* scope)
  : sc_(sc),
    prev_(sc.scope())
{
    sc.set_scope(scope);
}

AutoEnterScope::AutoEnterScope(SemaContext& sc, ScopeKind kind)
  : sc_(sc),
    prev_(sc.scope())
{
    sc.set_scope(new SymbolScope(sc.scope(), kind));
}

AutoEnterScope::~AutoEnterScope()
{
    sc_.set_scope(prev_);
}

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

    auto type = cc_.types()->find(tag);
    if (auto enum_type = type->asEnumStruct()) {
        if (ti->ident == iREFERENCE) {
            report(pos, 136);
            return false;
        }

        ti->set_tag(0);
        ti->declared_tag = tag;
        ti->dim.emplace_back(enum_type->addr());

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
    auto types = cc_.types();

    Type* type = types->find(atom);
    if (!type) {
        if (!is_label) {
            report(pos, 139) << atom;
            return false;
        }

        *tag = types->defineTag(atom)->tagid();
        return true;
    }

    *tag = type->tagid();
    return true;
}

bool
ParseTree::ResolveNames(SemaContext& sc)
{
    bool ok = true;
    ok &= stmts_->EnterNames(sc);
    ok &= stmts_->Bind(sc);
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

    AutoCreateScope enter_scope(sc, sLOCAL, &scope_);

    return StmtList::Bind(sc);
}

bool
EnumDecl::EnterNames(SemaContext& sc)
{
    AutoErrorPos error_pos(pos_);

    int tag = 0;
    if (label_) {
        auto type = sc.cc().types()->find(label_);
        if (!type) {
            tag = sc.cc().types()->defineEnumTag(label_->chars())->tagid();
        } else if (type->kind() == TypeKind::Int) {
            // No implicit-int allowed.
            report(pos_, 169);
            label_ = nullptr;
        } else if (type->kind() != TypeKind::Methodmap && type->kind() != TypeKind::Enum) {
            report(pos_, 432) << label_ << type->kindName();
        } else {
            tag = type->tagid();
        }
    }

    if (name_) {
        if (label_)
            error(pos_, 168);

        if (auto type = sc.cc().types()->find(name_)) {
            if (type->kind() != TypeKind::Methodmap && type->kind() != TypeKind::Enum)
                report(pos_, 432) << name_ << type->kindName();
            tag = type->tagid();
        } else {
            tag = sc.cc().types()->defineEnumTag(name_->chars())->tagid();
        }
    } else {
        // The name is automatically the label.
        name_ = label_;
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
    if (auto type = sc.cc().types()->find(name_)) {
        report(pos_, 432) << name_ << type->kindName();
        return false;
    }
    if (!isupper(*name_->chars())) {
        error(pos_, 109, "struct");
        return false;
    }

    ps_ = sc.cc().types()->definePStruct(name_);

    std::vector<structarg_t*> args;
    for (auto& field : fields_) {
        if (ps_->GetArg(field.name)) {
            report(field.pos, 103) << field.name << "internal struct";
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
    if (Type* prev_type = sc.cc().types()->find(name_)) {
        report(pos_, 432) << name_ << prev_type->kindName();
        return false;
    }

    fe_ = funcenums_add(sc.cc(), name_, false);
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
    if (Type* prev_type = sc.cc().types()->find(name_)) {
        report(pos_, 432) << name_ << prev_type->kindName();
        return false;
    }

    fe_ = funcenums_add(sc.cc(), name_, false);
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
    if (!expr_->EvalConst(&value, &tag)) {
        report(expr_, 8);
        return false;
    }

    AutoErrorPos aep(pos_);
    matchtag(type_.tag(), tag, 0);

    sym_->setAddr(value);
    sym_->tag = type_.tag();
    return true;
}

bool VarDeclBase::Bind(SemaContext& sc) {
    if (!sc.BindType(pos(), &type_))
        return false;

    // |int x = x| should bind to outer x, not inner.
    if (init_)
        init_rhs()->Bind(sc);

    if (type_.ident == iARRAY)
        ResolveArraySize(sc.sema(), this);

    auto types = sc.cc().types();
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

    if (sc.cc().types()->find(type_.tag())->kind() == TypeKind::Struct) {
        sym_ = new symbol(name_, 0, iVARIABLE, sGLOBAL, type_.tag());
        sym_->is_struct = true;
        sym_->stock = is_stock_;
        sym_->is_const = true;
    } else {
        IdentifierKind ident = type_.ident;
        if (vclass_ == sARGUMENT && ident == iARRAY)
            type_.ident = ident = iREFARRAY;

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

bool VarDeclBase::BindType(SemaContext& sc) {
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

    if (sc.cc().in_preprocessor()) {
        sc.cc().detected_illegal_preproc_symbols() = true;

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

    sym_ = FindSymbol(sc, sc.cc().atom("this"));
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
    for (const auto& arg : args_)
       ok &= arg->Bind(sc);
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
        ok &= field->value->Bind(sc);
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

    ke::Maybe<AutoCreateScope> enter_scope;
    if (init_) {
        if (!init_->is(StmtKind::ExprStmt))
            enter_scope.init(sc, sLOCAL, &scope_);

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
    symbol* sym = nullptr;
    if (!decl_.opertok) {
        // Handle forwards.
        sym = FindSymbol(sc, name_);
        if (sym && !CanRedefine(sym))
            return false;
    }

    if (!sym) {
        auto scope = is_static() ? sSTATIC : sGLOBAL;
        sym = new symbol(name_, 0, iFUNCTN, scope, 0);
        if (decl_.opertok)
            sym->is_operator = true;

        DefineSymbol(sc, sym);
    }

    // Prioritize the implementation as the canonical signature.
    if (!sym->function()->node || body_)
        sym->function()->node = this;

    if (is_forward())
        sym->function()->forward = this;

    sym_ = sym;
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
    if (data->forward && !is_forward() && !is_native() && !is_stock() && !is_static()) {
        if (!is_public()) {
            report(pos_, 245) << sym->name();

            set_is_public();
            return true;
        }

        if (data->node && data->node != data->forward) {
            report(pos_, 21) << name_;
            return false;
        }
        return true;
    }

    if (data->node) {
        if (is_forward() && !data->node->is_public()) {
            report(pos_, 412) << name_;
            return false;
        }
        if (body() && !data->forward) {
            report(pos_, 21) << name_;
            return false;
        }
        return true;
    }

    report(pos(), 21) << sym->name();
    return false;
}

bool
FunctionDecl::Bind(SemaContext& outer_sc)
{
    if (!outer_sc.BindType(pos_, &decl_.type))
        return false;

    // Only named functions get an early symbol in EnterNames.
    if (!sym_)
        sym_ = new symbol(decl_.name, 0, iFUNCTN, sGLOBAL, 0);

    // This may not be set if EnterNames wasn't called (eg not a global).
    if (!sym_->function()->node || body_)
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
        Type* type = outer_sc.cc().types()->find(*this_tag_);

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

        auto decl = new ArgDecl(pos_, outer_sc.cc().atom("this"), typeinfo, sARGUMENT, false,
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
            error(pos(), 42);      // invalid combination of class specifiers.
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

    if (ok && deprecate_) {
        sym_->documentation = new PoolString(deprecate_->chars(), deprecate_->length());
        sym_->deprecated = true;
    }
    return ok;
}

bool
FunctionDecl::BindArgs(SemaContext& sc)
{
    AutoCountErrors errors;

    size_t arg_index = 0;
    for (const auto& var : args_) {
        const auto& typeinfo = var->type();
        symbol* argsym = var->sym();

        AutoErrorPos pos(var->pos());

        if (typeinfo.ident == iVARARGS) {
            /* redimension the argument list, add the entry iVARARGS */
            continue;
        }

        Type* type = sc.cc().types()->find(typeinfo.semantic_tag());
        if (type->isEnumStruct()) {
            if (sym_->native)
                report(var->pos(), 135) << type->name();
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
        argsym->setAddr(static_cast<cell>((arg_index + 3) * sizeof(cell)));
        arg_index++;

        if (typeinfo.ident == iREFARRAY || typeinfo.ident == iARRAY) {
            if (sc.sema()->CheckVarDecl(var) && var->init_rhs())
                fill_arg_defvalue(sc.cc(), var);
        } else {
            Expr* init = var->init_rhs();
            if (init && sc.sema()->CheckExpr(init)) {
                assert(typeinfo.ident == iVARIABLE || typeinfo.ident == iREFERENCE);
                var->set_default_value(new DefaultArg());

                int tag;
                cell val;
                if (!init->EvalConst(&val, &tag)) {
                    error(var->pos(), 8);

                    // Populate to avoid errors.
                    val = 0;
                    tag = typeinfo.tag();
                }
                var->default_value()->tag = tag;
                var->default_value()->val = ke::Some(val);

                matchtag(var->type().tag(), tag, MATCHTAG_COERCE);
            }
        }

        if (var->type().ident == iREFERENCE)
            argsym->usage |= uREAD; /* because references are passed back */
        if (sym_->callback || sym_->stock || sym_->is_public)
            argsym->usage |= uREAD; /* arguments of public functions are always "used" */

        /* arguments of a public function may not have a default value */
        if (sym_->is_public && var->default_value())
            report(var->pos(), 59) << var->name();
    }

    auto forward = sym_->function()->forward;
    auto impl = sym_->function()->node;
    if (!(forward && impl))
        return errors.ok();

    // If we get here, we're a public and forward pair, and we need to compare
    // to make sure the argument lists are compatible.
    assert(sym_->function()->forward);
    assert(sym_->function()->node);
    token_pos_t error_pos = sym_->function()->node->pos();

    size_t fwd_argc = forward->args().size();
    size_t impl_argc = impl->args().size();

    // We allow forwards to omit arguments in their signature, so this is not
    // a straight-up equality test.
    if (this == impl && impl_argc > fwd_argc) {
        report(error_pos, 25);
        return false;
    }

    if (!sym_->function()->checked_one_signature) {
        sym_->function()->checked_one_signature = true;
        return errors.ok();
    }
    if (!sym_->function()->compared_prototype_args) {
        for (size_t i = 0; i < impl_argc; i++) {
            if (!argcompare(impl->args()[i], forward->args()[i]))
                report(error_pos, 181) << impl->args()[i]->name();
        }
        sym_->function()->compared_prototype_args = true;
    }
    return errors.ok();
}

sp::Atom*
FunctionDecl::NameForOperator()
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

        auto type = CompileContext::get().types()->find(var->type().tag());
        params.emplace_back(type->name()->str());
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
    return CompileContext::get().atom(name);
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
    root_->tag = sc.cc().types()->defineEnumStruct(name_->chars(), root_)->tagid();
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

        symbol* child = new symbol(field.decl.name, position, field.decl.type.ident, sGLOBAL,
                                   field.decl.type.semantic_tag());
        if (field.decl.type.numdim()) {
            child->set_dim_count(1);
            child->set_dim(0, field.decl.type.dim[0]);
        }
        child->enumfield = true;
        fields.emplace_back(child);

        cell size = 1;
        if (field.decl.type.numdim()) {
            size = field.decl.type.tag() == sc.cc().types()->tag_string()
                   ? char_array_cells(field.decl.type.dim[0])
                   : field.decl.type.dim[0];
        }
        position += size;
    }

    if (!position)
        report(pos_, 119) << name_;

    std::vector<symbol*> methods;
    for (const auto& decl : methods_) {
        if (seen.count(decl->name())) {
            report(decl->pos(), 103) << decl->name() << "enum struct";
            continue;
        }
        seen.emplace(decl->name());

        auto sym = new symbol(decl->name(), 0, iFUNCTN, sGLOBAL, 0);
        decl->set_sym(sym);
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
        auto inner_name = DecorateInnerName(name_, fun->decl_name());
        if (!inner_name)
            continue;

        fun->sym()->function()->is_member_function = true;

        fun->set_name(inner_name);
        fun->set_this_tag(root_->tag);
        fun->Bind(sc);
    }
    return errors.ok();
}

sp::Atom*
Decl::DecorateInnerName(sp::Atom* parent_name, sp::Atom* field_name)
{
    auto full_name = ke::StringPrintf("%s.%s", parent_name->chars(), field_name->chars());
    return CompileContext::get().atom(full_name);
}

bool
MethodmapDecl::EnterNames(SemaContext& sc)
{
    AutoErrorPos error_pos(pos_);

    if (auto type = sc.cc().types()->find(name_)) {
        if (!type->isEnum()) {
            report(pos_, 432) << name_ << type->kindName();
            return false;
        }
    }

    map_ = methodmap_add(sc.cc(), nullptr, name_);
    sc.cc().types()->defineMethodmap(name_->chars(), map_);

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
        if (map_->methods.count(method->decl->decl_name())) {
            report(method->decl->pos(), 103) << method->decl->decl_name() << "methodmap";
            continue;
        }

        auto m = new methodmap_method_t(map_);
        m->name = method->decl->decl_name();
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

            auto name = ke::StringPrintf("%s.%s.get", name_->chars(), prop->name->chars());
            method->getter->setName(sc.cc().atom(name));
        }
        if (prop->setter && BindSetter(sc, prop)) {
            method->setter = prop->setter->sym();

            auto name = ke::StringPrintf("%s.%s.set", name_->chars(), prop->name->chars());
            method->setter->setName(sc.cc().atom(name));
        }
    }

    for (const auto& method : methods_) {
        bool is_ctor = false;
        if (method->decl->decl_name() == map_->name) {
            // Constructors may not be static.
            if (method->is_static) {
                report(method->decl->pos(), 175);
                continue;
            }
            is_ctor = true;

            auto& type = method->decl->mutable_type();
            type.set_tag(map_->tag);
            type.ident = iVARIABLE;
            type.is_new = true;
        } else if (method->decl->type().ident == 0) {
            // Parsed as a constructor, but not using the map name. This is illegal.
            report(method->decl->pos(), 114) << "constructor" << "methodmap" << map_->name;
            continue;
        }

        if (!method->is_static && !is_ctor)
            method->decl->set_this_tag(map_->tag);

        if (!method->decl->Bind(sc))
            continue;

        method->decl->sym()->function()->is_member_function = true;
        method->decl->sym()->setName(DecorateInnerName(name_, method->decl->decl_name()));

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

    if (!fun->Bind(sc))
        return false;

    fun->sym()->function()->is_member_function = true;
    return true;
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

    fun->sym()->function()->is_member_function = true;

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

SymbolScope* SemaContext::ScopeForAdd() {
    if (!scope_creator_)
        return scope();

    // Note, when lazily creating scopes, the initial parent may be wrong. We
    // do this so scope lookups work as normal. However if we wind up having
    // to lazily create an intervening scope, the parent chain gets fixed up
    // automatically.
    //
    // Once we leave a scope, the hierarchy is never used again, so this fixup
    // is not super important (yet).
    if (scope_ == scope_creator_->prev())
        scope_ = new SymbolScope(scope_, scope_creator_->kind());
    return scope_;
}

AutoCreateScope::AutoCreateScope(SemaContext& sc, ScopeKind kind, SymbolScope** where)
  : sc_(sc),
    kind_(kind),
    where_(where),
    prev_(sc.scope()),
    prev_creator_(sc.scope_creator())
{
    sc.set_scope_creator(this);
}

AutoCreateScope::~AutoCreateScope() {
    if (sc_.scope() == prev_) {
        // We never changed scopes. If there's another lazy scope context, move
        // all the pending scopes (needing a parent) upwards. Otherwise, we'll
        // need to reparent to the top scope.
        if (prev_creator_)
            ke::MoveExtend(&prev_creator_->pending_, &pending_);
    } else {
        *where_ = sc_.scope();

        // Reparent the new scope.
        if (prev_creator_)
            prev_creator_->pending_.emplace_back(sc_.scope());
        else
            sc_.scope()->set_parent(prev_);
    }

    // Reparent our pending list. If we never created a new scope, these get
    // assigned to the previous top scope.
    for (const auto& child : pending_)
        child->set_parent(sc_.scope());

    sc_.set_scope(prev_);
    sc_.set_scope_creator(prev_creator_);
}
