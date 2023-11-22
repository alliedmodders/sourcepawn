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
#include "scopes.h"
#include "sctracker.h"
#include "semantics.h"
#include "symbols.h"

namespace sp {

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
        ti->dim.emplace_back(enum_type->array_size());

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
SemaContext::BindType(const token_pos_t& pos, Atom* atom, bool is_label, int* tag)
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

bool EnumDecl::EnterNames(SemaContext& sc) {
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

    tag_ = tag;

    symbol* enumsym = nullptr;
    if (name_) {
        if (vclass_ == sGLOBAL) {
            if (auto decl = FindSymbol(sc, name_)) {
                // If we were previously defined as a methodmap, don't overwrite the
                // symbol. Otherwise, flow into DefineConstant where we will error.
                if (auto mm = decl->as<MethodmapDecl>())
                    enumsym = mm->sym();
            }
        }

        if (!enumsym) {
            // create the root symbol, so the fields can have it as their "parent"
            enumsym = DefineConstant(sc, this, pos_, vclass_);
        }
    }

    // If this enum is for a methodmap, forget the symbol so code below doesn't
    // build an enum struct.
    if (enumsym && enumsym->ident() == iMETHODMAP)
        enumsym = NULL;

    cell value = 0;
    for (const auto& field : fields_ ) {
        AutoErrorPos error_pos(field->pos());

        if (field->value() && field->value()->Bind(sc) && sc.sema()->CheckExpr(field->value())) {
            int field_tag;
            if (field->value()->EvalConst(&value, &field_tag))
                matchtag(tag, field_tag, MATCHTAG_COERCE | MATCHTAG_ENUM_ASSN);
            else
                error(field->pos(), 80);
        }

        field->set_tag(tag);
        field->set_addr(value);

        symbol* sym = DefineConstant(sc, field, field->pos(), vclass_);
        if (!sym)
            continue;

        if (multiplier_ == 1)
            value += increment_;
        else
            value *= increment_ * multiplier_;
        field->set_sym(sym);
    }

    // set the enum name to the "next" value (typically the last value plus one)
    if (enumsym) {
        addr_ = value;
        array_size_ = value;
    }

    sym_ = enumsym;
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
        report(pos_, 109) << "struct";
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
    sym_ = DefineConstant(sc, this, pos_, vclass_);
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

    addr_ = value;
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
        sym_ = new symbol(iVARIABLE, sGLOBAL);
        sym_->set_is_const(true);
    } else {
        IdentifierKind ident = type_.ident;
        if (vclass_ == sARGUMENT && ident == iARRAY)
            type_.ident = ident = iREFARRAY;

        sym_ = NewVariable(this, ident, vclass_);

        if (ident == iVARARGS)
            markusage(this, uREAD);

        sym_->set_is_const(type_.is_const);
    }

    if (is_public_)
        is_read_ = true;

    if (def_ok)
        DefineSymbol(sc, this, vclass_);

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

    decl_ = FindSymbol(sc, name_);
    if (!decl_) {
        report(pos_, 17) << name_;
        return false;
    }

    if (auto fun = decl_->as<FunctionDecl>())
        decl_ = fun->canonical();

    if (decl_ && !is_lval)
        markusage(decl_, uREAD);
    return true;
}

bool ThisExpr::Bind(SemaContext& sc) {
    AutoErrorPos aep(pos_);

    if (auto decl = FindSymbol(sc, sc.cc().atom("this")))
        decl_ = decl->as<VarDeclBase>();

    if (!decl_) {
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

bool SizeofExpr::Bind(SemaContext& sc) {
    AutoErrorPos aep(pos_);

    decl_ = FindSymbol(sc, ident_);
    if (!decl_) {
        report(pos_, 17) << ident_;
        return false;
    }
    markusage(decl_, uREAD);
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
        if (auto decl = sym_expr->decl()) {
            if (auto var = decl->as<VarDeclBase>()) {
                if (var->type().ident == iARRAY || var->type().ident == iREFARRAY)
                    sc.func()->set_maybe_returns_array();
            }
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

bool FunctionDecl::EnterNames(SemaContext& sc) {
    FunctionDecl* other = nullptr;
    if (!decl_.opertok) {
        // Handle forwards.
        Decl* found = FindSymbol(sc, name_);
        if (found) {
            if ((other = CanRedefine(found)) == nullptr)
                return false;
            assert(!other || !other->proto_or_impl_);
        }
    }

    if (other) {
        sym_ = other->sym();
        proto_or_impl_ = other;
        other->proto_or_impl_ = this;
    } else {
        auto scope = is_static() ? sSTATIC : sGLOBAL;
        sym_ = new symbol(iFUNCTN, scope);

        DefineSymbol(sc, this, scope);
    }
    return true;
}

FunctionDecl* FunctionDecl::CanRedefine(Decl* other_decl) {
    FunctionDecl* fun = other_decl->as<FunctionDecl>();
    if (!fun) {
        report(pos_, 21) << name_;
        return nullptr;
    }

    if (fun->is_forward() && !is_forward() && !is_native() && !is_stock() && !is_static()) {
        if (!is_public()) {
            report(pos_, 245) << name_;

            set_is_public();
            return fun;
        }

        if (fun->impl()) {
            report(pos_, 21) << name_;
            return nullptr;
        }
        return fun;
    }

    if (fun->body()) {
        if (is_forward() && !fun->is_public()) {
            report(pos_, 412) << name_;
            return nullptr;
        }
        if (body()) {
            report(pos_, 21) << name_;
            return nullptr;
        }
        return fun;
    }

    report(pos(), 21) << name_;
    return nullptr;
}

bool FunctionDecl::Bind(SemaContext& outer_sc) {
    if (!outer_sc.BindType(pos_, &decl_.type))
        return false;

    // Only named functions get an early symbol in EnterNames.
    if (!sym_)
        sym_ = new symbol(iFUNCTN, sGLOBAL);

    // The forward's prototype is canonical. If this symbol has a forward, we
    // don't set or override the return type when we see the public
    // implementation. Note that args are checked similarly in BindArgs.
    if (!prototype()->is_forward() || is_forward_)
        explicit_return_type_ = decl_.type.is_new;

    // Ensure |this| argument exists.
    if (this_tag_) {
        Type* type = outer_sc.cc().types()->find(*this_tag_);

        typeinfo_t typeinfo = {};
        if (auto enum_type = type->asEnumStruct()) {
            typeinfo.set_tag(0);
            typeinfo.ident = iREFARRAY;
            typeinfo.declared_tag = *this_tag_;
            typeinfo.dim.emplace_back(enum_type->array_size());
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

    SemaContext sc(outer_sc, this);
    auto restore_sc = ke::MakeScopeGuard([&outer_sc]() {
        outer_sc.sema()->set_context(&outer_sc);
    });
    sc.sema()->set_context(&sc);

    if (name_->str() == uMAINFUNC) {
        if (!args_.empty())
            error(pos_, 5);     /* "main()" functions may not have any arguments */
        is_live_ = true;
        is_public_ = true;
    }

    if (is_native_)
        addr_ = -1;

    ke::Maybe<AutoEnterScope> enter_scope;
    if (!args_.empty()) {
        enter_scope.init(sc, sARGUMENT);
        scope_ = sc.scope();

        for (const auto& arg : args_)
            ok &= arg->Bind(sc);

        if (this_tag_ && ok)
            markusage(args_[0], uREAD);
    }

    if ((is_native_ || is_public_ || is_forward_) && decl_.type.numdim() > 0)
        error(pos_, 141);

    ok &= BindArgs(sc);

    if (body_)
        ok &= body_->Bind(sc);
    return ok;
}

bool
FunctionDecl::BindArgs(SemaContext& sc)
{
    AutoCountErrors errors;

    size_t arg_index = 0;
    for (auto& var : args_) {
        const auto& typeinfo = var->type();

        AutoErrorPos pos(var->pos());

        if (typeinfo.ident == iVARARGS) {
            /* redimension the argument list, add the entry iVARARGS */
            var->set_addr(static_cast<cell>((arg_index + 3) * sizeof(cell)));
            break;
        }

        Type* type = sc.cc().types()->find(typeinfo.semantic_tag());
        if (type->isEnumStruct()) {
            if (is_native_)
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
        var->set_addr(static_cast<cell>((arg_index + 3) * sizeof(cell)));
        arg_index++;

        if (typeinfo.ident == iREFARRAY || typeinfo.ident == iARRAY) {
            if (sc.sema()->CheckVarDecl(var) && var->init_rhs())
                fill_arg_defvalue(sc.cc(), var);
        } else {
            Expr* init = var->init_rhs();
            if (init && sc.sema()->CheckExpr(init)) {
                AutoErrorPos pos(init->pos());

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
            var->set_is_read();
        if (is_callback_ || is_stock_ || is_public_)
            var->set_is_read();

        /* arguments of a public function may not have a default value */
        if (is_public_ && var->default_value())
            report(var->pos(), 59) << var->name();
    }

    if (!proto_or_impl_)
        return errors.ok();

    // If we get here, we're a public and forward pair, and we need to compare
    // to make sure the argument lists are compatible.
    token_pos_t error_pos = impl()->pos();

    size_t fwd_argc = prototype()->args().size();
    size_t impl_argc = impl()->args().size();

    // We allow forwards to omit arguments in their signature, so this is not
    // a straight-up equality test.
    if (this == impl() && impl_argc > fwd_argc) {
        report(error_pos, 25);
        return false;
    }

    if (!canonical()->checked_one_signature) {
        canonical()->checked_one_signature = true;
        return errors.ok();
    }
    if (!canonical()->compared_prototype_args) {
        auto impl_fun = impl();
        auto proto_fun = prototype();
        for (size_t i = 0; i < impl_argc; i++) {
            if (!argcompare(impl_fun->args()[i], proto_fun->args()[i]))
                report(error_pos, 181) << impl_fun->args()[i]->name();
        }
        canonical()->compared_prototype_args = true;
    }
    return errors.ok();
}

Atom*
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
    std::vector<VarDeclBase*> symbols;
    for (const auto& name : names_) {
        auto decl = FindSymbol(sc, name);
        if (!decl || !decl->as<VarDeclBase>()) {
            report(pos_, 17) << name;
            continue;
        }
        symbols.emplace_back(decl->as<VarDeclBase>());
    }

    symbols_ = PoolArray<VarDeclBase*>(symbols);

    return names_.size() == symbols_.size();
}

bool EnumStructDecl::EnterNames(SemaContext& sc) {
    AutoCountErrors errors;

    tag_ = sc.cc().types()->defineEnumStruct(name_, this)->tagid();

    AutoErrorPos error_pos(pos_);
    root_ = DefineConstant(sc, this, pos_, sGLOBAL);
    root_->set_ident(iENUMSTRUCT);

    std::unordered_set<Atom*> seen;

    cell position = 0;
    for (auto& field : fields_) {
        if (!sc.BindType(field->pos(), &field->mutable_type()))
            continue;

        // It's not possible to have circular references other than this, because
        // Pawn is inherently forward-pass only.
        //
        // :TODO: this will not be true when we move to recursive binding.
        if (field->type().semantic_tag() == tag_) {
            report(field->pos(), 87) << name_;
            continue;
        }

        if (field->type().is_const)
            report(field->pos(), 94) << field->name();

        if (field->type().numdim()) {
            if (field->type().ident == iARRAY) {
                ResolveArraySize(sc.sema(), field->pos(), &field->mutable_type(), sENUMFIELD);

                if (field->type().numdim() > 1) {
                    error(field->pos(), 65);
                    continue;
                }
            } else {
                error(field->pos(), 81);
                continue;
            }
        }

        if (seen.count(field->name())) {
            report(field->pos(), 103) << field->name() << "enum struct";
            continue;
        }
        seen.emplace(field->name());

        symbol* child = new symbol(field->type().ident, sGLOBAL);
        field->set_offset(position);
        field->set_sym(child);

        cell size = 1;
        if (field->type().numdim()) {
            size = field->type().tag() == sc.cc().types()->tag_string()
                   ? char_array_cells(field->type().dim[0])
                   : field->type().dim[0];
        }
        position += size;
    }

    if (!position)
        report(pos_, 119) << name_;

    for (const auto& decl : methods_) {
        if (seen.count(decl->name())) {
            report(decl->pos(), 103) << decl->name() << "enum struct";
            continue;
        }
        seen.emplace(decl->name());

        auto sym = new symbol(iFUNCTN, sGLOBAL);
        decl->set_sym(sym);
    }

    addr_ = position;
    array_size_ = position;

    return errors.ok();
}

bool EnumStructDecl::Bind(SemaContext& sc) {
    if (!root_)
        return false;

    AutoCountErrors errors;
    for (const auto& fun : methods_) {
        auto inner_name = DecorateInnerName(name_, fun->decl_name());
        if (!inner_name)
            continue;

        fun->set_name(inner_name);
        fun->set_this_tag(tag_);
        fun->Bind(sc);
    }
    return errors.ok();
}

Atom*
Decl::DecorateInnerName(Atom* parent_name, Atom* field_name)
{
    auto full_name = ke::StringPrintf("%s.%s", parent_name->chars(), field_name->chars());
    return CompileContext::get().atom(full_name);
}

bool MethodmapDecl::EnterNames(SemaContext& sc) {
    AutoErrorPos error_pos(pos_);

    auto& cc = sc.cc();
    if (auto type = cc.types()->find(name_)) {
        if (!type->isEnum()) {
            report(pos_, 432) << name_ << type->kindName();
            return false;
        }
    }

    tag_ = cc.types()->defineMethodmap(name_, this)->tagid();

    if (auto prev_decl = FindSymbol(cc.globals(), name_)) {
        auto ed = prev_decl->as<EnumDecl>();
        if (!ed) {
            report(pos_, 11) << name_;
            return false;
        }
        if (ed->mm()) {
            report(pos_, 443) << name_;
            return false;
        }

        sym_ = ed->sym();
        sym_->set_ident(iMETHODMAP);
        ed->set_mm(this);
    } else {
        sym_ = new symbol(iMETHODMAP, sGLOBAL);
        cc.globals()->Add(this);
    }

    std::unordered_map<Atom*, Decl*> names;
    for (auto& prop : properties_) {
        if (names.count(prop->name())) {
            report(prop, 103) << prop->name() << "methodmap";
            continue;
        }
        names.emplace(prop->name(), prop);
    }

    for (auto& method : methods_) {
        if (names.count(method->decl_name())) {
            report(method->pos(), 103) << method->decl_name() << "methodmap";
            continue;
        }
        names.emplace(method->decl_name(), method);

        if (method->is_dtor()) {
            if (dtor_) {
                report(method, 154) << method->name();
                continue;
            }

            if (method->decl_name() != name_)
                report(method, 440);

            // Hack: modify name.
            method->decl().name = cc.atom("~" + name_->str());

            dtor_ = method;
        } else if (method->is_ctor()) {
            if (ctor_) {
                report(method, 113) << method->name();
                continue;
            }
            ctor_ = method;
        }
    }
    return true;
}

bool MethodmapDecl::Bind(SemaContext& sc) {
    AutoCountErrors errors;

    is_bound_ = true;

    auto& cc = sc.cc();
    if (extends_) {
        auto parent = FindSymbol(cc.globals(), extends_);
        parent_ = MethodmapDecl::LookupMethodmap(parent);
        if (!parent_)
            report(pos_, 102) << "methodmap" << extends_;
    }

    if (parent_) {
        if (!parent_->is_bound())
            report(pos_, 409) << extends_;

        for (auto iter = parent_; iter; iter = iter->parent_) {
            if (iter == this) {
                parent_ = nullptr;
                report(pos_, 410);
            }
        }
    }

    if (parent_)
        nullable_ = parent_->nullable();

    for (const auto& prop : properties_) {
        if (!sc.BindType(prop->pos(), &prop->mutable_type()))
            return false;

        if (prop->type().numdim() > 0) {
            report(prop, 82);
            continue;
        }

        if (prop->getter() && BindGetter(sc, prop)) {
            auto name = ke::StringPrintf("%s.%s.get", name_->chars(), prop->name()->chars());
            prop->getter()->set_name(sc.cc().atom(name));
        }
        if (prop->setter() && BindSetter(sc, prop)) {
            auto name = ke::StringPrintf("%s.%s.set", name_->chars(), prop->name()->chars());
            prop->setter()->set_name(sc.cc().atom(name));
        }
    }

    for (const auto& method : methods_) {
        if (method->is_ctor()) {
            // Constructors may not be static.
            if (method->is_static())
                report(method, 175);

            auto& type = method->mutable_type();
            type.set_tag(tag_);
            type.ident = iVARIABLE;
            type.is_new = true;
        } else if (method->is_dtor()) {
            if (method->is_static())
                report(method, 441);
            if (method->args().size() > 1)
                report(method, 438);
            if (!method->is_native())
                report(method, 118);

            auto& type = method->mutable_type();
            if (type.ident != 0)
                report(method, 439);
            type.set_tag(sc.cc().types()->tag_void());
            type.is_new = true;
            type.ident = iVARIABLE;
        } else if (method->type().ident == 0) {
            // Parsed as a constructor, but not using the map name. This is illegal.
            report(method, 114) << "constructor" << "methodmap" << name_;
            continue;
        }

        if (!method->is_static() && !method->is_ctor())
            method->set_this_tag(tag_);

        if (!method->Bind(sc))
            continue;

        method->set_name(DecorateInnerName(name_, method->decl_name()));
    }
    return errors.ok();
}

bool MethodmapDecl::BindGetter(SemaContext& sc, MethodmapPropertyDecl* prop) {
    auto fun = prop->getter();

    // There should be no extra arguments.
    if (fun->args().size() > 1) {
        report(fun->pos(), 127);
        return false;
    }

    fun->set_this_tag(tag_);

    if (!fun->Bind(sc))
        return false;
    return true;
}

bool MethodmapDecl::BindSetter(SemaContext& sc, MethodmapPropertyDecl* prop) {
    auto fun = prop->setter();

    // Must have one extra argument taking the return type.
    if (fun->args().size() > 2) {
        report(prop, 150) << pc_tagname(prop->type().tag());
        return false;
    }

    fun->set_this_tag(tag_);

    if (!fun->Bind(sc))
        return false;

    if (fun->args().size() <= 1) {
        report(prop, 150) << pc_tagname(prop->type().tag());
        return false;
    }

    auto decl = fun->args()[1];
    if (decl->type().ident != iVARIABLE || decl->init_rhs() ||
        decl->type().tag() != prop->type().tag())
    {
        report(prop, 150) << pc_tagname(prop->type().tag());
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

} // namespace sp
