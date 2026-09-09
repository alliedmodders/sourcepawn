// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2021-2026 AlliedModders LLC
//
#include "parse-node.h"

#include <errno.h>
#include <stdlib.h>

#include <unordered_map>

#include "compile-context.h"
#include "errors.h"
#include "ir-node.h"

namespace sp {
namespace cc {

VarDeclBase::VarDeclBase(StmtKind kind, const token_pos_t& pos, Atom* name,
                         const typeinfo_t& type, int vclass, VarDeclFlags flags, Expr* initializer)
  : Decl(kind, pos, name),
    type_(type),
    vclass_(vclass),
    is_public_((flags & VARDECL_PUBLIC) == VARDECL_PUBLIC),
    is_static_((flags & VARDECL_STATIC) == VARDECL_STATIC),
    is_stock_((flags & VARDECL_STOCK) == VARDECL_STOCK),
    autozero_(true),
    is_read_(false),
    is_written_(false),
    implicit_dynamic_array_(false),
    is_shared_((flags & VARDECL_SHARED) == VARDECL_SHARED),
    already_bound_(false),
    is_emitted_(false)
{
    // Having a BinaryExpr allows us to re-use assignment logic.
    if (initializer)
        set_init(initializer);
}

void VarDeclBase::set_init(Expr* expr) {
    init_ = new BinaryExpr(pos(), '=', new SymbolExpr(pos(), name()), expr);
    init_->set_initializer();
}

Expr* VarDeclBase::init_rhs() const {
    if (!init_)
        return nullptr;
    return init_->right();
}

void VarDeclBase::BindAddress(cell addr) {
    addr_.bind(addr);
}

void
ParseNode::error(const token_pos_t& pos, int number)
{
    report(pos, number);
}

void
Expr::FlattenLogical(int token, std::vector<Expr*>* out)
{
    if (kind_ == ExprKind::LogicalExpr) {
        to<LogicalExpr>()->FlattenLogical(token, out);
    } else {
        out->push_back(this);
    }
}

void
LogicalExpr::FlattenLogical(int token, std::vector<Expr*>* out)
{
    if (token_ == token) {
        left_->FlattenLogical(token, out);
        right_->FlattenLogical(token, out);
    } else {
        out->push_back(this);
    }
}

BlockStmt*
BlockStmt::WrapStmt(Stmt* stmt)
{
    if (BlockStmt* block = stmt->as<BlockStmt>())
        return block;

    std::vector<Stmt*> stmts = {stmt};
    return new BlockStmt(stmt->pos(), stmts);
}

BinaryExprBase::BinaryExprBase(ExprKind kind, const token_pos_t& pos, int token, Expr* left, Expr* right)
  : Expr(kind, pos),
    token_(token),
    left_(left),
    right_(right)
{
    assert(right_ != this);
}

FunctionDecl::FunctionDecl(StmtKind kind, const token_pos_t& pos, const declinfo_t& decl)
  : Decl(kind, pos, decl.name),
    decl_(decl),
    analyzed_(false),
    analyze_result_(false),
    is_public_(false),
    is_static_(false),
    is_stock_(false),
    is_forward_(false),
    is_native_(false),
    is_builtin_(false),
    is_analyzing_(false),
    explicit_return_type_(false),
    retvalue_used_(false),
    is_callback_(false),
    returns_value_(false),
    is_live_(false),
    is_global_ctor_(false),
    maybe_used_(false)
{
}

void FunctionDecl::update_return_type(Type* type) {
    signature_ = CompileContext::get().types()->UpdateReturnType(signature_, QualType(type));
}

int FunctionDecl::FindNamedArg(Atom* name) const {
    for (size_t i = 0; i < args_.size() && !args_[i]->type_info().is_varargs; i++) {
        if (args_[i]->name() == name)
            return (int)i;
    }
    return -1;
}

FunctionDecl* FunctionDecl::prototype() {
    if (!proto_or_impl_)
        return this;
    if (!body_)
        return this;
    return proto_or_impl_;
}

FunctionDecl* FunctionDecl::impl() {
    if (body_)
        return this;
    return proto_or_impl_;
}

FunctionDecl* FunctionDecl::canonical() {
    if (body_ || !proto_or_impl_)
        return this;
    return proto_or_impl_;
}

bool FunctionDecl::IsVariadic() {
    return !args_.empty() && args_.back()->type_info().is_varargs;
}

bool FunctionDecl::MustReturnValue() const {
    return retvalue_used_ || (explicit_return_type_ && !return_type()->isVoid());
}

void FunctionDecl::AddReferenceTo(FunctionDecl* other) {
    if (!refers_to_) {
        auto& cc = CompileContext::get();
        refers_to_ = cc.allocator().alloc<PoolForwardList<FunctionDecl*>>();
    }
    for (FunctionDecl* decl : *refers_to_) {
        if (decl == other)
            return;
    }
    refers_to_->emplace_front(other);
}

auto FunctionDecl::cg() -> CGInfo* {
    if (!cg_)
        cg_ = new CGInfo();
    return cg_;
}

void FunctionDecl::AddSharedVar(VarDeclBase* var) {
    if (var->is_captured())
        return;

    shared_var_list_.push_back(var);
    var->set_is_captured();
}

UpvarDecl* FunctionDecl::AddUpvar(const token_pos_t& pos, FunctionDecl* owner, VarDeclBase* var) {
    if (auto iter = upvar_decls_.find(var); iter != upvar_decls_.end())
        return iter->second;

    auto upvar_decl = new UpvarDecl(pos, var, owner);
    upvar_decls_.emplace(var, upvar_decl);

    if (!var->is_shared()) {
        uint16_t index = static_cast<uint16_t>(upvars_.size());
        upvar_decl->set_upvar_index(index);
        upvars_.push_back(upvar_decl);

        // If a non-shared upvar crosses any intermediate closures, we need to propagate it
        // through each intermediate frame, otherwise there is no way to get the value.
        for (FunctionDecl* iter = outer_; iter != nullptr && iter != owner; iter = iter->outer_)
            iter->AddUpvar(pos, owner, var);
    }

    var->set_is_captured();
    return upvar_decl;
}

LayoutFieldDecl* FunctionDecl::GetSharedVarField(VarDeclBase* var) {
    auto iter = shared_vars_.find(var);
    assert(iter != shared_vars_.end());
    return iter->second;
}

UpvarDecl* FunctionDecl::FindUpvarDecl(VarDeclBase* var) const {
    if (auto iter = upvar_decls_.find(var); iter != upvar_decls_.end())
        return iter->second;
    return nullptr;
}

FunctionType* CallExpr::callee_type() {
    if (auto p = std::get_if<FunctionType*>(&resolved_target_))
        return *p;
    if (auto p = std::get_if<FunctionDecl*>(&resolved_target_))
        return (*p)->signature();
    return nullptr;
}

MethodmapDecl* MethodmapDecl::LookupMethodmap(Decl* decl) {
    if (auto mm = decl->as<MethodmapDecl>())
        return mm;
    if (auto ed = decl->as<EnumDecl>())
        return ed->mm();
    return nullptr;
}

Decl* LayoutDecl::FindMember(Atom* name) {
    switch (kind()) {
        case StmtKind::MethodmapDecl:
            return to<MethodmapDecl>()->FindMember(name);
        case StmtKind::ClassDecl:
            return to<ClassDecl>()->FindMember(name);
        default:
            return nullptr;
    }
}

Decl* MethodmapDecl::FindMember(Atom* name) {
    for (const auto& prop : properties_) {
        if (prop->name() == name)
            return prop;
    }
    for (const auto& method : methods_) {
        if (method->decl_name() == name)
            return method;
    }
    if (parent_)
        return parent_->FindMember(name);
    return nullptr;
}

Decl* ClassDecl::FindMember(Atom* name) {
    for (const auto& prop : properties_) {
        if (prop->name() == name)
            return prop;
    }
    for (const auto& method : methods_) {
        if (method->decl_name() == name)
            return method;
    }
    return nullptr;
}

Type* PropertyDecl::property_type() const {
    auto types = CompileContext::get().types();

    if (getter_)
        return getter_->type_info().type;
    if (!setter_ || setter_->args().size() != 2)
        return types->type_void();
    ArgDecl* valp = setter_->args()[1];
    return *valp->type();
}

ConstVal Decl::const_value() {
    if (auto cv = as<ConstDecl>())
        return cv->value();
    if (auto efd = as<EnumFieldDecl>())
        return ConstVal(efd->type().unqualified(), efd->const_val());

    assert(false);
    return ConstVal(nullptr, 0);
}

QualType Decl::type() {
    switch (kind()) {
        case StmtKind::VarDecl:
        case StmtKind::ArgDecl:
        case StmtKind::ConstDecl:
            return to<VarDeclBase>()->type();
        case StmtKind::EnumFieldDecl:
            return to<EnumFieldDecl>()->type();
        case StmtKind::EnumDecl:
            return to<EnumDecl>()->type();
        case StmtKind::FunctionDecl:
        case StmtKind::MemberFunctionDecl:
            return to<FunctionDecl>()->type();
        case StmtKind::ClassDecl:
            return to<ClassDecl>()->type();
        case StmtKind::LayoutFieldDecl:
            return to<LayoutFieldDecl>()->type();
        case StmtKind::EnumStructDecl:
            return to<EnumStructDecl>()->type();
        case StmtKind::PropertyDecl:
            return to<PropertyDecl>()->type();
        case StmtKind::MethodmapDecl:
            return to<MethodmapDecl>()->type();
        case StmtKind::UpvarDecl:
            return to<UpvarDecl>()->type();
        default:
            assert(false);
            return QualType(nullptr);
    }
}

bool Decl::is_const() {
    auto var = as<VarDeclBase>();
    assert(var);

    return (int)var->type_info().is_const;
}

char Decl::vclass() {
    if (auto var = as<VarDeclBase>()) {
        return var->vclass();
    } else if (auto fun = as<FunctionDecl>()) {
        return fun->is_static() ? sSTATIC : sGLOBAL;
    }
    assert(false);
    return 0;
}

PstructDecl::PstructDecl(const token_pos_t& pos, Atom* name, const std::vector<LayoutFieldDecl*>& fields)
  : Decl(StmtKind::PstructDecl, pos, name),
    fields_(fields)
{
    for (auto field : fields_)
        field->set_parent(this);
}

LayoutFieldDecl* PstructDecl::FindField(Atom* name) {
    for (const auto& field : fields_) {
        if (field->name() == name)
            return field;
    }
    return nullptr;
}

} // namespace cc
} // namespace sp
