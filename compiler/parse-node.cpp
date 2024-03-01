// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
//  Copyright (c) 2021 AlliedModders LLC
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
#include "parse-node.h"

#include "errors.h"

namespace sp {
namespace cc {

VarDeclBase::VarDeclBase(StmtKind kind, const token_pos_t& pos, Atom* name,
                         const typeinfo_t& type, int vclass, bool is_public, bool is_static,
                         bool is_stock, Expr* initializer)
 : Decl(kind, pos, name),
   type_(type),
   vclass_(vclass),
   is_public_(is_public),
   is_static_(is_static),
   is_stock_(is_stock),
   autozero_(true),
   is_read_(false),
   is_written_(false)
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
    out->push_back(this);
}

void
LogicalExpr::FlattenLogical(int token, std::vector<Expr*>* out)
{
    if (token_ == token) {
        left_->FlattenLogical(token, out);
        right_->FlattenLogical(token, out);
    } else {
        Expr::FlattenLogical(token, out);
    }
}

bool Stmt::IsTerminal() const {
    switch (flow_type()) {
        case Flow_Break:
        case Flow_Continue:
        case Flow_Return:
            return true;
        default:
            return false;
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
    is_analyzing_(false),
    explicit_return_type_(false),
    retvalue_used_(false),
    is_callback_(false),
    returns_value_(false),
    always_returns_(false),
    is_live_(false),
    maybe_used_(false)
{
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

FloatExpr::FloatExpr(CompileContext& cc, const token_pos_t& pos, cell value)
  : TaggedValueExpr(pos, cc.types()->type_float(), value)
{
}

MethodmapDecl* MethodmapDecl::LookupMethodmap(Decl* decl) {
    if (auto mm = decl->as<MethodmapDecl>())
        return mm;
    if (auto ed = decl->as<EnumDecl>())
        return ed->mm();
    return nullptr;
}

Decl* MethodmapDecl::FindMember(Atom* name) const {
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

Type* MethodmapPropertyDecl::property_type() const {
    auto types = CompileContext::get().types();

    if (getter_)
        return getter_->type_info().type;
    if (setter_->args().size() != 2)
        return types->type_void();
    ArgDecl* valp = setter_->args()[1];
    return valp->type();
}

cell Decl::ConstVal() {
    if (auto cv = as<ConstDecl>())
        return cv->const_val();
    else if (auto efd = as<EnumFieldDecl>())
        return efd->const_val();

    assert(false);
    return 0;
}

Type* Decl::type() const {
    assert(false);
    return nullptr;
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

IdentifierKind Decl::ident() {
    return ident_impl();
}

IdentifierKind Decl::ident_impl() {
    switch (kind()) {
        case StmtKind::ArgDecl:
        case StmtKind::VarDecl:
            return iVARIABLE;
        case StmtKind::ConstDecl:
        case StmtKind::EnumFieldDecl:
            return iCONSTEXPR;
        case StmtKind::FunctionDecl:
        case StmtKind::MemberFunctionDecl:
        case StmtKind::MethodmapMethodDecl:
            return iFUNCTN;
        case StmtKind::EnumStructDecl:
        case StmtKind::MethodmapDecl:
            return iTYPENAME;
        case StmtKind::EnumDecl: {
            auto es = as<EnumDecl>();
            if (es->mm())
                return iTYPENAME;
            return iCONSTEXPR;
        }
        default:
            assert(false);
            return iINVALID;
    }
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
