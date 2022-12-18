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

VarDeclBase::VarDeclBase(StmtKind kind, const token_pos_t& pos, sp::Atom* name,
                         const typeinfo_t& type, int vclass, bool is_public, bool is_static,
                         bool is_stock, Expr* initializer)
 : Decl(kind, pos, name),
   type_(type),
   vclass_(vclass),
   is_public_(is_public),
   is_static_(is_static),
   is_stock_(is_stock),
   autozero_(true)
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

void
ParseNode::error(const token_pos_t& pos, int number, ...)
{
    va_list ap;
    va_start(ap, number);
    error_va(pos, number, ap);
    va_end(ap);
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

FunctionDecl::FunctionDecl(const token_pos_t& pos, const declinfo_t& decl)
  : Decl(StmtKind::FunctionDecl, pos, decl.name),
    decl_(decl),
    analyzed_(false),
    analyze_result_(false),
    is_public_(false),
    is_static_(false),
    is_stock_(false),
    is_forward_(false),
    is_native_(false),
    is_analyzing_(false),
    maybe_returns_array_(false)
{
}

int FunctionDecl::FindNamedArg(sp::Atom* name) const {
    for (size_t i = 0; i < args_.size() && args_[i]->type().ident != iVARARGS; i++) {
        if (args_[i]->name() == name)
            return (int)i;
    }
    return -1;
}

FloatExpr::FloatExpr(CompileContext& cc, const token_pos_t& pos, cell value)
  : TaggedValueExpr(pos, cc.types()->tag_float(), value)
{
}
