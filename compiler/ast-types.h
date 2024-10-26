// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
//  Copyright (c) AlliedModders 2021
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
#pragma once

#include <stdint.h>

#define AST_STMT_TYPE_LIST(FOR_EACH) \
    FOR_EACH(StmtList) \
    FOR_EACH(BlockStmt) \
    FOR_EACH(BreakStmt) \
    FOR_EACH(ContinueStmt) \
    FOR_EACH(StaticAssertStmt) \
    FOR_EACH(VarDecl) \
    FOR_EACH(ArgDecl) \
    FOR_EACH(ConstDecl) \
    FOR_EACH(EnumDecl) \
    FOR_EACH(EnumFieldDecl) \
    FOR_EACH(PstructDecl) \
    FOR_EACH(TypedefDecl) \
    FOR_EACH(TypesetDecl) \
    FOR_EACH(IfStmt) \
    FOR_EACH(ExprStmt) \
    FOR_EACH(ReturnStmt) \
    FOR_EACH(AssertStmt) \
    FOR_EACH(DeleteStmt) \
    FOR_EACH(ExitStmt) \
    FOR_EACH(DoWhileStmt) \
    FOR_EACH(ForStmt) \
    FOR_EACH(SwitchStmt) \
    FOR_EACH(PragmaUnusedStmt) \
    FOR_EACH(FunctionDecl) \
    FOR_EACH(MemberFunctionDecl) \
    FOR_EACH(EnumStructDecl) \
    FOR_EACH(LayoutFieldDecl) \
    FOR_EACH(MethodmapDecl) \
    FOR_EACH(ChangeScopeNode) \
    FOR_EACH(MethodmapPropertyDecl) \
    FOR_EACH(MethodmapMethodDecl)

#define AST_EXPR_TYPE_LIST(FOR_EACH) \
    FOR_EACH(UnaryExpr) \
    FOR_EACH(BinaryExpr) \
    FOR_EACH(LogicalExpr) \
    FOR_EACH(ChainedCompareExpr) \
    FOR_EACH(TernaryExpr) \
    FOR_EACH(IncDecExpr) \
    FOR_EACH(CastExpr) \
    FOR_EACH(SizeofExpr) \
    FOR_EACH(SymbolExpr) \
    FOR_EACH(CallExpr) \
    FOR_EACH(NamedArgExpr) \
    FOR_EACH(CallUserOpExpr) \
    FOR_EACH(DefaultArgExpr) \
    FOR_EACH(FieldAccessExpr) \
    FOR_EACH(IndexExpr) \
    FOR_EACH(RvalueExpr) \
    FOR_EACH(CommaExpr) \
    FOR_EACH(ThisExpr) \
    FOR_EACH(NullExpr) \
    FOR_EACH(TaggedValueExpr) \
    FOR_EACH(StringExpr) \
    FOR_EACH(NewArrayExpr) \
    FOR_EACH(ArrayExpr) \
    FOR_EACH(StructExpr) \
    FOR_EACH(StructInitFieldExpr)

#define IR_NODE_TYPE_LIST(FOR_EACH) \
    /* Decls */ \
    FOR_EACH(Function) \
    FOR_EACH(Variable) \
    FOR_EACH(Argument) \
    /* Statements */ \
    FOR_EACH(Return) \
    FOR_EACH(ValueInsn) \
    FOR_EACH(Exit) \
    FOR_EACH(Break) \
    FOR_EACH(Continue) \
    FOR_EACH(Assert) \
    FOR_EACH(If) \
    FOR_EACH(DoWhile) \
    FOR_EACH(Delete) \
    FOR_EACH(ForLoop) \
    FOR_EACH(Switch) \
    FOR_EACH(FunctionDef) \
    /* Values */ \
    FOR_EACH(ConstVal) \
    FOR_EACH(CharArrayLiteral) \
    FOR_EACH(VariableRef) \
    FOR_EACH(TypeRef) \
    FOR_EACH(FunctionRef) \
    FOR_EACH(IndexOp) \
    FOR_EACH(Load) \
    FOR_EACH(TernaryOp) \
    FOR_EACH(BinaryOp) \
    FOR_EACH(Array) \
    FOR_EACH(CommaOp) \
    FOR_EACH(CallOp) \
    FOR_EACH(PropertyRef) \
    FOR_EACH(FieldRef) \
    FOR_EACH(UnaryOp) \
    FOR_EACH(CallUserOp) \
    FOR_EACH(Store) \
    FOR_EACH(StackRef) \
    FOR_EACH(AddressOf) \
    FOR_EACH(ArrayInitializer) \
    FOR_EACH(StoreWithTemp) \
    FOR_EACH(IncDec) \
    FOR_EACH(IncDecWithTemp) \
    FOR_EACH(TempValueRef) \
    FOR_EACH(BoundFunction) \
    FOR_EACH(CastOp)

namespace sp {
namespace cc {

enum class ExprKind : uint8_t
{
#define _(Name) Name,
    AST_EXPR_TYPE_LIST(_)
#undef _
};

enum class StmtKind : uint8_t
{
#define _(Name) Name,
    AST_STMT_TYPE_LIST(_)
#undef _
};

enum class IrKind : uint8_t
{
#define _(Name) Name,
    IR_NODE_TYPE_LIST(_)
#undef _
};

} // namespace cc
} // namespace sp
