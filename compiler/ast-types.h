// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2021-2026 AlliedModders LLC
//
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
    FOR_EACH(DeleteStmt) \
    FOR_EACH(DoWhileStmt) \
    FOR_EACH(ForStmt) \
    FOR_EACH(SwitchStmt) \
    FOR_EACH(PragmaUnusedStmt) \
    FOR_EACH(FunctionDecl) \
    FOR_EACH(MemberFunctionDecl) \
    FOR_EACH(LayoutMemberDecl) \
    FOR_EACH(EnumStructDecl) \
    FOR_EACH(ClassDecl) \
    FOR_EACH(LayoutFieldDecl) \
    FOR_EACH(MethodmapDecl) \
    FOR_EACH(ChangeScopeNode) \
    FOR_EACH(PropertyDecl) \
    FOR_EACH(GlobalInitStmt) \
    FOR_EACH(UpvarDecl)

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
    FOR_EACH(DefaultArgExpr) \
    FOR_EACH(FieldAccessExpr) \
    FOR_EACH(IndexExpr) \
    FOR_EACH(CommaExpr) \
    FOR_EACH(ThisExpr) \
    FOR_EACH(NullExpr) \
    FOR_EACH(NumberExpr) \
    FOR_EACH(StringExpr) \
    FOR_EACH(NewArrayExpr) \
    FOR_EACH(ArrayExpr) \
    FOR_EACH(StructExpr) \
    FOR_EACH(StructInitFieldExpr) \
    FOR_EACH(SpreadArgsExpr) \
    FOR_EACH(FunctionExpr) \

#define IR_TYPE_LIST(FOR_EACH) \
    FOR_EACH(Constant) \
    FOR_EACH(Rvalue) \
    FOR_EACH(Typename) \
    FOR_EACH(FunctionRef) \
    FOR_EACH(Variable) \
    FOR_EACH(Upvar) \
    FOR_EACH(String) \
    FOR_EACH(Unary) \
    FOR_EACH(Index) \
    FOR_EACH(StaticFieldRef) \
    FOR_EACH(FieldRef) \
    FOR_EACH(Accessor) \
    FOR_EACH(MethodRef) \
    FOR_EACH(Cast) \
    FOR_EACH(LvalueCast) \
    FOR_EACH(SimpleCast) \
    FOR_EACH(Sizeof) \
    FOR_EACH(IncDec) \
    FOR_EACH(Binary) \
    FOR_EACH(Logical) \
    FOR_EACH(Ternary) \
    FOR_EACH(Comma) \
    FOR_EACH(ChainedCompare) \
    FOR_EACH(Call) \
    FOR_EACH(DefaultArg) \
    FOR_EACH(NamedArg) \
    FOR_EACH(SpreadArgs) \
    FOR_EACH(Function) \
    FOR_EACH(Array) \
    FOR_EACH(Slice) \
    FOR_EACH(NewArray) \
    FOR_EACH(Struct) \
    FOR_EACH(StructInitField)


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
    IR_TYPE_LIST(_)
#undef _
};
