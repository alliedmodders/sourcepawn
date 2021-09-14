// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
//  Copyright (c) AlliedModders LLC 2021
//  Copyright (c) ITB CompuPhase, 1997-2006
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

#include <string>
#include <vector>

#include "parse-node.h"

class CompileContext;
class ParseTree;
struct symbol;

class CodeGenerator final
{
  public:
    CodeGenerator(CompileContext& cc, ParseTree* tree);

    void Generate();

    const std::vector<std::string>& debug_strings() const { return debug_strings_; }

  private:
    // Statements/decls.
    void EmitStmtList(StmtList* list);
    void EmitStmt(Stmt* stmt);
    void EmitChangeScopeNode(ChangeScopeNode* node);
    void EmitVarDecl(VarDecl* decl);
    void EmitPstruct(VarDecl* decl);
    void EmitGlobalVar(VarDecl* decl);
    void EmitLocalVar(VarDecl* decl);
    void EmitIfStmt(IfStmt* stmt);
    void EmitDeleteStmt(DeleteStmt* stmt);
    void EmitDoWhileStmt(DoWhileStmt* stmt);
    void EmitLoopControlStmt(LoopControlStmt* stmt);
    void EmitForStmt(ForStmt* stmt);
    void EmitSwitchStmt(SwitchStmt* stmt);
    void EmitFunctionInfo(FunctionInfo* info);
    void EmitEnumStructDecl(EnumStructDecl* info);
    void EmitMethodmapDecl(MethodmapDecl* info);
    void EmitReturnStmt(ReturnStmt* stmt);
    void EmitReturnArrayStmt(ReturnStmt* stmt);

    // Expressions.
    void EmitExpr(Expr* expr);
    void EmitTest(Expr* expr, bool jump_on_true, int target);
    void EmitUnary(UnaryExpr* expr);
    void EmitIncDec(IncDecExpr* expr);
    void EmitBinary(BinaryExpr* expr);
    void EmitBinaryInner(OpFunc oper, const UserOperation& in_user_op, Expr* left, Expr* right);
    void EmitLogicalExpr(LogicalExpr* expr);
    void EmitChainedCompareExpr(ChainedCompareExpr* expr);
    void EmitTernaryExpr(TernaryExpr* expr);
    void EmitTernaryInner(TernaryExpr* expr, ke::Maybe<cell_t>* branch1,
                          ke::Maybe<cell_t>* branch2);
    void EmitSymbolExpr(SymbolExpr* expr);
    void EmitIndexExpr(IndexExpr* expr);
    void EmitFieldAccessExpr(FieldAccessExpr* expr);
    void EmitCallExpr(CallExpr* expr);
    void EmitDefaultArgExpr(DefaultArgExpr* expr);
    void EmitCallUserOpExpr(CallUserOpExpr* expr);
    void EmitNewArrayExpr(NewArrayExpr* expr);

    // Logical test helpers.
    bool EmitUnaryExprTest(UnaryExpr* expr, bool jump_on_true, int target);
    void EmitLogicalExprTest(LogicalExpr* expr, bool jump_on_true, int target);
    bool EmitChainedCompareExprTest(ChainedCompareExpr* expr, bool jump_on_true, int target);

    void AddDebugFile(const std::string& line);
    void AddDebugLine(int linenr);
    void AddDebugSymbol(symbol* sym);

  private:
    CompileContext& cc_;
    ParseTree* tree_;
    symbol* func_ = nullptr;

    std::vector<std::string> debug_strings_;
};
