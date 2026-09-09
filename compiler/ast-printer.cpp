// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
//
#include "ast-printer.h"

#include <inttypes.h>

#include <amtl/am-string.h>
#include "ir-node.h"
#include "lexer.h"
#include "parse-node.h"

namespace sp {
namespace cc {

AstPrinter::AstPrinter(FILE* out)
  : out_(out)
{}

void AstPrinter::PrintIndent(bool is_last) {
    for (bool last : stack_) {
        fprintf(out_, last ? "    " : "│   ");
    }
    fprintf(out_, is_last ? "└── " : "├── ");
}

void AstPrinter::Print(ParseTree* tree) {
    const auto& stmts = tree->stmts()->stmts();
    for (size_t i = 0; i < stmts.size(); i++) {
        Print(stmts[i], i == stmts.size() - 1);
    }
}

void AstPrinter::Print(Stmt* stmt, bool is_last) {
    if (!stmt)
        return;

    PrintIndent(is_last);
    switch (stmt->kind()) {
#define _(Name) case StmtKind::Name: Print##Name(stmt->to<Name>(), is_last); break;
        AST_STMT_TYPE_LIST(_)
#undef _
        default:
            fprintf(out_, "Unknown StmtKind\n");
            break;
    }
}

void AstPrinter::Print(Expr* expr, bool is_last) {
    if (!expr) {
        PrintIndent(is_last);
        fprintf(out_, "(null)\n");
        return;
    }

    PrintIndent(is_last);
    switch (expr->kind()) {
#define _(Name) case ExprKind::Name: Print##Name(expr->to<Name>(), is_last); break;
        AST_EXPR_TYPE_LIST(_)
#undef _
        default:
            assert(false);
            break;
    }
}

void AstPrinter::PrintExprInline(Expr* expr) {
    if (!expr)
        return;

    switch (expr->kind()) {
        case ExprKind::NumberExpr: {
            auto node = expr->to<NumberExpr>();
            PrintConstValue(node->val());
            break;
        }
        case ExprKind::SymbolExpr:
            fprintf(out_, "%s", expr->to<SymbolExpr>()->name()->chars());
            break;
        default:
            fprintf(out_, "...");
            break;
    }
}

void AstPrinter::PrintStmtList(StmtList* node, bool is_last) {
    fprintf(out_, "StmtList (%d stmts)\n", (int)node->stmts().size());
    stack_.push_back(is_last);
    for (size_t i = 0; i < node->stmts().size(); i++)
        Print(node->stmts()[i], i == node->stmts().size() - 1);
    stack_.pop_back();
}

void AstPrinter::PrintBlockStmt(BlockStmt* node, bool is_last) {
    fprintf(out_, "BlockStmt (%d stmts)\n", (int)node->stmts().size());
    stack_.push_back(is_last);
    for (size_t i = 0; i < node->stmts().size(); i++)
        Print(node->stmts()[i], i == node->stmts().size() - 1);
    stack_.pop_back();
}

void AstPrinter::PrintBreakStmt(BreakStmt* node, bool is_last) {
    fprintf(out_, "BreakStmt\n");
}

void AstPrinter::PrintContinueStmt(ContinueStmt* node, bool is_last) {
    fprintf(out_, "ContinueStmt\n");
}

void AstPrinter::PrintExprStmt(ExprStmt* node, bool is_last) {
    fprintf(out_, "ExprStmt\n");
    stack_.push_back(is_last);
    Print(node->expr(), false);
    PrintIr(node->sema_expr(), true);
    stack_.pop_back();
}

void AstPrinter::PrintUnaryExpr(UnaryExpr* node, bool is_last) {
    fprintf(out_, "UnaryExpr (token '%s')\n", get_token_string(node->token()).c_str());
    stack_.push_back(is_last);
    Print(node->expr(), true);
    stack_.pop_back();
}

void AstPrinter::PrintBinaryExpr(BinaryExpr* node, bool is_last) {
    fprintf(out_, "BinaryExpr (token '%s')\n", get_token_string(node->token()).c_str());
    stack_.push_back(is_last);
    Print(node->left(), false);
    Print(node->right(), true);
    stack_.pop_back();
}

void AstPrinter::PrintType(const typeinfo_t& type) {
    if (type.type) {
        fprintf(out_, "%s", type.type->prettyName());
    } else if (type.type_atom) {
        fprintf(out_, "%s", type.type_atom->chars());
    } else {
        fprintf(out_, "(unknown-type)");
    }
    if (type.is_const)
        fprintf(out_, " const");
    if (type.reference)
        fprintf(out_, "&");
    for (size_t i = 0; i < type.dim_exprs.size(); i++) {
        fprintf(out_, "[");
        if (type.dim_exprs[i])
            PrintExprInline(type.dim_exprs[i]);
        fprintf(out_, "]");
    }
}

void AstPrinter::PrintStaticAssertStmt(StaticAssertStmt* node, bool is_last) {
    fprintf(out_, "StaticAssertStmt\n");
    stack_.push_back(is_last);
    Print(node->expr(), node->text() ? false : true);
    if (node->text()) {
        PrintIndent(true);
        fprintf(out_, "message: %s\n", node->text()->chars());
    }
    stack_.pop_back();
}

void AstPrinter::PrintGlobalInitStmt(GlobalInitStmt* node, bool is_last) {
    fprintf(out_, "GlobalInitStmt\n");
    stack_.push_back(is_last);
    for (size_t i = 0; i < node->vars().size(); i++) {
        bool last = i == node->vars().size() - 1;
        PrintIndent(last);
        fprintf(out_, "%s\n", node->vars()[i]->name()->chars());
        stack_.push_back(last);
        PrintIr(node->vars()[i]->sema_init_rhs(), true);
        stack_.pop_back();
    }
    stack_.pop_back();
}

void AstPrinter::PrintVarDecl(VarDecl* node, bool is_last) {
    fprintf(out_, "VarDecl: %s (type: ", node->name()->chars());
    PrintType(node->type_info());
    fprintf(out_, ")\n");
    if (node->init()) {
        stack_.push_back(is_last);
        Print(node->init(), false);
        PrintIr(node->sema_init_rhs(), true);
        stack_.pop_back();
    }
}

void AstPrinter::PrintArgDecl(ArgDecl* node, bool is_last) {
    fprintf(out_, "ArgDecl: %s (type: ", node->name()->chars());
    PrintType(node->type_info());
    fprintf(out_, ")\n");
    if (node->init()) {
        stack_.push_back(is_last);
        Print(node->init(), false);
        PrintIr(node->sema_init_rhs(), true);
        stack_.pop_back();
    }
}

void AstPrinter::PrintConstDecl(ConstDecl* node, bool is_last) {
    fprintf(out_, "ConstDecl: %s (type: ", node->name()->chars());
    PrintType(node->type_info());
    fprintf(out_, ") value: ");
    PrintConstValue(node->value());
    fputc('\n', out_);
}

void AstPrinter::PrintConstValue(const ExprVal& cv) {
    if (cv.type()->isFloat()) {
        fprintf(out_, "%f", cv.const_float());
    } else if (cv.type()->isDouble()) {
        fprintf(out_, "%f", cv.const_double());
    } else if (cv.type()->isInt64()) {
        fprintf(out_, "%" PRId64, (int64_t)cv.const_int64());
    } else {
        fprintf(out_, "%d", (int)cv.const_cell());
    }
}

void AstPrinter::PrintEnumDecl(EnumDecl* node, bool is_last) {
    fprintf(out_, "EnumDecl: %s (fields: %d)\n", node->name() ? node->name()->chars() : "(unnamed)",
            (int)node->fields().size());
    stack_.push_back(is_last);
    for (size_t i = 0; i < node->fields().size(); i++)
        Print(node->fields()[i], i == node->fields().size() - 1);
    stack_.pop_back();
}

void AstPrinter::PrintEnumFieldDecl(EnumFieldDecl* node, bool is_last) {
    fprintf(out_, "EnumFieldDecl: %s", node->name()->chars());
    if (node->value()) {
        fprintf(out_, " = ");
        PrintExprInline(node->value());
    }
    fprintf(out_, "\n");
}

void AstPrinter::PrintPstructDecl(PstructDecl* node, bool is_last) {
    fprintf(out_, "PstructDecl: %s\n", node->name()->chars());
    stack_.push_back(is_last);
    for (size_t i = 0; i < node->fields().size(); i++)
        Print(node->fields()[i], i == node->fields().size() - 1);
    stack_.pop_back();
}

void AstPrinter::PrintTypedefDecl(TypedefDecl* node, bool is_last) {
    fprintf(out_, "TypedefDecl: %s\n", node->name()->chars());
}

void AstPrinter::PrintTypesetDecl(TypesetDecl* node, bool is_last) {
    fprintf(out_, "TypesetDecl: %s\n", node->name()->chars());
}

void AstPrinter::PrintIfStmt(IfStmt* node, bool is_last) {
    fprintf(out_, "IfStmt\n");
    stack_.push_back(is_last);
    Print(node->cond(), false);
    PrintIr(node->sema_cond(), false);
    if (node->on_false()) {
        Print(node->on_true(), false);
        Print(node->on_false(), true);
    } else {
        Print(node->on_true(), true);
    }
    stack_.pop_back();
}

void AstPrinter::PrintReturnStmt(ReturnStmt* node, bool is_last) {
    fprintf(out_, "ReturnStmt\n");
    if (node->expr()) {
        stack_.push_back(is_last);
        Print(node->expr(), false);
        PrintIr(node->sema_expr(), true);
        stack_.pop_back();
    }
}

void AstPrinter::PrintDeleteStmt(DeleteStmt* node, bool is_last) {
    fprintf(out_, "DeleteStmt\n");
    stack_.push_back(is_last);
    Print(node->expr(), false);
    PrintIr(node->sema_expr(), true);
    stack_.pop_back();
}

void AstPrinter::PrintDoWhileStmt(DoWhileStmt* node, bool is_last) {
    fprintf(out_, "DoWhileStmt (token '%s')\n", get_token_string(node->token()).c_str());
    stack_.push_back(is_last);
    Print(node->cond(), false);
    PrintIr(node->sema_cond(), false);
    Print(node->body(), true);
    stack_.pop_back();
}

void AstPrinter::PrintForStmt(ForStmt* node, bool is_last) {
    fprintf(out_, "ForStmt\n");
    stack_.push_back(is_last);

    size_t count = (node->init() ? 1 : 0) + (node->cond() ? 2 : 0) +
                   (node->advance() ? 1 : 0) + 1;
    size_t current = 0;
    if (node->init())
        Print(node->init(), ++current == count);
    if (node->cond()) {
        Print(node->cond(), ++current == count);
        PrintIr(node->sema_cond(), ++current == count);
    }
    if (node->advance())
        Print(node->advance(), ++current == count);
    Print(node->body(), ++current == count);

    stack_.pop_back();
}

void AstPrinter::PrintSwitchStmt(SwitchStmt* node, bool is_last) {
    fprintf(out_, "SwitchStmt\n");
    stack_.push_back(is_last);

    size_t count = 2 + node->cases().size() + (node->default_case() ? 1 : 0);
    size_t current = 0;

    Print(node->expr(), ++current == count);
    PrintIr(node->sema_expr(), ++current == count);

    for (size_t ci = 0; ci < node->cases().size(); ci++) {
        const auto& cas = node->cases()[ci];
        bool last = (++current == count);
        PrintIndent(last);
        fprintf(out_, "case ");
        if (cas.first.empty()) {
            fprintf(out_, "(none?)");
        } else {
            for (size_t i = 0; i < cas.first.size(); i++) {
                if (i > 0)
                    fprintf(out_, ", ");
                PrintExprInline(cas.first[i]);
            }
        }
        fprintf(out_, ":\n");
        const auto& case_irs = node->sema_case_exprs(ci);
        stack_.push_back(last);
        for (size_t i = 0; i < case_irs.size(); i++)
            PrintIr(case_irs[i], i == case_irs.size() - 1);
        Print(cas.second, true);
        stack_.pop_back();
    }
    if (node->default_case()) {
        bool last = (++current == count);
        PrintIndent(last);
        fprintf(out_, "default:\n");
        stack_.push_back(last);
        Print(node->default_case(), true);
        stack_.pop_back();
    }
    stack_.pop_back();
}

void AstPrinter::PrintPragmaUnusedStmt(PragmaUnusedStmt* node, bool is_last) {
    fprintf(out_, "PragmaUnusedStmt: ");
    for (size_t i = 0; i < node->names().size(); i++) {
        if (i > 0)
            fprintf(out_, ", ");
        fprintf(out_, "%s", node->names()[i]->chars());
    }
    fprintf(out_, "\n");
}

void AstPrinter::PrintFunctionBody(FunctionDecl* node, bool is_last) {
    stack_.push_back(is_last);
    const auto& prebody = node->prebody();
    bool has_after_args = !prebody.empty() || node->body();
    for (size_t i = 0; i < node->args().size(); i++)
        Print(node->args()[i], (i == node->args().size() - 1) && !has_after_args);
    for (size_t i = 0; i < prebody.size(); i++)
        Print(prebody[i], (i == prebody.size() - 1) && !node->body());
    if (node->body())
        Print(node->body(), true);
    stack_.pop_back();
}

void AstPrinter::PrintFunctionDecl(FunctionDecl* node, bool is_last) {
    fprintf(out_, "FunctionDecl: %s (args: %d)\n", node->name()->chars(), (int)node->args().size());
    PrintFunctionBody(node, is_last);
}

void AstPrinter::PrintMemberFunctionDecl(MemberFunctionDecl* node, bool is_last) {
    fprintf(out_, "MemberFunctionDecl: %s::%s (ctor: %d, dtor: %d)\n", node->parent()->name()->chars(), node->name()->chars(),
            node->is_ctor(), node->is_dtor());
    PrintFunctionBody(node, is_last);
}

void AstPrinter::PrintLayoutMemberDecl(LayoutMemberDecl* node, bool is_last) {
    fprintf(out_, "LayoutMemberDecl: %s (private: %d)\n", node->name()->chars(), node->is_private());
}

void AstPrinter::PrintEnumStructDecl(EnumStructDecl* node, bool is_last) {
    fprintf(out_, "EnumStructDecl: %s\n", node->name()->chars());
    stack_.push_back(is_last);

    bool has_methods = !node->methods().empty();

    PrintIndent(!has_methods);
    fprintf(out_, "fields:\n");
    stack_.push_back(!has_methods);
    for (size_t i = 0; i < node->fields().size(); i++)
        Print(node->fields()[i], i == node->fields().size() - 1);
    stack_.pop_back();

    if (has_methods) {
        PrintIndent(true);
        fprintf(out_, "methods:\n");
        stack_.push_back(true);
        for (size_t i = 0; i < node->methods().size(); i++)
            Print(node->methods()[i], i == node->methods().size() - 1);
        stack_.pop_back();
    }

    stack_.pop_back();
}

void AstPrinter::PrintClassDecl(ClassDecl* node, bool is_last) {
    fprintf(out_, "ClassDecl: %s\n", node->name()->chars());
    stack_.push_back(is_last);

    bool has_methods = !node->methods().empty();

    PrintIndent(!has_methods);
    fprintf(out_, "fields:\n");
    stack_.push_back(!has_methods);
    for (size_t i = 0; i < node->fields().size(); i++)
        Print(node->fields()[i], i == node->fields().size() - 1);
    stack_.pop_back();

    if (has_methods) {
        PrintIndent(true);
        fprintf(out_, "methods:\n");
        stack_.push_back(true);
        for (size_t i = 0; i < node->methods().size(); i++)
            Print(node->methods()[i], i == node->methods().size() - 1);
        stack_.pop_back();
    }

    stack_.pop_back();
}

void AstPrinter::PrintLayoutFieldDecl(LayoutFieldDecl* node, bool is_last) {
    fprintf(out_, "LayoutFieldDecl: %s (type: ", node->name()->chars());
    PrintType(node->type_info());
    fprintf(out_, ")\n");
}

void AstPrinter::PrintMethodmapDecl(MethodmapDecl* node, bool is_last) {
    fprintf(out_, "MethodmapDecl: %s", node->name()->chars());
    if (node->extends())
        fprintf(out_, " extends %s", node->extends()->chars());
    fprintf(out_, "\n");
    stack_.push_back(is_last);

    bool has_methods = !node->methods().empty();

    PrintIndent(!has_methods);
    fprintf(out_, "properties:\n");
    stack_.push_back(!has_methods);
    for (size_t i = 0; i < node->properties().size(); i++)
        Print(node->properties()[i], i == node->properties().size() - 1);
    stack_.pop_back();

    if (has_methods) {
        PrintIndent(true);
        fprintf(out_, "methods:\n");
        stack_.push_back(true);
        for (size_t i = 0; i < node->methods().size(); i++)
            Print(node->methods()[i], i == node->methods().size() - 1);
        stack_.pop_back();
    }

    stack_.pop_back();
}

void AstPrinter::PrintChangeScopeNode(ChangeScopeNode* node, bool is_last) {
    fprintf(out_, "ChangeScopeNode: %s\n", node->file()->chars());
}

void AstPrinter::PrintPropertyDecl(PropertyDecl* node, bool is_last) {
    fprintf(out_, "PropertyDecl: %s\n", node->name()->chars());
}

void AstPrinter::PrintUpvarDecl(UpvarDecl* node, bool is_last) {
    fprintf(out_, "UpvarDecl: %s\n", node->name()->chars());
}

void AstPrinter::PrintLogicalExpr(LogicalExpr* node, bool is_last) {
    fprintf(out_, "LogicalExpr (token '%s')\n", get_token_string(node->token()).c_str());
    stack_.push_back(is_last);
    Print(node->left(), false);
    Print(node->right(), true);
    stack_.pop_back();
}

void AstPrinter::PrintChainedCompareExpr(ChainedCompareExpr* node, bool is_last) {
    fprintf(out_, "ChainedCompareExpr\n");
    stack_.push_back(is_last);
    Print(node->first(), node->ops().empty());
    for (size_t i = 0; i < node->ops().size(); i++) {
        bool last = (i == node->ops().size() - 1);
        PrintIndent(last);
        fprintf(out_, "op %d\n", node->ops()[i].token);
        stack_.push_back(last);
        Print(node->ops()[i].expr, true);
        stack_.pop_back();
    }
    stack_.pop_back();
}

void AstPrinter::PrintTernaryExpr(TernaryExpr* node, bool is_last) {
    fprintf(out_, "TernaryExpr\n");
    stack_.push_back(is_last);
    Print(node->first(), false);
    Print(node->second(), false);
    Print(node->third(), true);
    stack_.pop_back();
}

void AstPrinter::PrintIncDecExpr(IncDecExpr* node, bool is_last) {
    fprintf(out_, "IncDecExpr (token '%s', prefix: %d)\n", get_token_string(node->token()).c_str(), node->prefix());
    stack_.push_back(is_last);
    Print(node->expr(), true);
    stack_.pop_back();
}

void AstPrinter::PrintCastExpr(CastExpr* node, bool is_last) {
    fprintf(out_, "CastExpr (token '%s')\n", get_token_string(node->token()).c_str());
    stack_.push_back(is_last);
    Print(node->expr(), true);
    stack_.pop_back();
}

void AstPrinter::PrintSizeofExpr(SizeofExpr* node, bool is_last) {
    fprintf(out_, "SizeofExpr\n");
    stack_.push_back(is_last);
    Print(node->child(), true);
    stack_.pop_back();
}

void AstPrinter::PrintSymbolExpr(SymbolExpr* node, bool is_last) {
    fprintf(out_, "SymbolExpr: %s\n", node->name()->chars());
}

void AstPrinter::PrintCallExpr(CallExpr* node, bool is_last) {
    fprintf(out_, "CallExpr (token '%s')\n", get_token_string(node->token()).c_str());
    stack_.push_back(is_last);
    if (!node->args().empty()) {
        Print(node->target(), false);
        for (size_t i = 0; i < node->args().size(); i++)
            Print(node->args()[i], i == node->args().size() - 1);
    } else {
        Print(node->target(), true);
    }
    stack_.pop_back();
}

void AstPrinter::PrintNamedArgExpr(NamedArgExpr* node, bool is_last) {
    fprintf(out_, "NamedArgExpr: %s\n", node->name->chars());
    stack_.push_back(is_last);
    Print(node->expr, true);
    stack_.pop_back();
}

void AstPrinter::PrintDefaultArgExpr(DefaultArgExpr* node, bool is_last) {
    fprintf(out_, "DefaultArgExpr\n");
}

void AstPrinter::PrintFieldAccessExpr(FieldAccessExpr* node, bool is_last) {
    fprintf(out_, "FieldAccessExpr: .%s\n", node->name()->chars());
    stack_.push_back(is_last);
    Print(node->base(), true);
    stack_.pop_back();
}

void AstPrinter::PrintIndexExpr(IndexExpr* node, bool is_last) {
    fprintf(out_, "IndexExpr\n");
    stack_.push_back(is_last);
    Print(node->base(), false);
    Print(node->index(), true);
    stack_.pop_back();
}

void AstPrinter::PrintCommaExpr(CommaExpr* node, bool is_last) {
    fprintf(out_, "CommaExpr\n");
    stack_.push_back(is_last);
    for (size_t i = 0; i < node->exprs().size(); i++)
        Print(node->exprs()[i], i == node->exprs().size() - 1);
    stack_.pop_back();
}

void AstPrinter::PrintThisExpr(ThisExpr* node, bool is_last) {
    fprintf(out_, "ThisExpr\n");
}

void AstPrinter::PrintNullExpr(NullExpr* node, bool is_last) {
    fprintf(out_, "NullExpr\n");
}

void AstPrinter::PrintNumberExpr(NumberExpr* node, bool is_last) {
    fprintf(out_, "NumberExpr: ");
    PrintConstValue(node->val());
    fputc('\n', out_);
}

void AstPrinter::PrintEscapedString(const char* s) {
    for (; *s; s++) {
        unsigned char c = *s;
        switch (c) {
            case '\n': fputs("\\n", out_); break;
            case '\r': fputs("\\r", out_); break;
            case '\t': fputs("\\t", out_); break;
            case '"':  fputs("\\\"", out_); break;
            case '\\': fputs("\\\\", out_); break;
            default:
                if (c < 0x20)
                    fprintf(out_, "\\x%02x", c);
                else
                    fputc(c, out_);
                break;
        }
    }
}

void AstPrinter::PrintStringExpr(StringExpr* node, bool is_last) {
    fprintf(out_, "StringExpr: \"");
    PrintEscapedString(node->text()->chars());
    fputc('"', out_);
    fputc('\n', out_);
}

void AstPrinter::PrintNewArrayExpr(NewArrayExpr* node, bool is_last) {
    fprintf(out_, "NewArrayExpr\n");
    stack_.push_back(is_last);
    for (size_t i = 0; i < node->exprs().size(); i++)
        Print(node->exprs()[i], i == node->exprs().size() - 1);
    stack_.pop_back();
}

void AstPrinter::PrintArrayExpr(ArrayExpr* node, bool is_last) {
    fprintf(out_, "ArrayExpr (ellipses: %d)\n", node->ellipses());
    stack_.push_back(is_last);
    for (size_t i = 0; i < node->exprs().size(); i++)
        Print(node->exprs()[i], i == node->exprs().size() - 1);
    stack_.pop_back();
}

void AstPrinter::PrintStructExpr(StructExpr* node, bool is_last) {
    fprintf(out_, "StructExpr\n");
    stack_.push_back(is_last);
    for (size_t i = 0; i < node->fields().size(); i++)
        Print(node->fields()[i], i == node->fields().size() - 1);
    stack_.pop_back();
}

void AstPrinter::PrintStructInitFieldExpr(StructInitFieldExpr* node, bool is_last) {
    fprintf(out_, "field %s\n", node->name->chars());
    stack_.push_back(is_last);
    Print(node->value, true);
    stack_.pop_back();
}

void AstPrinter::PrintSpreadArgsExpr(SpreadArgsExpr* node, bool is_last) {
    fprintf(out_, "SpreadArgsExpr\n");
}

void AstPrinter::PrintFunctionExpr(FunctionExpr* node, bool is_last) {
    fprintf(out_, "FunctionExpr: %s\n",
            node->decl()->name() ? node->decl()->name()->chars() : "(anonymous)");
    PrintFunctionBody(node->decl(), is_last);
}

void AstPrinter::PrintIr(ir::Value* expr, bool is_last) {
    if (!expr)
        return;

    PrintIndent(is_last);
    switch (expr->kind()) {
        case IrKind::Number: {
            const auto& v = expr->val();
            if (v.type()->isInt64())
                fprintf(out_, "Number i64 0x%" PRIx64 "\n", v.const_int64());
            else if (v.type()->isIntPtr())
                fprintf(out_, "Number intptr 0x%x\n", v.const_intptr());
            else if (v.type()->isDouble())
                fprintf(out_, "Number f64 %g\n", v.const_double());
            else if (v.type()->isHeapItem())
                fprintf(out_, "Number heapitem 0x%x\n", v.const_i32_);
            else
                fprintf(out_, "Number 0x%x\n", v.const_cell());
            break;
        }
        case IrKind::Symbol:
            fprintf(out_, "Symbol %s\n", expr->pn()->to<SymbolExpr>()->name()->chars());
            break;
        case IrKind::String:
            fprintf(out_, "String \"%s\"\n", expr->pn()->to<StringExpr>()->text()->chars());
            break;
        case IrKind::This:
            fprintf(out_, "This\n");
            break;
        case IrKind::Null:
            fprintf(out_, "Null\n");
            break;
        case IrKind::Unary: {
            auto u = expr->to<ir::Unary>();
            fprintf(out_, "Unary '%c'\n", u->token());
            stack_.push_back(is_last);
            PrintIr(u->expr(), true);
            stack_.pop_back();
            break;
        }
        case IrKind::Logical: {
            auto e = expr->to<ir::Logical>();
            fprintf(out_, "Logical '%s'\n", get_token_string(e->token()).c_str());
            stack_.push_back(is_last);
            PrintIr(e->left(), false);
            PrintIr(e->right(), true);
            stack_.pop_back();
            break;
        }
        case IrKind::Ternary: {
            auto e = expr->to<ir::Ternary>();
            fprintf(out_, "Ternary\n");
            stack_.push_back(is_last);
            PrintIr(e->first(), false);
            PrintIr(e->second(), false);
            PrintIr(e->third(), true);
            stack_.pop_back();
            break;
        }
        case IrKind::Comma: {
            auto e = expr->to<ir::Comma>();
            fprintf(out_, "Comma\n");
            stack_.push_back(is_last);
            const auto& exprs = e->exprs();
            for (size_t i = 0; i < exprs.size(); i++)
                PrintIr(exprs[i], i == exprs.size() - 1);
            stack_.pop_back();
            break;
        }
        case IrKind::Struct: {
            auto e = expr->to<ir::Struct>();
            fprintf(out_, "Struct\n");
            stack_.push_back(is_last);
            const auto& fields = e->fields();
            for (size_t i = 0; i < fields.size(); i++)
                PrintIr(fields[i], i == fields.size() - 1);
            stack_.pop_back();
            break;
        }
        case IrKind::StructInitField: {
            auto e = expr->to<ir::StructInitField>();
            auto ast = expr->pn()->to<StructInitFieldExpr>();
            fprintf(out_, "StructInitField '%s'\n", ast->name->chars());
            stack_.push_back(is_last);
            PrintIr(e->value(), true);
            stack_.pop_back();
            break;
        }
        case IrKind::NewArray: {
            auto e = expr->to<ir::NewArray>();
            fprintf(out_, "NewArray\n");
            stack_.push_back(is_last);
            const auto& dims = e->dims();
            for (size_t i = 0; i < dims.size(); i++) {
                if (dims[i])
                    PrintIr(dims[i], i == dims.size() - 1);
                else {
                    PrintIndent(i == dims.size() - 1);
                    fprintf(out_, "(dim)\n");
                }
            }
            stack_.pop_back();
            break;
        }
        case IrKind::Array: {
            auto e = expr->to<ir::Array>();
            fprintf(out_, "Array%s\n", e->ellipses() ? " [..]" : "");
            stack_.push_back(is_last);
            const auto& elts = e->elements();
            for (size_t i = 0; i < elts.size(); i++)
                PrintIr(elts[i], i == elts.size() - 1);
            stack_.pop_back();
            break;
        }
        case IrKind::Slice: {
            auto e = expr->to<ir::Slice>();
            fprintf(out_, "Slice\n");
            stack_.push_back(is_last);
            PrintIr(e->base(), !e->index());
            if (e->index())
                PrintIr(e->index(), true);
            stack_.pop_back();
            break;
        }
        case IrKind::Call: {
            auto e = expr->to<ir::Call>();
            fprintf(out_, "Call\n");
            stack_.push_back(is_last);
            if (e->target())
                PrintIr(e->target(), false);
            const auto& args = e->args();
            for (size_t i = 0; i < args.size(); i++)
                PrintIr(args[i], i == args.size() - 1);
            if (!e->target() && args.empty())
                fprintf(out_, "    (no children)\n");
            stack_.pop_back();
            break;
        }
        case IrKind::DefaultArg: {
            auto ast = expr->to<ir::DefaultArg>()->parent();
            fprintf(out_, "DefaultArg %s\n", ast->arg()->name()->chars());
            stack_.push_back(is_last);
            PrintIr(ast->arg()->sema_init_rhs(), true);
            stack_.pop_back();
            break;
        }
        case IrKind::NamedArg: {
            auto e = expr->to<ir::NamedArg>();
            auto ast = expr->pn()->to<NamedArgExpr>();
            fprintf(out_, "NamedArg %s\n", ast->name->chars());
            stack_.push_back(is_last);
            PrintIr(e->expr(), true);
            stack_.pop_back();
            break;
        }
        case IrKind::SpreadArgs:
            fprintf(out_, "SpreadArgs\n");
            break;
        case IrKind::Function: {
            auto ast = expr->to<ir::Function>()->parent();
            fprintf(out_, "Function %s\n", ast->decl()->name()->chars());
            break;
        }
        case IrKind::ChainedCompare: {
            auto e = expr->to<ir::ChainedCompare>();
            fprintf(out_, "ChainedCompare\n");
            stack_.push_back(is_last);
            PrintIr(e->first(), false);
            const auto& ops = e->ops();
            for (size_t i = 0; i < ops.size(); i++) {
                bool last = i == ops.size() - 1;
                PrintIndent(last);
                fprintf(out_, "Op '%s'\n", get_token_string(ops[i].token).c_str());
                stack_.push_back(last);
                PrintIr(ops[i].expr, true);
                stack_.pop_back();
            }
            stack_.pop_back();
            break;
        }
        case IrKind::Binary: {
            auto e = expr->to<ir::Binary>();
            fprintf(out_, "Binary '%s'\n", get_token_string(e->token()).c_str());
            stack_.push_back(is_last);
            PrintIr(e->left(), false);
            PrintIr(e->right(), true);
            stack_.pop_back();
            break;
        }
        case IrKind::IncDec: {
            auto e = expr->to<ir::IncDec>();
            fprintf(out_, "IncDec '%s' %s\n", e->token() == tINC ? "++" : "--",
                    e->prefix() ? "prefix" : "postfix");
            stack_.push_back(is_last);
            PrintIr(e->expr(), true);
            stack_.pop_back();
            break;
        }
        case IrKind::Index: {
            auto e = expr->to<ir::Index>();
            fprintf(out_, "Index\n");
            stack_.push_back(is_last);
            PrintIr(e->base(), false);
            PrintIr(e->index(), true);
            stack_.pop_back();
            break;
        }
        case IrKind::FieldAccess: {
            auto e = expr->to<ir::FieldAccess>();
            fprintf(out_, "FieldAccess '%s'\n",
                    e->token() == tDBLCOLON ? "::" : expr->pn()->to<FieldAccessExpr>()->name()->chars());
            stack_.push_back(is_last);
            PrintIr(e->base(), true);
            stack_.pop_back();
            break;
        }
        case IrKind::Cast: {
            auto e = expr->to<ir::Cast>();
            fprintf(out_, "Cast\n");
            stack_.push_back(is_last);
            PrintIr(e->expr(), true);
            stack_.pop_back();
            break;
        }
        case IrKind::SimpleCast: {
            auto e = expr->to<ir::SimpleCast>();
            fprintf(out_, "SimpleCast\n");
            stack_.push_back(is_last);
            PrintIr(e->from(), true);
            stack_.pop_back();
            break;
        }
        case IrKind::Sizeof: {
            auto e = expr->to<ir::Sizeof>();
            fprintf(out_, "Sizeof\n");
            stack_.push_back(is_last);
            PrintIr(e->child(), true);
            stack_.pop_back();
            break;
        }
        case IrKind::Rvalue: {
            auto r = expr->to<ir::Rvalue>();
            fprintf(out_, "Rvalue\n");
            stack_.push_back(is_last);
            PrintIr(r->expr(), true);
            stack_.pop_back();
            break;
        }
        default:
            assert(false);
            break;
    }
}

} // namespace cc
} // namespace sp
