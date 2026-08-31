// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
//  Copyright (c) AlliedModders 2026
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
#include "ast-printer.h"

#include "lexer.h"
#include "parse-node.h"
#include <amtl/am-string.h>

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
        case ExprKind::Number64Expr: {
            auto node = expr->to<Number64Expr>();
            if (node->atom())
                fprintf(out_, "%s", node->atom()->chars());
            else
                fprintf(out_, "%lld", (long long)node->ToInt64().value_or(0));
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
    Print(node->expr(), true);
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

void AstPrinter::PrintVarDecl(VarDecl* node, bool is_last) {
    fprintf(out_, "VarDecl: %s (type: ", node->name()->chars());
    PrintType(node->type_info());
    fprintf(out_, ")\n");
    if (node->init()) {
        stack_.push_back(is_last);
        Print(node->init(), true);
        stack_.pop_back();
    }
}

void AstPrinter::PrintArgDecl(ArgDecl* node, bool is_last) {
    fprintf(out_, "ArgDecl: %s (type: ", node->name()->chars());
    PrintType(node->type_info());
    fprintf(out_, ")\n");
    if (node->init()) {
        stack_.push_back(is_last);
        Print(node->init(), true);
        stack_.pop_back();
    }
}

void AstPrinter::PrintConstDecl(ConstDecl* node, bool is_last) {
    fprintf(out_, "ConstDecl: %s (type: ", node->name()->chars());
    PrintType(node->type_info());
    fprintf(out_, ") value: %d\n", (int)node->const_val());
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
        Print(node->expr(), true);
        stack_.pop_back();
    }
}

void AstPrinter::PrintAssertStmt(AssertStmt* node, bool is_last) {
    fprintf(out_, "AssertStmt\n");
    stack_.push_back(is_last);
    Print(node->expr(), true);
    stack_.pop_back();
}

void AstPrinter::PrintDeleteStmt(DeleteStmt* node, bool is_last) {
    fprintf(out_, "DeleteStmt\n");
    stack_.push_back(is_last);
    Print(node->expr(), true);
    stack_.pop_back();
}

void AstPrinter::PrintExitStmt(ExitStmt* node, bool is_last) {
    fprintf(out_, "ExitStmt\n");
    stack_.push_back(is_last);
    Print(node->expr(), true);
    stack_.pop_back();
}

void AstPrinter::PrintDoWhileStmt(DoWhileStmt* node, bool is_last) {
    fprintf(out_, "DoWhileStmt (token '%s')\n", get_token_string(node->token()).c_str());
    stack_.push_back(is_last);
    Print(node->cond(), false);
    Print(node->body(), true);
    stack_.pop_back();
}

void AstPrinter::PrintForStmt(ForStmt* node, bool is_last) {
    fprintf(out_, "ForStmt\n");
    stack_.push_back(is_last);

    size_t count = (node->init() ? 1 : 0) + (node->cond() ? 1 : 0) + (node->advance() ? 1 : 0) + 1;
    size_t current = 0;
    if (node->init())
        Print(node->init(), ++current == count);
    if (node->cond())
        Print(node->cond(), ++current == count);
    if (node->advance())
        Print(node->advance(), ++current == count);
    Print(node->body(), ++current == count);

    stack_.pop_back();
}

void AstPrinter::PrintSwitchStmt(SwitchStmt* node, bool is_last) {
    fprintf(out_, "SwitchStmt\n");
    stack_.push_back(is_last);

    size_t count = 1 + node->cases().size() + (node->default_case() ? 1 : 0);
    size_t current = 0;

    Print(node->expr(), ++current == count);

    for (const auto& cas : node->cases()) {
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
        stack_.push_back(last);
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

void AstPrinter::PrintFunctionDecl(FunctionDecl* node, bool is_last) {
    fprintf(out_, "FunctionDecl: %s (args: %d)\n", node->name()->chars(), (int)node->args().size());
    stack_.push_back(is_last);
    for (size_t i = 0; i < node->args().size(); i++)
        Print(node->args()[i], (i == node->args().size() - 1) && !node->body());
    if (node->body())
        Print(node->body(), true);
    stack_.pop_back();
}

void AstPrinter::PrintMemberFunctionDecl(MemberFunctionDecl* node, bool is_last) {
    fprintf(out_, "MemberFunctionDecl: %s::%s\n", node->parent()->name()->chars(), node->name()->chars());
    stack_.push_back(is_last);
    for (size_t i = 0; i < node->args().size(); i++)
        Print(node->args()[i], (i == node->args().size() - 1) && !node->body());
    if (node->body())
        Print(node->body(), true);
    stack_.pop_back();
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

void AstPrinter::PrintMethodmapPropertyDecl(MethodmapPropertyDecl* node, bool is_last) {
    fprintf(out_, "MethodmapPropertyDecl: %s\n", node->name()->chars());
}

void AstPrinter::PrintMethodmapMethodDecl(MethodmapMethodDecl* node, bool is_last) {
    fprintf(out_, "MethodmapMethodDecl: %s (ctor: %d, dtor: %d)\n", node->name()->chars(),
            node->is_ctor(), node->is_dtor());
    stack_.push_back(is_last);
    for (size_t i = 0; i < node->args().size(); i++)
        Print(node->args()[i], (i == node->args().size() - 1) && !node->body());
    if (node->body())
        Print(node->body(), true);
    stack_.pop_back();
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

void AstPrinter::PrintRvalueExpr(RvalueExpr* node, bool is_last) {
    fprintf(out_, "RvalueExpr\n");
    stack_.push_back(is_last);
    Print(node->expr(), true);
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

void AstPrinter::PrintTaggedValueExpr(TaggedValueExpr* node, bool is_last) {
    fprintf(out_, "TaggedValueExpr: %d\n", (int)node->value());
}

void AstPrinter::PrintNumber64Expr(Number64Expr* node, bool is_last) {
    if (node->atom())
        fprintf(out_, "Number64Expr: %s\n", node->atom()->chars());
    else
        fprintf(out_, "Number64Expr: %lld\n", (long long)node->ToInt64().value_or(0));
}

void AstPrinter::PrintStringExpr(StringExpr* node, bool is_last) {
    fprintf(out_, "StringExpr: \"%s\"\n", node->text()->chars());
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

void AstPrinter::PrintSimpleCastExpr(SimpleCastExpr* node, bool is_last) {
    fprintf(out_, "SimpleCastExpr\n");
    stack_.push_back(is_last);
    Print(node->from(), true);
    stack_.pop_back();
}

} // namespace cc
} // namespace sp
