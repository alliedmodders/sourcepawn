// vim: set ts=8 sts=4 sw=4 tw=99 et:
//  Pawn compiler - Recursive descend expresion parser
//
//  Copyright (c) ITB CompuPhase, 1997-2005
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
#include "semantics.h"

#include <unordered_set>

#include <amtl/am-raii.h>
#include "array-helpers.h"
#include "code-generator.h"
#include "emitter.h"
#include "errors.h"
#include "expressions.h"
#include "lexer.h"
#include "lexer-inl.h"
#include "parse-node.h"
#include "sclist.h"
#include "sctracker.h"
#include "scvars.h"
#include "symbols.h"

AutoEnterScope::AutoEnterScope(SemaContext& sc, SymbolScope* scope)
  : sc_(sc)
{
    assert(scope->parent() == sc.scope());
    sc.set_scope(scope);
}

AutoEnterScope::AutoEnterScope(SemaContext& sc, ScopeKind kind)
  : sc_(sc)
{
    sc.set_scope(new SymbolScope(sc.scope(), kind));
}

AutoEnterScope::~AutoEnterScope()
{
    sc_.set_scope(sc_.scope()->parent());
}

void
Stmt::Process()
{
    SemaContext sc;
    AutoCountErrors errors;

    if (!Bind(sc) || !errors.ok())
        return;

    errors.Reset();
    if (!Analyze(sc) || !errors.ok())
        return;

    ProcessUses(sc);

    CodegenContext cg(curfunc);
    if (sc_status == statWRITE)
        Emit(cg);
}

bool
StmtList::Analyze(SemaContext& sc)
{
    bool ok = true;
    for (const auto& stmt : stmts_) {
        errorset(sRESET, 0);

        ok &= stmt->Analyze(sc);

        FlowType flow = stmt->flow_type();
        if (flow != Flow_None && flow_type() == Flow_None)
            set_flow_type(flow);
    }
    return ok;
}

void
StmtList::DoEmit(CodegenContext& cg)
{
    for (const auto& stmt : stmts_)
        stmt->Emit(cg);
}

bool
Decl::Analyze(SemaContext& sc)
{
    // No analysis needed for most decls.
    return true;
}

VarDecl::VarDecl(const token_pos_t& pos, sp::Atom* name, const typeinfo_t& type, int vclass,
                 bool is_public, bool is_static, bool is_stock, Expr* initializer)
 : Decl(pos, name),
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

void
VarDecl::set_init(Expr* expr)
{
    init_ = new BinaryExpr(pos(), '=', new SymbolExpr(pos(), name()), expr);
    init_->set_initializer();
}

Expr*
VarDecl::init_rhs() const
{
    if (!init_)
        return nullptr;
    return init_->right();
}

bool
VarDecl::Analyze(SemaContext& sc)
{
    AutoErrorPos aep(pos_);

    if (gTypes.find(sym_->tag)->kind() == TypeKind::Struct)
        return AnalyzePstruct();

    if (type_.ident == iARRAY || type_.ident == iREFARRAY)
        return CheckArrayDeclaration(sc, this);

    assert(type_.ident == iVARIABLE || type_.ident == iREFERENCE);

    // Since we always create an assignment expression, all type checks will
    // be performed by the Analyze(sc) call here.
    if (init_ && !init_->Analyze(sc))
        return false;

    if (init_ && vclass_ != sLOCAL) {
        if (!init_rhs()->EvalConst(nullptr, nullptr)) {
            if (vclass_ == sARGUMENT && init_rhs()->AsSymbolExpr())
                return true;
            error(init_rhs()->pos(), 8);
        }
    }
    return true;
}

bool
VarDecl::AnalyzePstruct()
{
    if (!init_)
        return true;

    auto init = init_->right()->AsStructExpr();
    assert(init); // If we parse struct initializers as a normal global, this check will need to be
                  // soft.
    auto type = gTypes.find(sym_->tag);
    auto ps = type->asStruct();

    std::vector<bool> visited;
    visited.resize(ps->args.size());

    // Do as much checking as we can before bailing out.
    bool ok = true;
    for (const auto& field : init->fields())
        ok &= AnalyzePstructArg(ps, field, &visited);

    if (!ok)
        return false;

    // Fill in default values as needed.
    for (size_t i = 0; i < visited.size(); i++) {
        if (visited[i])
            continue;
        if (ps->args[i]->type.ident == iREFARRAY) {
            assert(ps->args[i]->type.tag == pc_tag_string);

            auto expr = new StringExpr(pos_, "", 0);
            init->fields().push_back(StructInitField(ps->args[i]->name, expr));
        }
    }

    return true;
}

bool
VarDecl::AnalyzePstructArg(const pstruct_t* ps, const StructInitField& field,
                           std::vector<bool>* visited)
{
    auto arg = pstructs_getarg(ps, field.name);
    if (!arg) {
        report(pos_, 96) << field.name << "struct" << name_;
        return false;
    }

    if (visited->at(arg->index)) {
        error(field.value->pos(), 58);
        return false;
    }

    visited->at(arg->index) = true;

    if (auto expr = field.value->AsStringExpr()) {
        if (arg->type.ident != iREFARRAY) {
            error(expr->pos(), 48);
            return false;
        }
        if (arg->type.tag != pc_tag_string)
            error(expr->pos(), 213, type_to_name(pc_tag_string), type_to_name(arg->type.tag));
    } else if (auto expr = field.value->AsTaggedValueExpr()) {
        if (arg->type.ident != iVARIABLE) {
            error(expr->pos(), 23);
            return false;
        }

        // Proper tag checks were missing in the old parser, and unfortunately
        // adding them breaks older code. As a special case, we allow implicit
        // coercion of constants 0 or 1 to bool.
        if (!(arg->type.tag == pc_tag_bool && expr->tag() == 0 &&
            (expr->value() == 0 || expr->value() == 1)))
        {
            matchtag(arg->type.tag, expr->tag(), MATCHTAG_COERCE);
        }
    } else {
        assert(false);
        return false;
    }
    return true;
}

bool
ConstDecl::Analyze(SemaContext& sc)
{
    AutoErrorPos aep(pos_);

    matchtag(type_.tag, expr_tag_, 0);
    return true;
}

static inline OpFunc TokenToOpFunc(int token) {
    switch (token) {
        case '*':
        case taMULT:
            return os_mult;
        case '/':
        case taDIV:
            return os_div;
        case '%':
        case taMOD:
            return os_mod;
        case '+':
        case taADD:
            return ob_add;
        case '-':
        case taSUB:
            return ob_sub;
        case tSHL:
        case taSHL:
            return ob_sal;
        case tSHR:
        case taSHR:
            return os_sar;
        case tSHRU:
        case taSHRU:
            return ou_sar;
        case '&':
        case taAND:
            return ob_and;
        case '^':
        case taXOR:
            return ob_xor;
        case '|':
        case taOR:
            return ob_or;
        case tlLE:
            return os_le;
        case tlGE:
            return os_ge;
        case '<':
            return os_lt;
        case '>':
            return os_gt;
        case tlEQ:
            return ob_eq;
        case tlNE:
            return ob_ne;
        case '=':
        case tlOR:
        case tlAND:
            return nullptr;
        default:
            assert(false);
            return nullptr;
    }
}

CompareOp::CompareOp(const token_pos_t& pos, int token, Expr* expr)
  : pos(pos),
    token(token),
    expr(expr),
    oper(TokenToOpFunc(token))
{
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

bool
Expr::EvalConst(cell* value, int* tag)
{
    if (val_.ident != iCONSTEXPR) {
        if (!FoldToConstant())
            return false;
        assert(val_.ident == iCONSTEXPR);
    }

    if (value)
        *value = val_.constval;
    if (tag)
        *tag = val_.tag;
    return true;
}

Expr*
Expr::AnalyzeForTest(SemaContext& sc)
{
    if (!Analyze(sc))
        return nullptr;

    if (val_.ident == iARRAY || val_.ident == iREFARRAY) {
        if (val_.sym)
            error(pos_, 33, val_.sym->name());
        else
            error(pos_, 29);
        return nullptr;
    }

    if (val_.tag != 0 || val_.tag != pc_tag_bool) {
        UserOperation userop;
        if (find_userop(lneg, val_.tag, 0, 1, &val_, &userop, pos_.line)) {
            // Call user op for '!', then invert it. EmitTest will fold out the
            // extra invert.
            //
            // First convert to rvalue, since user operators should never
            // taken an lvalue.
            Expr* expr = this;
            if (expr->lvalue())
                expr = new RvalueExpr(expr);

            expr = new CallUserOpExpr(userop, expr);
            expr = new UnaryExpr(expr->pos(), '!', expr);
            expr->val_.ident = iEXPRESSION;
            expr->val_.tag = pc_tag_bool;
            return expr;
        }
    }

    if (val_.ident == iCONSTEXPR) {
        if (val_.constval)
            error(pos_, 206);
        else
            error(pos_, 205);
    }

    if (lvalue())
        return new RvalueExpr(this);

    return this;
}

RvalueExpr::RvalueExpr(Expr* expr)
  : EmitOnlyExpr(expr->pos()),
    expr_(expr)
{
    assert(expr_->lvalue());

    val_ = expr_->val();
    if (val_.ident == iACCESSOR) {
        if (val_.accessor->getter)
            markusage(val_.accessor->getter, uREAD);
        val_.ident = iEXPRESSION;
    }
}

void
RvalueExpr::ProcessUses(SemaContext& sc)
{
    expr_->MarkAndProcessUses(sc);
}

bool
IsDefinedExpr::Analyze(SemaContext& sc)
{
    val_.ident = iCONSTEXPR;
    val_.constval = value_;
    val_.tag = 0;
    return true;
}

bool
UnaryExpr::Analyze(SemaContext& sc)
{
    AutoErrorPos aep(pos_);

    if (!expr_->Analyze(sc))
        return false;

    if (expr_->lvalue())
        expr_ = new RvalueExpr(expr_);
    val_ = expr_->val();

    // :TODO: check for invalid types

    UserOperation userop;
    switch (token_) {
        case '~':
            if (val_.ident == iCONSTEXPR)
                val_.constval = ~val_.constval;
            break;
        case '!':
            if (find_userop(lneg, val_.tag, 0, 1, &val_, &userop, pos_.line)) {
                expr_ = new CallUserOpExpr(userop, expr_);
                val_ = expr_->val();
                userop_ = true;
            } else if (val_.ident == iCONSTEXPR) {
                val_.constval = !val_.constval;
            }
            val_.tag = pc_addtag("bool");
            break;
        case '-':
            if (val_.ident == iCONSTEXPR && val_.tag == sc_rationaltag) {
                float f = sp::FloatCellUnion(val_.constval).f32;
                val_.constval = sp::FloatCellUnion(-f).cell;
            } else if (find_userop(neg, val_.tag, 0, 1, &val_, &userop, pos_.line)) {
                expr_ = new CallUserOpExpr(userop, expr_);
                val_ = expr_->val();
                userop_ = true;
            } else if (val_.ident == iCONSTEXPR) {
                /* the negation of a fixed point number is just an integer negation */
                val_.constval = -val_.constval;
            }
            break;
        default:
            assert(false);
    }

    if (val_.ident != iCONSTEXPR)
        val_.ident = iEXPRESSION;
    return true;
}

bool
UnaryExpr::HasSideEffects()
{
    return expr_->HasSideEffects();
}

void
UnaryExpr::ProcessUses(SemaContext& sc)
{
    expr_->MarkAndProcessUses(sc);
}

bool
IncDecExpr::Analyze(SemaContext& sc)
{
    AutoErrorPos aep(pos_);

    if (!expr_->Analyze(sc))
        return false;
    if (!expr_->lvalue()) {
        error(pos_, 22);
        return false;
    }

    const auto& expr_val = expr_->val();
    if (expr_val.ident != iACCESSOR) {
        if (expr_val.sym->is_const) {
            error(pos_, 22); /* assignment to const argument */
            return false;
        }
    } else {
        if (!expr_val.accessor->setter) {
            report(pos_, 152) << expr_val.accessor->name;
            return false;
        }
        if (!expr_val.accessor->getter) {
            report(pos_, 149) << expr_val.accessor->name;
            return false;
        }
        markusage(expr_val.accessor->getter, uREAD);
        markusage(expr_val.accessor->setter, uREAD);
    }

    auto op = (token_ == tINC) ? user_inc : user_dec;
    find_userop(op, expr_val.tag, 0, 1, &expr_val, &userop_, pos_.line);

    // :TODO: more type checks
    val_.ident = iEXPRESSION;
    val_.tag = expr_val.tag;
    return true;
}

void
IncDecExpr::ProcessUses(SemaContext& sc)
{
    expr_->MarkAndProcessUses(sc);
}

BinaryExprBase::BinaryExprBase(const token_pos_t& pos, int token, Expr* left, Expr* right)
  : Expr(pos),
    token_(token),
    left_(left),
    right_(right)
{
    assert(right_ != this);
}

bool
BinaryExprBase::HasSideEffects()
{
    return left_->HasSideEffects() ||
           right_->HasSideEffects() ||
           IsAssignOp(token_);
}

void
BinaryExprBase::ProcessUses(SemaContext& sc)
{
    // Assign ops, even read/write ones, do not count as variable uses for TestSymbols.
    if (IsAssignOp(token_))
        left_->ProcessUses(sc);
    else
        left_->MarkAndProcessUses(sc);
    right_->MarkAndProcessUses(sc);
}

BinaryExpr::BinaryExpr(const token_pos_t& pos, int token, Expr* left, Expr* right)
  : BinaryExprBase(pos, token, left, right)
{
    oper_ = TokenToOpFunc(token_);
}

bool
BinaryExpr::HasSideEffects()
{
    if (userop_.sym != nullptr)
        return true;
    return BinaryExprBase::HasSideEffects();
}

bool
BinaryExpr::Analyze(SemaContext& sc)
{
    AutoErrorPos aep(pos_);

    if (!left_->Analyze(sc) || !right_->Analyze(sc))
        return false;

    if (IsAssignOp(token_)) {
        // Mark the left-hand side as written as soon as we can.
        if (symbol* sym = left_->val().sym) {
            markusage(sym, uWRITTEN);

            // Update the line number as a hack so we can warn that it was never
            // used.
            sym->lnumber = pos_.line;
        } else if (auto* accessor = left_->val().accessor) {
            if (!accessor->setter) {
                report(pos_, 152) << accessor->name;
                return false;
            }
            markusage(accessor->setter, uREAD);
        }

        if (!ValidateAssignmentLHS())
            return false;
    } else if (left_->lvalue()) {
        left_ = new RvalueExpr(left_);
    }

    // RHS is always loaded. Note we do this after validating the left-hand side,
    // so ValidateAssignment has an original view of RHS.
    if (right_->lvalue())
        right_ = new RvalueExpr(right_);

    const auto& left_val = left_->val();
    const auto& right_val = right_->val();

    if (oper_) {
        assert(token_ != '=');

        if (left_val.ident == iARRAY || left_val.ident == iREFARRAY) {
            const char* ptr = (left_val.sym != nullptr) ? left_val.sym->name() : "-unknown-";
            error(pos_, 33, ptr); /* array must be indexed */
            return false;
        }
        if (right_val.ident == iARRAY || right_val.ident == iREFARRAY) {
            const char* ptr = (right_val.sym != nullptr) ? right_val.sym->name() : "-unknown-";
            error(pos_, 33, ptr); /* array must be indexed */
            return false;
        }
        /* ??? ^^^ should do same kind of error checking with functions */
    }

    // The assignment operator is overloaded separately.
    if (IsAssignOp(token_)) {
        if (!ValidateAssignmentRHS())
            return false;
    }

    val_.ident = iEXPRESSION;
    val_.tag = left_val.tag;

    if (assignop_.sym)
        val_.tag = assignop_.sym->tag;

    if (oper_) {
        if (find_userop(oper_, left_val.tag, right_val.tag, 2, nullptr, &userop_, pos_.line)) {
            val_.tag = userop_.sym->tag;
        } else if (left_val.ident == iCONSTEXPR && right_val.ident == iCONSTEXPR) {
            char boolresult = FALSE;
            matchtag(left_val.tag, right_val.tag, FALSE);
            val_.ident = iCONSTEXPR;
            val_.constval = calc(left_val.constval, oper_, right_val.constval, &boolresult);
        } else {
            // For the purposes of tag matching, we consider the order to be irrelevant.
            if (!checkval_string(&left_val, &right_val))
                matchtag_commutative(left_val.tag, right_val.tag, MATCHTAG_DEDUCE);
        }

        if (IsChainedOp(token_) || token_ == tlEQ || token_ == tlNE)
            val_.tag = pc_addtag("bool");
    }

    return true;
}

bool
BinaryExpr::ValidateAssignmentLHS()
{
    int left_ident = left_->val().ident;
    if (left_ident == iARRAYCHAR) {
        // This is a special case, assigned to a packed character in a cell
        // is permitted.
        return true;
    }

    if (left_ident == iARRAY || left_ident == iREFARRAY) {
        // array assignment is permitted too (with restrictions)
        if (oper_) {
            error(pos_, 23);
            return false;
        }
        symbol* left_sym = left_->val().sym;
        if (!left_sym) {
            error(pos_, 142);
            return false;
        }

        symbol* iter = left_sym;
        while (iter) {
            if (!iter->dim.array.length) {
                error(pos_, 46, left_sym->name());
                return false;
            }
            iter = iter->array_child();
        }
        return true;
    }
    if (!left_->lvalue()) {
        error(pos_, 22);
        return false;
    }

    const auto& left_val = left_->val();
    assert(left_val.sym || left_val.accessor);

    // may not change "constant" parameters
    if (!initializer_ && left_val.sym && left_val.sym->is_const) {
        error(pos_, 22);
        return false;
    }
    return true;
}

bool
BinaryExpr::ValidateAssignmentRHS()
{
    const auto& left_val = left_->val();
    const auto& right_val = right_->val();

    if (left_val.ident == iVARIABLE) {
        const auto& right_val = right_->val();
        if (right_val.ident == iVARIABLE && right_val.sym == left_val.sym)
            error(pos_, 226, left_val.sym->name()); // self-assignment
    }

    // :TODO: check this comment post-enumstructectomy
    /* Array elements are sometimes considered as sub-arrays --when the
     * array index is an enumeration field and the enumeration size is greater
     * than 1. If the expression on the right side of the assignment is a cell,
     * or if an operation is in effect, this does not apply.
     */
    bool leftarray = left_val.ident == iARRAY ||
                     left_val.ident == iREFARRAY ||
                     ((left_val.ident == iARRAYCELL || left_val.ident == iARRAYCHAR) &&
                       left_val.constval > 1 &&
                       left_val.sym->dim.array.level == 0 &&
                       !oper_ &&
                       (right_val.ident == iARRAY || right_val.ident == iREFARRAY));
    if (leftarray) {
        if (right_val.ident != iARRAY && right_val.ident != iREFARRAY) {
            error(pos_, 47);
            return false;
        }

        bool exact_match = true;
        cell right_length = 0;
        int right_idxtag = 0;
        int left_length = left_val.sym->dim.array.length;
        if (right_val.sym) {
            // Change from the old logic - we immediately reject multi-dimensional
            // arrays in assignment and don't bother validating subarray assignment.
            if (right_val.sym->dim.array.level > 0) {
                error(pos_, 23);
                return false;
            }

            if (right_val.constval == 0)
                right_length = right_val.sym->dim.array.length; // array variable
            else
                right_length = right_val.constval;

            right_idxtag = right_val.sym->x.tags.index;
            if (right_idxtag == 0 && left_val.sym->x.tags.index == 0)
                exact_match = false;
        } else {
            right_length = right_val.constval; // literal array

            // If val is negative, it means that lval2 is a literal string.
            // The string array size may be smaller than the destination
            // array, provided that the destination array does not have an
            // index tag.
            if (right_length < 0) {
                right_length = -right_length;
                if (left_val.sym->x.tags.index == 0)
                    exact_match = false;
            }
        }
        if (left_val.sym->dim.array.level != 0) {
            error(pos_, 47); // array dimensions must match
            return false;
        }
        if (left_length < right_length || (exact_match && left_length > right_length) ||
            right_length == 0)
        {
            error(pos_, 47); // array sizes must match
            return false;
        }
        if (left_val.ident != iARRAYCELL &&
            !matchtag(left_val.sym->x.tags.index, right_idxtag, MATCHTAG_COERCE | MATCHTAG_SILENT))
        {
            error(pos_, 229, right_val.sym ? right_val.sym->name() : left_val.sym->name());
        }

        array_copy_length_ = right_length;
        if (left_val.sym->tag == pc_tag_string)
            array_copy_length_ = char_array_cells(array_copy_length_);
    } else {
        if (right_val.ident == iARRAY || right_val.ident == iREFARRAY) {
            error(pos_, 6); // must be assigned to an array
            return false;
        }

        // Userop tag will be propagated by the caller.
        find_userop(nullptr, left_val.tag, right_val.tag, 2, &left_val, &assignop_, pos_.line);
    }

    if (!oper_ && !checkval_string(&left_val, &right_val)) {
        if ((left_val.tag == pc_tag_string && right_val.tag != pc_tag_string) ||
            (left_val.tag != pc_tag_string && right_val.tag == pc_tag_string))
        {
            error(pos_, 179, type_to_name(left_val.tag), type_to_name(right_val.tag));
            return false;
        }
        matchtag(left_val.tag, right_val.tag, TRUE);
    }
    return true;
}

static inline bool
IsTypeBinaryConstantFoldable(Type* type)
{
    if (type->isEnum() || type->tagid() == 0)
        return true;
    return false;
}

bool
BinaryExpr::FoldToConstant()
{
    cell left_val, right_val;
    int left_tag, right_tag;

    if (!left_->EvalConst(&left_val, &left_tag) || !right_->EvalConst(&right_val, &right_tag))
        return false;
    if (IsAssignOp(token_) || userop_.sym)
        return false;

    Type* left_type = gTypes.find(left_tag);
    Type* right_type = gTypes.find(right_tag);
    if (!IsTypeBinaryConstantFoldable(left_type) || !IsTypeBinaryConstantFoldable(right_type))
        return false;

    switch (token_) {
        case '*':
            val_.constval = left_val * right_val;
            break;
        case '/':
        case '%':
            if (!right_val) {
                error(pos_, 93);
                return false;
            }
            if (left_val == cell(0x80000000) && right_val == -1) {
                error(pos_, 97);
                return false;
            }
            if (token_ == '/')
                val_.constval = left_val / right_val;
            else
                val_.constval = left_val % right_val;
            break;
        case '+':
            val_.constval = left_val + right_val;
            break;
        case '-':
            val_.constval = left_val - right_val;
            break;
        case tSHL:
            val_.constval = left_val << right_val;
            break;
        case tSHR:
            val_.constval = left_val >> right_val;
            break;
        case tSHRU:
            val_.constval = uint32_t(left_val) >> uint32_t(right_val);
            break;
        case '&':
            val_.constval = left_val & right_val;
            break;
        case '^':
            val_.constval = left_val ^ right_val;
            break;
        case '|':
            val_.constval = left_val | right_val;
            break;
        default:
            return false;
    }

    val_.ident = iCONSTEXPR;
    return true;
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

bool
LogicalExpr::Analyze(SemaContext& sc)
{
    AutoErrorPos aep(pos_);

    if (!left_->Analyze(sc) || !right_->Analyze(sc))
        return false;

    if (left_->lvalue())
        left_ = new RvalueExpr(left_);
    if (right_->lvalue())
        right_ = new RvalueExpr(right_);

    const auto& left_val = left_->val();
    const auto& right_val = right_->val();
    if (left_val.ident == iCONSTEXPR && right_val.ident == iCONSTEXPR) {
        val_.ident = iCONSTEXPR;
        if (token_ == tlOR)
            val_.constval = (left_val.constval || right_val.constval);
        else if (token_ == tlAND)
            val_.constval = (left_val.constval && right_val.constval);
        else
            assert(false);
    } else {
        val_.ident = iEXPRESSION;
    }
    val_.sym = nullptr;
    val_.tag = pc_addtag("bool");
    return true;
}

bool
ChainedCompareExpr::HasSideEffects()
{
    if (first_->HasSideEffects())
        return true;
    for (const auto& op : ops_) {
        if (op.userop.sym || op.expr->HasSideEffects())
            return true;
    }
    return false;
}

bool
ChainedCompareExpr::Analyze(SemaContext& sc)
{
    if (!first_->Analyze(sc))
        return false;
    if (first_->lvalue())
        first_ = new RvalueExpr(first_);

    for (auto& op : ops_) {
        if (!op.expr->Analyze(sc))
            return false;
        if (op.expr->lvalue())
            op.expr = new RvalueExpr(op.expr);
    }

    Expr* left = first_;
    bool all_const = (left->val().ident == iCONSTEXPR);
    bool constval = true;

    val_.ident = iEXPRESSION;
    val_.tag = pc_tag_bool;

    for (auto& op : ops_) {
        Expr* right = op.expr;
        const auto& left_val = left->val();
        const auto& right_val = right->val();

        if (left_val.ident == iARRAY || left_val.ident == iREFARRAY) {
            const char* ptr = (left_val.sym != nullptr) ? left_val.sym->name() : "-unknown-";
            error(pos_, 33, ptr); /* array must be indexed */
            return false;
        }
        if (right_val.ident == iARRAY || right_val.ident == iREFARRAY) {
            const char* ptr = (right_val.sym != nullptr) ? right_val.sym->name() : "-unknown-";
            error(pos_, 33, ptr); /* array must be indexed */
            return false;
        }

        if (find_userop(op.oper, left_val.tag, right_val.tag, 2, nullptr, &op.userop, pos_.line)) {
            if (op.userop.sym->tag != pc_tag_bool) {
                report(op.pos, 51) << get_token_string(op.token);
                return false;
            }
        } else {
            // For the purposes of tag matching, we consider the order to be irrelevant.
            if (!checkval_string(&left_val, &right_val))
                matchtag_commutative(left_val.tag, right_val.tag, MATCHTAG_DEDUCE);
        }

        if (right_val.ident != iCONSTEXPR || op.userop.sym)
            all_const = false;

        // Fold constants as we go.
        if (all_const) {
            switch (op.token) {
                case tlLE:
                    constval &= left_val.constval <= right_val.constval;
                    break;
                case tlGE:
                    constval &= left_val.constval >= right_val.constval;
                    break;
                case '>':
                    constval &= left_val.constval > right_val.constval;
                    break;
                case '<':
                    constval &= left_val.constval < right_val.constval;
                    break;
                default:
                    assert(false);
                    break;
            }
        }

        left = right;
    }

    if (all_const) {
        val_.ident = iCONSTEXPR;
        val_.constval = constval ? 1 :0;
    }
    return true;
}

void
ChainedCompareExpr::ProcessUses(SemaContext& sc)
{
    first_->MarkAndProcessUses(sc);
    for (const auto& op : ops_)
        op.expr->MarkAndProcessUses(sc);
}

bool
TernaryExpr::Analyze(SemaContext& sc)
{
    AutoErrorPos aep(pos_);

    if (!first_->Analyze(sc) || !second_->Analyze(sc) || !third_->Analyze(sc))
        return false;

    if (first_->lvalue()) {
        first_ = new RvalueExpr(first_);
    } else if (first_->val().ident == iCONSTEXPR) {
        error(pos_, first_->val().constval ? 206 : 205);
    }

    if (second_->lvalue())
        second_ = new RvalueExpr(second_);
    if (third_->lvalue())
        third_ = new RvalueExpr(third_);

    const auto& left = second_->val();
    const auto& right = third_->val();
    bool left_array = (left.ident == iARRAY || right.ident == iREFARRAY);
    bool right_array = (left.ident == iARRAY || right.ident == iREFARRAY);
    if (!left_array && right_array) {
        const char* ptr = "-unknown-";
        if (left.sym != nullptr)
            ptr = left.sym->name();
        error(pos_, 33, ptr); /* array must be indexed */
        return false;
    } else if (left_array && !right_array) {
        const char* ptr = "-unknown-";
        if (right.sym != nullptr)
            ptr = right.sym->name();
        error(pos_, 33, ptr); /* array must be indexed */
        return false;
    }

    if (!matchtag_commutative(left.tag, right.tag, FALSE))
        return false;

    /* If both sides are arrays, we should return the maximal as the lvalue.
     * Otherwise we could buffer overflow and the compiler is too stupid.
     * Literal strings have a constval == -(num_cells) so the cmp is flipped.
     */
    val_ = left;
    if (val_.ident == iARRAY && right.ident == iARRAY && val_.constval < 0 && val_.constval > right.constval)
        val_ = right;

    if (val_.ident == iARRAY)
        val_.ident = iREFARRAY;
    else if (val_.ident != iREFARRAY)
        val_.ident = iEXPRESSION;
    return true;
}

bool
TernaryExpr::FoldToConstant()
{
    cell cond, left, right;
    if (!first_->EvalConst(&cond, nullptr) || second_->EvalConst(&left, nullptr) ||
        !third_->EvalConst(&right, nullptr))
    {
        return false;
    }

    val_.constval = cond ? left : right;
    val_.ident = iCONSTEXPR;
    return true;
}

void
TernaryExpr::ProcessUses(SemaContext& sc)
{
    first_->MarkAndProcessUses(sc);
    second_->MarkAndProcessUses(sc);
    third_->MarkAndProcessUses(sc);
}

void
TernaryExpr::ProcessDiscardUses(SemaContext& sc)
{
    first_->MarkAndProcessUses(sc);
    second_->ProcessUses(sc);
    third_->ProcessUses(sc);
}

bool
CastExpr::Analyze(SemaContext& sc)
{
    AutoErrorPos aep(pos_);

    if (tag_ == pc_tag_void) {
        error(pos_, 144);
        return false;
    }

    if (!expr_->Analyze(sc))
        return false;

    val_ = expr_->val();
    lvalue_ = expr_->lvalue();

    Type* ltype = gTypes.find(val_.tag);
    Type* atype = gTypes.find(tag_);
    if (ltype->isObject() || atype->isObject()) {
        matchtag(tag_, val_.tag, MATCHTAG_COERCE);
    } else if (ltype->isFunction() != atype->isFunction()) {
        // Warn: unsupported cast.
        error(pos_, 237);
    } else if (val_.sym && val_.sym->tag == pc_tag_void) {
        error(pos_, 89);
    } else if (atype->isEnumStruct()) {
        report(pos_, 95) << atype->name();
    }
    val_.tag = tag_;
    return true;
}

void
CastExpr::ProcessUses(SemaContext& sc)
{
    expr_->MarkAndProcessUses(sc);
}

void
SymbolExpr::MarkUsed(SemaContext& sc)
{
    markusage(sym_, uREAD);
}

bool
SymbolExpr::Analyze(SemaContext& sc)
{
    return AnalyzeWithOptions(sc, false);
}

// This is a hack. Most code is not prepared to handle iMETHODMAP in type
// checks, so for now, we forbid it by default. Since the '.' operator *is*
// prepared for this, we have a special analysis option to allow returning
// types as values.
bool
SymbolExpr::AnalyzeWithOptions(SemaContext& sc, bool allow_types)
{
    AutoErrorPos aep(pos_);

    val_.ident = sym_->ident;
    val_.sym = sym_;
    val_.tag = sym_->tag;

    if (sym_->ident == iCONSTEXPR)
        val_.constval = sym_->addr();

    if (sym_->vclass == sGLOBAL && sym_->ident != iFUNCTN) {
        if (!sym_->defined) {
            report(pos_, 17) << sym_->name();
            return false;
        }
    }
    if (sym_->ident == iFUNCTN) {
        // If the function is only in the table because it was inserted as
        // a stub in the first pass (used but never declared or implemented),
        // issue an error.
        if (!sym_->prototyped && sym_ != sc.func())
            report(pos_, 17) << sym_->name();

        if (sym_->native) {
            error(pos_, 76);
            return false;
        }
        if (sym_->array_return()) {
            error(pos_, 182);
            return false;
        }
        if (sym_->missing && sym_ != sc.func()) {
            auto symname = funcdisplayname(sym_->name());
            report(pos_, 4) << symname;
            return false;
        }

        funcenum_t* fe = funcenum_for_symbol(sym_);

        // New-style "closure".
        val_.ident = iEXPRESSION;
        val_.tag = fe->tag;
    }

    switch (sym_->ident) {
        case iVARIABLE:
        case iREFERENCE:
            lvalue_ = true;
            break;
        case iARRAY:
        case iREFARRAY:
        case iCONSTEXPR:
        case iFUNCTN:
            // Not an l-value.
            break;
        case iMETHODMAP:
        case iENUMSTRUCT:
            if (!allow_types) {
                error(pos_, 174, sym_->name());
                return false;
            }
            break;
        default:
            // Should not be a symbol.
            assert(false);
    }
    return true;
}

bool
CommaExpr::Analyze(SemaContext& sc)
{
    AutoErrorPos aep(pos_);

    for (const auto& expr : exprs_) {
        if (!expr->Analyze(sc))
            return false;
        has_side_effects_ |= expr->HasSideEffects();
    }

    Expr* last = exprs_.back();
    if (exprs_.size() > 1 && last->lvalue()) {
        last = new RvalueExpr(last);
        exprs_.back() = last;
    }

    val_ = last->val();
    lvalue_ = last->lvalue();

    // Don't propagate a constant if it would cause Emit() to shortcut and not
    // emit other expressions.
    if (exprs_.size() > 1 && val_.ident == iCONSTEXPR)
        val_.ident = iEXPRESSION;
    return true;
}

void
CommaExpr::ProcessUses(SemaContext& sc)
{
    for (const auto& expr : exprs_)
        expr->ProcessUses(sc);
    exprs_.back()->MarkUsed(sc);
}

void
CommaExpr::ProcessDiscardUses(SemaContext& sc)
{
    for (const auto& expr : exprs_)
        expr->ProcessUses(sc);
}

bool
ArrayExpr::Analyze(SemaContext& sc)
{
    AutoErrorPos aep(pos_);

    int lasttag = -1;
    for (const auto& expr : exprs_) {
        if (!expr->Analyze(sc))
            return false;

        const auto& val = expr->val();
        if (val.ident != iCONSTEXPR) {
            error(pos_, 8);
            return false;
        }
        if (lasttag < 0)
            lasttag = val.tag;
        else
            matchtag(lasttag, val.tag, FALSE);
    }

    val_.ident = iARRAY;
    val_.constval = exprs_.size();
    val_.tag = lasttag;
    return true;
}

bool
IndexExpr::Analyze(SemaContext& sc)
{
    AutoErrorPos aep(pos_);

    if (!base_->Analyze(sc) || !expr_->Analyze(sc))
        return false;
    if (base_->lvalue() && base_->val().ident == iACCESSOR)
        base_ = new RvalueExpr(base_);
    if (expr_->lvalue())
        expr_ = new RvalueExpr(expr_);

    const auto& base = base_->val();
    if (!base.sym) {
        error(pos_, 29);
        return false;
    }
    if (base.sym->ident != iARRAY && base.sym->ident != iREFARRAY) {
        error(pos_, 28, base.sym->name());
        return false;
    }

    if (base.sym->enumroot) {
        if (!matchtag(base.sym->x.tags.index, expr_->val().tag, TRUE))
            return false;
    }

    const auto& expr = expr_->val();
    if (expr.ident == iARRAY || expr.ident == iREFARRAY) {
        error(pos_, 33, expr.sym ? expr.sym->name() : "-unknown-"); /* array must be indexed */
        return false;
    }

    if (gTypes.find(base.sym->x.tags.index)->isEnumStruct()) {
        error(pos_, 117);
        return false;
    }

    int idx_tag = expr_->val().tag;
    if (!is_valid_index_tag(idx_tag)) {
        error(pos_, 77, gTypes.find(idx_tag)->prettyName());
        return false;
    }

    val_ = base_->val();

    if (expr.ident == iCONSTEXPR) {
        if (!(base.sym->tag == pc_tag_string && base.sym->dim.array.level == 0)) {
            /* normal array index */
            if (expr.constval < 0 ||
                (base.sym->dim.array.length != 0 && base.sym->dim.array.length <= expr.constval))
            {
                error(pos_, 32, base.sym->name()); /* array index out of bounds */
                return false;
            }
        } else {
            /* character index */
            if (expr.constval < 0 ||
                (base.sym->dim.array.length != 0 &&
                 base.sym->dim.array.length <= (ucell)expr.constval))
            {
                error(pos_, 32, base.sym->name()); /* array index out of bounds */
                return false;
            }
        }
        /* if the array index is a field from an enumeration, get the tag name
         * from the field and save the size of the field too.
         */
        assert(expr.sym == nullptr || expr.sym->dim.array.level == 0);
    }

    if (base.sym->dim.array.level > 0) {
        // Note: Intermediate arrays are not l-values.
        val_.ident = iREFARRAY;
        val_.sym = base.sym->array_child();

        assert(val_.sym != nullptr);
        assert(val_.sym->dim.array.level == base.sym->dim.array.level - 1);
        return true;
    }

    /* set type to fetch... INDIRECTLY */
    if (base.sym->tag == pc_tag_string)
        val_.ident = iARRAYCHAR;
    else
        val_.ident = iARRAYCELL;

    val_.tag = base.sym->tag;
    val_.constval = 0;

    lvalue_ = true;
    return true;
}

void
IndexExpr::ProcessUses(SemaContext& sc)
{
    base_->MarkAndProcessUses(sc);
    expr_->MarkAndProcessUses(sc);
}

bool
ThisExpr::Analyze(SemaContext& sc)
{
    assert(sym_->ident == iREFARRAY || sym_->ident == iVARIABLE);

    val_.ident = sym_->ident;
    val_.sym = sym_;
    val_.tag = sym_->tag;
    lvalue_ = (sym_->ident != iREFARRAY);
    return true;
}

bool
NullExpr::Analyze(SemaContext& sc)
{
    val_.ident = iCONSTEXPR;
    val_.constval = 0;
    val_.tag = pc_tag_null_t;
    return true;
}

bool
TaggedValueExpr::Analyze(SemaContext& sc)
{
    val_.ident = iCONSTEXPR;
    val_.tag = tag_;
    val_.constval = value_;
    return true;
}

bool
StringExpr::Analyze(SemaContext& sc)
{
    val_.ident = iARRAY;
    val_.constval = -((cell)text_->length() + 1);
    val_.tag = pc_tag_string;
    return true;
}

bool FieldAccessExpr::Analyze(SemaContext& sc) {
    return AnalyzeWithOptions(sc, false);
}

bool
FieldAccessExpr::AnalyzeWithOptions(SemaContext& sc, bool from_call)
{
    AutoErrorPos aep(pos_);

    if (SymbolExpr* expr = base_->AsSymbolExpr()) {
        if (!expr->AnalyzeWithOptions(sc, true))
            return false;
    } else {
        if (!base_->Analyze(sc))
            return false;
    }

    if (token_ == tDBLCOLON)
        return AnalyzeStaticAccess();

    const auto& base_val = base_->val();
    switch (base_val.ident) {
        case iARRAY:
        case iREFARRAY:
            if (base_val.sym && base_val.sym->dim.array.level == 0) {
                Type* type = gTypes.find(base_val.sym->x.tags.index);
                if (symbol* root = type->asEnumStruct())
                    return AnalyzeEnumStructAccess(type, root, from_call);
            }
            report(pos_, 96) << name_->chars() << "type" << "array";
            return false;
        case iFUNCTN:
            error(pos_, 107);
            return false;
    }

    if (base_val.ident == iMETHODMAP) {
        methodmap_t* map = base_val.sym->methodmap;
        if (map)
            method_ = methodmap_find_method(map, name_);
        if (!method_) {
            report(pos_, 105) << base_val.sym->name() << name_;
            return false;
        }
        if (!method_->is_static) {
            report(pos_, 176) << method_->name << map->name;
            return false;
        }
        val_.ident = iFUNCTN;
        val_.sym = method_->target;
        markusage(method_->target, uREAD);
        return true;
    }

    Type* base_type = gTypes.find(base_val.tag);
    methodmap_t* map = base_type->asMethodmap();
    if (!map) {
        error(pos_, 104, type_to_name(base_val.tag));
        return false;
    }

    method_ = methodmap_find_method(map, name_);
    if (!method_) {
        report(pos_, 105) << map->name << name_;
        return false;
    }

    if (method_->getter || method_->setter) {
        // This is the only scenario in which we need to compute a load of the
        // base address. Otherwise, we're only accessing the type.
        if (base_->lvalue())
            base_ = new RvalueExpr(base_);
        val_.ident = iACCESSOR;
        val_.tag = method_->property_tag();
        val_.accessor = method_;
        lvalue_ = true;
        return true;
    }

    if (method_->is_static) {
        report(pos_, 177) << method_->name << map->name << method_->name;
        return false;
    }

    val_.ident = iFUNCTN;
    val_.sym = method_->target;
    markusage(method_->target, uREAD);
    return true;
}

void
FieldAccessExpr::ProcessUses(SemaContext& sc)
{
    base_->MarkAndProcessUses(sc);
}

symbol*
FieldAccessExpr::BindCallTarget(SemaContext& sc, int token, Expr** implicit_this)
{
    if (!AnalyzeWithOptions(sc, true))
        return nullptr;
    if (val_.ident != iFUNCTN)
        return nullptr;

    // The static accessor (::) is offsetof(), so it can't return functions.
    assert(token_ == '.');

    if (method_ && method_->parent->ctor == method_) {
        report(pos_, 84) << method_->parent->name;
        return nullptr;
    }

    if (base_->lvalue())
        base_ = new RvalueExpr(base_);
    if (field_ || !method_->is_static)
        *implicit_this = base_;
    return val_.sym;
}

symbol*
SymbolExpr::BindCallTarget(SemaContext& sc, int token, Expr** implicit_this)
{
    AutoErrorPos aep(pos_);

    *implicit_this = nullptr;

    if (token != tNEW && sym_->ident == iMETHODMAP && sym_->methodmap) {
        if (!sym_->methodmap->ctor) {
            // Immediately fatal - no function to call.
            error(pos_, 172, sym_->name());
            return nullptr;
        }
        if (sym_->methodmap->must_construct_with_new()) {
            // Keep going, this is basically a style thing.
            report(pos_, 170) << sym_->methodmap->name;
            return nullptr;
        }
        return sym_->methodmap->ctor->target;
    }
    if (sym_->ident != iFUNCTN)
        return nullptr;
    if (sym_->missing) {
        auto symname = funcdisplayname(sym_->name());
        error(pos_, 4, symname.c_str());
        return nullptr;
    }
    return sym_;
}

symbol*
SymbolExpr::BindNewTarget(SemaContext& sc)
{
    AutoErrorPos aep(pos_);

    if (sym_->ident != iMETHODMAP) {
        error(pos_, 116, sym_->name());
        return nullptr;
    }

    methodmap_t* methodmap = sym_->methodmap;
    if (!methodmap->must_construct_with_new()) {
        report(pos_, 171) << methodmap->name;
        return nullptr;
    }
    if (!methodmap->ctor) {
        report(pos_, 172) << methodmap->name;
        return nullptr;
    }
    return methodmap->ctor->target;
}

bool
FieldAccessExpr::AnalyzeEnumStructAccess(Type* type, symbol* root, bool from_call)
{
    // Enum structs are always arrays, so they're never l-values.
    assert(!base_->lvalue());

    field_ = FindEnumStructField(type, name_);
    if (!field_) {
        report(pos_, 105) << type->name() << name_;
        return false;
    }
    if (field_->ident == iFUNCTN) {
        if (!from_call) {
            error(pos_, 76);
            return false;
        }

        val_.ident = iFUNCTN;
        val_.sym = field_;
        markusage(val_.sym, uREAD);
        return true;
    }
    assert(field_->parent() == root);

    int tag = field_->x.tags.index;

    symbol* var = base_->val().sym;
    if (!var->data())
        var->set_data(std::make_unique<EnumStructVarData>());

    EnumStructVarData* es_var = var->data()->asEnumStructVar();
    es_var->children.push_back(std::make_unique<symbol>(*field_));

    symbol* child = es_var->children.back().get();
    child->setName(name_);
    child->vclass = var->vclass;

    if (gTypes.find(tag)->isEnumStruct()) {
        val_.tag = 0;
        child->tag = 0;
        child->x.tags.index = tag;
    } else {
        val_.tag = tag;
        child->tag = tag;
        child->x.tags.index = 0;
    }

    if (field_->dim.array.length > 0) {
        child->dim.array.length = field_->dim.array.length;
        child->dim.array.level = 0;
        child->ident = iREFARRAY;
        val_.constval = field_->dim.array.length;
    } else {
        child->ident = (tag == pc_tag_string) ? iARRAYCHAR : iARRAYCELL;
        val_.constval = 0;
        lvalue_ = true;
    }
    val_.ident = child->ident;
    val_.sym = child;
    return true;
}

bool
FieldAccessExpr::AnalyzeStaticAccess()
{
    AutoErrorPos aep(pos_);

    const auto& base_val = base_->val();
    if (base_val.ident != iENUMSTRUCT) {
        error(pos_, 108);
        return false;
    }

    Type* type = gTypes.find(base_val.tag);
    symbol* field = FindEnumStructField(type, name_);
    if (!field) {
        report(pos_, 105) << type->name() << name_;
        return FALSE;
    }
    assert(field->parent() == type->asEnumStruct());

    val_.ident = iCONSTEXPR;
    val_.sym = nullptr;
    val_.constval = field->addr();
    val_.tag = 0;
    return true;
}

bool
FieldAccessExpr::HasSideEffects()
{
    return base_->HasSideEffects() || val_.ident == iACCESSOR;
}

bool
SizeofExpr::Analyze(SemaContext& sc)
{
    AutoErrorPos aep(pos_);

    symbol* sym = sym_;

    markusage(sym, uREAD);

    if (sym->ident == iCONSTEXPR) {
        error(pos_, 39); // constant symbol has no size
        return false;
    } else if (sym->ident == iFUNCTN) {
        error(pos_, 72); // "function" symbol has no size
        return false;
    } else if (!sym->defined) {
        report(pos_, 17) << ident_;
        return false;
    }

    val_.ident = iCONSTEXPR;
    val_.constval = 1;

    if (sym->ident == iARRAY || sym->ident == iREFARRAY || sym->ident == iENUMSTRUCT) {
        symbol* subsym = sym;
        for (int level = 0; level < array_levels_; level++) {
            // Forbid index operations on enum structs.
            if (sym->ident == iENUMSTRUCT || gTypes.find(sym->x.tags.index)->isEnumStruct()) {
                error(pos_, 111, sym->name());
                return false;
            }
            if (subsym)
                subsym = subsym->array_child();
        }

        Type* enum_type = nullptr;
        if (suffix_token_ == tDBLCOLON) {
            if (subsym->ident != iENUMSTRUCT) {
                error(pos_, 112, subsym->name());
                return false;
            }
            enum_type = gTypes.find(subsym->tag);
        } else if (suffix_token_ == '.') {
            enum_type = gTypes.find(subsym->x.tags.index);
            if (!enum_type->asEnumStruct()) {
                error(pos_, 116, sym->name());
                return false;
            }
        }

        if (enum_type) {
            assert(enum_type->asEnumStruct());

            symbol* field = FindEnumStructField(enum_type, field_);
            if (!field) {
                report(pos_, 105) << enum_type->name() << field_;
                return false;
            }
            if (int array_size = field->dim.array.length) {
                val_.constval = array_size;
                return true;
            }
            return true;
        }

        if (sym->ident == iENUMSTRUCT) {
            if (!sym->dim.enumlist) {
                error(pos_, 19, sym->name());
                return false;
            }
            val_.constval = sym->addr();
            return true;
        }

        if (array_levels_ > sym->dim.array.level + 1) {
            error(pos_, 28, sym->name()); // invalid subscript
            return false;
        }
        if (array_levels_ != sym->dim.array.level + 1) {
            symbol* iter = sym;
            int level = array_levels_;
            while (level-- > 0)
                iter = iter->array_child();

            if (!iter->dim.array.length) {
                error(pos_, 163, sym->name()); // indeterminate array size in "sizeof"
                return false;
            }
            val_.constval = iter->dim.array.length;
            return true;
        }
    }
    return true;
}

CallUserOpExpr::CallUserOpExpr(const UserOperation& userop, Expr* expr)
  : EmitOnlyExpr(expr->pos()),
    userop_(userop),
    expr_(expr)
{
    val_.ident = iEXPRESSION;
    val_.tag = userop_.sym->tag;
}

void
CallUserOpExpr::ProcessUses(SemaContext& sc)
{
    expr_->MarkAndProcessUses(sc);
}

DefaultArgExpr::DefaultArgExpr(const token_pos_t& pos, arginfo* arg)
  : EmitOnlyExpr(pos),
    arg_(arg)
{
    // Leave val bogus, it doesn't participate in anything, and we can't
    // accurately construct it.
}

bool
CallExpr::Analyze(SemaContext& sc)
{
    AutoErrorPos aep(pos_);

    // Note: we do not Analyze the call target. We leave this to the
    // implementation of BindCallTarget.
    if (token_ == tNEW)
        sym_ = target_->BindNewTarget(sc);
    else
        sym_ = target_->BindCallTarget(sc, token_, &implicit_this_);
    if (!sym_) {
        error(pos_, 12);
        return false;
    }

    markusage(sym_, uREAD);

    // If we're calling a stock in the 2nd pass, and it was never defined as
    // read, then we're encountering some kind of compiler bug. If we're not
    // supposed to emit this code than the status should be statSKIP - so
    // we're generating code that will jump to the wrong address.
    if (sym_->stock && !(sym_->usage & uREAD) && sc_status == statWRITE) {
        error(pos_, FATAL_ERROR_NO_GENERATED_CODE, sym_->name());
        return false;
    }

    val_.ident = iEXPRESSION;
    val_.tag = sym_->tag;
    if (sym_->array_return()) {
        val_.ident = iREFARRAY;
        val_.sym = sym_->array_return();
    }

    if (sym_->deprecated) {
        const char* ptr = sym_->documentation.c_str();
        error(pos_, 234, sym_->name(), ptr); /* deprecated (probably a native function) */
    }

    unsigned int nargs = 0;
    unsigned int argidx = 0;
    arginfo* arglist = &sym_->function()->args[0];
    if (implicit_this_) {
        if (!ProcessArg(&arglist[0], implicit_this_, 0))
            return false;
        nargs++;
        argidx++;
    }

    bool namedparams = false;
    for (const auto& param : args_) {
        unsigned int argpos;
        if (param.name) {
            int pos = findnamedarg(arglist, param.name);
            if (pos < 0) {
                report(pos_, 17) << param.name;
                break;
            }
            argpos = pos;
            argidx = pos;
        } else {
            if (namedparams) {
                error(pos_, 44); // positional parameters must precede named parameters
                return false;
            }
            argpos = nargs;
        }

        if (argpos >= SP_MAX_CALL_ARGUMENTS) {
            error(pos_, 45); // too many function arguments
            return false;
        }
        if (argpos < argv_.size() && argv_[argpos].expr) {
            error(pos_, 58); // argument already set
            return false;
        }
        // Note: we don't do this in ProcessArg, since we don't want to double-call
        // analyze on implicit_this (Analyze is not idempotent).
        if (param.expr && !param.expr->Analyze(sc))
            return false;

        // Add the argument to |argv_| and perform type checks.
        if (!ProcessArg(&arglist[argidx], param.expr, argpos))
            return false;

        assert(argv_[argpos].expr != nullptr);
        nargs++;

        // Don't iterate past terminators (0 or varargs).
        switch (arglist[argidx].type.ident) {
            case 0:
            case iVARARGS:
                break;
            default:
                argidx++;
                break;
        }
    }

    if (!sc.func()) {
        error(pos_, 10);
        return false;
    }

    // Check for missing or invalid extra arguments, and fill in default
    // arguments.
    for (unsigned int argidx = 0; ; argidx++) {
        auto& arg = arglist[argidx];
        if (arg.type.ident == 0 || arg.type.ident == iVARARGS)
            break;
        if (argidx >= argv_.size() || !argv_[argidx].expr) {
            if (!ProcessArg(&arg, nullptr, argidx))
                return false;
        }

        Expr* expr = argv_[argidx].expr;
        if (expr->AsDefaultArgExpr() && arg.type.ident == iVARIABLE) {
            UserOperation userop;
            if (find_userop(nullptr, arg.def->tag, arg.type.tag, 2, nullptr, &userop, pos_.line))
                argv_[argidx].expr = new CallUserOpExpr(userop, expr);
        }
    }

    return true;
}

bool
CallExpr::ProcessArg(arginfo* arg, Expr* param, unsigned int pos)
{
    while (pos >= argv_.size())
        argv_.push_back(ComputedArg{});

    unsigned int visual_pos = implicit_this_ ? pos : pos + 1;

    if (!param) {
        if (arg->type.ident == 0 || arg->type.ident == iVARARGS) {
            error(pos_, 92); // argument count mismatch
            return false;
        }
        if (!arg->def) {
            error(pos_, 34, visual_pos); // argument has no default value
            return false;
        }

        // The rest of the code to handle default values is in DoEmit.
        argv_[pos].expr = new DefaultArgExpr(pos_, arg);
        argv_[pos].arg = arg;
        return true;
    }

    AutoErrorPos aep(param->pos());

    bool handling_this = implicit_this_ && (pos == 0);

    if (param->val().ident == iACCESSOR) {
        // We must always compute r-values for accessors.
        if (!param->val().accessor->getter) {
            report(param->pos(), 149) << param->val().accessor->name;
            return false;
        }
        param = new RvalueExpr(param);
    }

    const auto* val = &param->val();
    bool lvalue = param->lvalue();
    switch (arg->type.ident) {
        case 0:
            // On the first pass, we don't have all of the parameter info.
            // However, use information must be marked anyway, otherwise
            // vars declared previously will be omitted in the second pass.
            // See SourceMod bug 4643.
            error(pos_, 92); // argument count mismatch
            break;
        case iVARARGS:
            assert(!handling_this);

            // Always pass by reference.
            if (val->ident == iVARIABLE || val->ident == iREFERENCE) {
                if (val->sym->is_const && !arg->type.is_const) {
                    // Treat a "const" variable passed to a function with a
                    // non-const "variable argument list" as a constant here.
                    if (!lvalue) {
                        error(pos_, 22); // need lvalue
                        return false;
                    }
                }
            }
            if (!checktag_string(arg->type.tag, val) && !checktag(arg->type.tag, val->tag))
                error(pos_, 213, type_to_name(arg->type.tag), type_to_name(val->tag));
            break;
        case iVARIABLE:
        {
            if (val->ident == iFUNCTN || val->ident == iARRAY || val->ident == iREFARRAY) {
                error(pos_, 35, visual_pos); // argument type mismatch
                return false;
            }

            if (lvalue) {
                param = new RvalueExpr(param);
                val = &param->val();
            }

            // Do not allow user operators to transform |this|.
            UserOperation userop;
            if (!handling_this &&
                find_userop(nullptr, val->tag, arg->type.tag, 2, nullptr, &userop, pos_.line))
            {
                param = new CallUserOpExpr(userop, param);
                val = &param->val();
            }
            if (!checktag_string(arg->type.tag, val))
                checktag(arg->type.tag, val->tag);
            break;
        }
        case iREFERENCE:
            assert(!handling_this);

            if (!lvalue || val->ident == iARRAYCHAR) {
                error(pos_, 35, visual_pos); // argument type mismatch
                return false;
            }
            if (val->sym && val->sym->is_const && !arg->type.is_const) {
                error(pos_, 35, visual_pos); // argument type mismatch
                return false;
            }
            checktag(arg->type.tag, val->tag);
            break;
        case iREFARRAY:
            if (val->ident != iARRAY && val->ident != iREFARRAY && val->ident != iARRAYCELL &&
                val->ident != iARRAYCHAR)
            {
                error(pos_, 35, visual_pos); // argument type mismatch
                return false;
            }
            if (val->sym && val->sym->is_const && !arg->type.is_const) {
                error(pos_, 35, visual_pos); // argument type mismatch
                return false;
            }
            // Verify that the dimensions match those in |arg|. A literal array
            // always has a single dimension. An iARRAYCELL parameter is also
            // assumed to have a single dimension.
            if (!val->sym || val->ident == iARRAYCELL || val->ident == iARRAYCHAR) {
                if (arg->type.numdim() != 1) {
                    error(pos_, 48); // array dimensions must match
                    return false;
                }
                if (arg->type.dim[0] != 0) {
                    assert(arg->type.dim[0] > 0);
                    if (val->ident == iARRAYCELL) {
                        if (val->constval == 0 || arg->type.dim[0] != val->constval) {
                            error(pos_, 47); // array sizes must match
                            return false;
                        }
                    } else {
                        assert(val->constval != 0); // literal array must have a size
                        if ((val->constval > 0 && arg->type.dim[0] != val->constval) ||
                            (val->constval < 0 && arg->type.dim[0] < -val->constval))
                        {
                            error(pos_, 47); // array sizes must match
                            return false;
                        }
                    }
                }
            } else {
                symbol* sym = val->sym;
                if (sym->dim.array.level + 1 != arg->type.numdim()) {
                    error(pos_, 48); // array dimensions must match
                    return false;
                }
                // The lengths for all dimensions must match, unless the dimension
                // length was defined at zero (which means "undefined").
                short level = 0;
                while (sym->dim.array.level > 0) {
                    if (arg->type.dim[level] != 0 &&
                        sym->dim.array.length != arg->type.dim[level])
                    {
                        error(pos_, 47); // array sizes must match
                        return false;
                    }
                    sym = sym->array_child();
                    level++;
                }
                // The last dimension is checked too, again, unless it is zero.
                if (arg->type.dim[level] != 0 && sym->dim.array.length != arg->type.dim[level]) {
                    error(pos_, 47); // array sizes must match
                    return false;
                }
                if (!matchtag(arg->type.enum_struct_tag(), sym->x.tags.index, MATCHTAG_SILENT)) {
                    // We allow enumstruct -> any[].
                    if (arg->type.tag != pc_anytag || !gTypes.find(sym->x.tags.index)->asEnumStruct())
                        error(pos_, 229, sym->name());
                }
            }

            checktag(arg->type.tag, val->tag);
            if ((arg->type.tag != pc_tag_string && val->tag == pc_tag_string) ||
                (arg->type.tag == pc_tag_string && val->tag != pc_tag_string))
            {
                error(pos_, 178, type_to_name(val->tag), type_to_name(arg->type.tag));
                return false;
            }
            break;
        default:
            assert(false);
            break;
    }

    argv_[pos].expr = param;
    argv_[pos].arg = arg;
    return true;
}

void
CallExpr::ProcessUses(SemaContext& sc)
{
    for (const auto& arg : argv_) {
        if (!arg.expr)
            continue;
        arg.expr->MarkAndProcessUses(sc);
    }
}

void
CallExpr::MarkUsed(SemaContext& sc)
{
    if (sym_->defined) {
        /* function is defined, can now check the return value (but make an
         * exception for directly recursive functions)
         */
        if (sym_ != sc.func() && !sym_->retvalue) {
            auto symname = funcdisplayname(sym_->name());
            report(pos_, 140) << symname; /* function should return a value */
        }
    } else {
        /* function not yet defined, set */
        sym_->retvalue = true;
    }
    sym_->retvalue_used = true;
}

bool StaticAssertStmt::Analyze(SemaContext& sc)
{
    if (val_)
        return true;

    std::string message;
    if (text_)
        message += ": " + std::string(text_->chars(), text_->length());

    report(pos_, 70) << message;

    return false;
}

bool
NewArrayExpr::Analyze(SemaContext& sc)
{
    // We can't handle random refarrays floating around yet, so forbid this.
    error(pos_, 142);
    return false;
}

bool
NewArrayExpr::AnalyzeForInitializer(SemaContext& sc)
{
    if (already_analyzed_)
        return true;

    val_.ident = iREFARRAY;
    val_.tag = tag_;
    for (auto& expr : exprs_) {
        if (!expr->Analyze(sc))
            return false;
        if (expr->lvalue())
            expr = new RvalueExpr(expr);

        const auto& v = expr->val();
        if (IsLegacyEnumTag(sc.scope(), v.tag)) {
            error(expr->pos(), 153);
            return false;
        }
        if (!is_valid_index_tag(v.tag)) {
            error(expr->pos(), 77, type_to_name(v.tag));
            return false;
        }
        if (v.ident == iCONSTEXPR && v.constval <= 0) {
            error(expr->pos(), 9);
            return false;
        }
    }
    return true;
}

void
NewArrayExpr::ProcessUses(SemaContext& sc)
{
    for (const auto& expr : exprs_)
        expr->MarkAndProcessUses(sc);
}

bool
IfStmt::Analyze(SemaContext& sc)
{
    if (Expr* expr = cond_->AnalyzeForTest(sc))
        cond_ = expr;

    // Note: unlike loop conditions, we don't factor in constexprs here, it's
    // too much work and way less common than constant loop conditions.

    ke::Maybe<bool> always_returns;
    {
        AutoCollectSemaFlow flow(sc, &always_returns);
        if (!on_true_->Analyze(sc))
            return false;
    }
    {
        AutoCollectSemaFlow flow(sc, &always_returns);
        if (on_false_ && !on_false_->Analyze(sc))
            return false;
    }

    if (on_true_ && on_false_) {
        FlowType a = on_true_->flow_type();
        FlowType b = on_false_->flow_type();
        if (a == b)
            set_flow_type(a);
        else if (a != Flow_None && b != Flow_None)
            set_flow_type(Flow_Mixed);
    }

    if (*always_returns)
        sc.set_always_returns(true);
    return true;
}

bool
ExprStmt::Analyze(SemaContext& sc)
{
    if (!expr_->Analyze(sc))
        return false;
    if (!expr_->HasSideEffects())
        error(expr_->pos(), 215);
    return true;
}

/*  testsymbols - test for unused local or global variables
 *
 *  "Public" functions are excluded from the check, since these
 *  may be exported to other object modules.
 *  Labels are excluded from the check if the argument 'testlabs'
 *  is 0. Thus, labels are not tested until the end of the function.
 *  Constants may also be excluded (convenient for global constants).
 *
 *  When the nesting level drops below "level", the check stops.
 *
 *  The function returns whether there is an "entry" point for the file.
 *  This flag will only be 1 when browsing the global symbol table.
 */
static bool
TestSymbol(symbol* sym, bool testconst)
{
    bool entry = false;
    switch (sym->ident) {
        case iFUNCTN:
            if ((sym->usage & uREAD) == 0 && !(sym->native || sym->stock || sym->is_public) &&
                sym->defined)
            {
                auto symname = funcdisplayname(sym->name());
                if (!symname.empty()) {
                    /* symbol isn't used ... (and not public/native/stock) */
                    report(sym, 203) << symname;
                }
            }
            if (sym->is_public || strcmp(sym->name(), uMAINFUNC) == 0)
                entry = true; /* there is an entry point */
            break;
        case iCONSTEXPR:
            if (testconst && (sym->usage & uREAD) == 0) {
                error(sym, 203, sym->name()); /* symbol isn't used: ... */
            }
            break;
        case iMETHODMAP:
        case iENUMSTRUCT:
            // Ignore usage on methodmaps and enumstructs.
            break;
        default:
            /* a variable */
            if (sym->parent() != NULL)
                break; /* hierarchical data type */
            if (!sym->stock && (sym->usage & (uWRITTEN | uREAD)) == 0 && !sym->is_public) {
                error(sym, 203, sym->name()); /* symbol isn't used (and not stock) */
            } else if (!sym->stock && !sym->is_public && (sym->usage & uREAD) == 0) {
                error(sym, 204, sym->name()); /* value assigned to symbol is never used */
            }
            /* also mark the variable (local or global) to the debug information */
            if ((sym->is_public || (sym->usage & (uWRITTEN | uREAD)) != 0) && !sym->native)
                insert_dbgsymbol(sym);
    }
    return entry;
}

bool
TestSymbols(SymbolScope* root, int testconst)
{
    bool entry = false;
    root->ForEachSymbol([&](symbol* sym) -> void {
        entry |= TestSymbol(sym, !!testconst);
    });
    return entry;
}

bool
TestSymbols(symbol* root, int testconst)
{
    bool entry = false;

    symbol* sym = root->next;
    while (sym != NULL) {
        entry |= TestSymbol(sym, !!testconst);
        sym = sym->next;
    }

    errorset(sEXPRRELEASE, 0); /* clear error data */
    errorset(sRESET, 0);
    return entry;
}

BlockStmt*
BlockStmt::WrapStmt(Stmt* stmt)
{
    if (BlockStmt* block = stmt->AsBlockStmt())
        return block;
    BlockStmt* block = new BlockStmt(stmt->pos());
    block->stmts().emplace_back(stmt);
    return block;
}

bool
BlockStmt::Analyze(SemaContext& sc)
{
    bool ok = true;
    for (const auto& stmt : stmts_) {
        errorset(sRESET, 0);

        if (ok && !sc.warned_unreachable() && (sc.always_returns() || flow_type() != Flow_None)) {
            error(stmt->pos(), 225);
            sc.set_warned_unreachable();
        }
        ok &= stmt->Analyze(sc);

        FlowType flow = stmt->flow_type();
        if (flow != Flow_None && flow_type() == Flow_None)
            set_flow_type(flow);
    }

    if (scope_)
        TestSymbols(scope_, TRUE);
    return true;
}

AutoCollectSemaFlow::AutoCollectSemaFlow(SemaContext& sc, ke::Maybe<bool>* out)
  : sc_(sc),
    out_(out),
    old_value_(sc.always_returns())
{
    sc.set_always_returns(false);
}

AutoCollectSemaFlow::~AutoCollectSemaFlow()
{
    if (out_->isValid())
        out_->get() &= sc_.always_returns();
    else
        out_->init(sc_.always_returns());
    sc_.set_always_returns(old_value_);
}

bool
ReturnStmt::Analyze(SemaContext& sc)
{
    sc.set_always_returns();
    sc.loop_has_return() = true;

    symbol* curfunc = sc.func();

    if (!expr_) {
        if (curfunc->must_return_value())
            ReportFunctionReturnError(curfunc);
        if (sc.void_return())
            return true;
        sc.set_void_return(this);
        return true;
    }

    if (Stmt* stmt = sc.void_return()) {
        if (!sc.warned_mixed_returns()) {
            error(stmt->pos(), 78);
            error(pos_, 78);
            sc.set_warned_mixed_returns();
        }
    }

    if (!expr_->Analyze(sc))
        return false;

    if (expr_->lvalue())
        expr_ = new RvalueExpr(expr_);

    AutoErrorPos aep(expr_->pos());

    if (curfunc->tag == pc_tag_void) {
        error(pos_, 88);
        return false;
    }

    const auto& v = expr_->val();
    if (v.ident == iARRAY && !v.sym) {
        /* returning a literal string is not supported (it must be a variable) */
        error(pos_, 39);
        return false;
    }
    /* see if this function already has a sub type (an array attached) */
    auto sub = curfunc->array_return();
    assert(sub == nullptr || sub->ident == iREFARRAY);
    if (sc.returns_value()) {
        int retarray = (v.ident == iARRAY || v.ident == iREFARRAY);
        /* there was an earlier "return" statement in this function */
        if ((sub == nullptr && retarray) || (sub != nullptr && !retarray)) {
            error(pos_, 79); /* mixing "return array;" and "return value;" */
            return false;
        }
        if (retarray && curfunc->is_public) {
            error(pos_, 90, curfunc->name()); /* public function may not return array */
            return false;
        }
    } else {
        sc.set_returns_value();
    }

    /* check tagname with function tagname */
    assert(curfunc != nullptr);
    if (!matchtag_string(v.ident, v.tag))
        matchtag(curfunc->tag, v.tag, TRUE);

    if (v.ident == iARRAY || v.ident == iREFARRAY) {
        if (!CheckArrayReturn(sc))
            return false;
    }
    return true;
}

bool
ReturnStmt::CheckArrayReturn(SemaContext& sc)
{
    symbol* curfunc = sc.func();
    symbol* sub = curfunc->array_return();
    symbol* sym = expr_->val().sym;

    array_ = {};
    array_.ident = iARRAY;

    if (sub) {
        assert(sub->ident == iREFARRAY);
        // this function has an array attached already; check that the current
        // "return" statement returns exactly the same array
        int level = sym->dim.array.level;
        if (sub->dim.array.level != level) {
            error(pos_, 48); /* array dimensions must match */
            return false;
        }

        for (int i = 0; i <= level; i++) {
            array_.dim.emplace_back((int)sub->dim.array.length);
            if (sym->dim.array.length != array_.dim.back()) {
                error(pos_, 47); /* array sizes must match */
                return false;
            }

            if (i != level) {
                sym = sym->array_child();
                sub = sub->array_child();
                assert(sym != NULL && sub != NULL);
                // ^^^ both arrays have the same dimensions (this was checked
                //     earlier) so the dependend should always be found
            }
        }
        if (!sub->dim.array.length) {
            error(pos_, 128);
            return false;
        }

        // Restore it for below.
        sub = curfunc->array_return();
    } else {
        // this function does not yet have an array attached; clone the
        // returned symbol beneath the current function
        sub = sym;
        assert(sub != NULL);
        int level = sub->dim.array.level;
        for (int i = 0; i <= level; i++) {
            array_.dim.emplace_back((int)sub->dim.array.length);
            if (sub->x.tags.index) {
                array_.tag = 0;
                array_.declared_tag = sub->x.tags.index;
            }
            if (i != level) {
                sub = sub->array_child();
                assert(sub != NULL);
            }

            /* check that all dimensions are known */
            if (array_.dim.back() <= 0) {
                error(pos_, 46, sym->name());
                return false;
            }
        }
        if (!sub->dim.array.length) {
            error(pos_, 128);
            return false;
        }

        // the address of the array is stored in a hidden parameter; the address
        // of this parameter is 1 + the number of parameters (times the size of
        // a cell) + the size of the stack frame and the return address
        //   base + 0*sizeof(cell)         == previous "base"
        //   base + 1*sizeof(cell)         == function return address
        //   base + 2*sizeof(cell)         == number of arguments
        //   base + 3*sizeof(cell)         == first argument of the function
        //   ...
        //   base + ((n-1)+3)*sizeof(cell) == last argument of the function
        //   base + (n+3)*sizeof(cell)     == hidden parameter with array address
        assert(curfunc != NULL);
        int argcount;
        for (argcount = 0; curfunc->function()->args[argcount].type.ident != 0; argcount++)
            /* nothing */;

        auto dim = array_.dim.empty() ? nullptr : &array_.dim[0];
        sub = NewVariable(curfunc->nameAtom(), (argcount + 3) * sizeof(cell), iREFARRAY,
                          sGLOBAL, curfunc->tag, dim, array_.numdim(),
                          array_.enum_struct_tag());
        sub->set_parent(curfunc);
        curfunc->set_array_return(sub);
    }

    array_.tag = sub->tag;
    array_.has_postdims = true;
    return true;
}

bool
AssertStmt::Analyze(SemaContext& sc)
{
    if (Expr* expr = expr_->AnalyzeForTest(sc)) {
        expr_ = expr;
        return true;
    }
    return false;
}

bool
DeleteStmt::Analyze(SemaContext& sc)
{
    if (!expr_->Analyze(sc))
        return false;

    const auto& v = expr_->val();
    switch (v.ident) {
        case iFUNCTN:
            error(expr_->pos(), 167, "functions");
            return false;

        case iARRAY:
        case iREFARRAY:
        case iARRAYCELL:
        case iARRAYCHAR: {
            symbol* sym = v.sym;
            if (!sym || sym->dim.array.level > 0) {
                error(expr_->pos(), 167, "arrays");
                return false;
            }
            break;
        }
    }

    if (v.tag == 0) {
        error(expr_->pos(), 167, "integers");
        return false;
    }

    methodmap_t* map = gTypes.find(v.tag)->asMethodmap();
    if (!map) {
        error(expr_->pos(), 115, "type", type_to_name(v.tag));
        return false;
    }

    for (methodmap_t* iter = map; iter; iter = iter->parent) {
        if (iter->dtor) {
            map = iter;
            break;
        }
    }

    if (!map || !map->dtor) {
        report(expr_->pos(), 115) << layout_spec_name(map->spec) << map->name;
        return false;
    }

    map_ = map;
    return true;
}

bool
ExitStmt::Analyze(SemaContext& sc)
{
    if (!expr_->Analyze(sc))
        return false;
    if (expr_->lvalue())
        expr_ = new RvalueExpr(expr_);

    switch (expr_->val().ident) {
        case iEXPRESSION:
        case iREFERENCE:
        case iVARIABLE:
        case iCONSTEXPR:
        case iARRAYCHAR:
        case iARRAYCELL: {
            AutoErrorPos aep(expr_->pos());
            matchtag(0, expr_->val().tag, MATCHTAG_COERCE);
            break;
        }
        default:
            error(expr_->pos(), 106);
            return false;
    }
    return true;
}

bool
DoWhileStmt::Analyze(SemaContext& sc)
{
    if (Expr* expr = cond_->AnalyzeForTest(sc))
        cond_ = expr;

    ke::Maybe<cell> constval;
    if (cond_->val().ident == iCONSTEXPR)
        constval.init(cond_->val().constval);

    bool has_break = false;
    bool has_return = false;
    ke::Maybe<bool> always_returns;
    {
        AutoCollectSemaFlow flow(sc, &always_returns);
        ke::SaveAndSet<bool> auto_break(&sc.loop_has_break(), false);
        ke::SaveAndSet<bool> auto_return(&sc.loop_has_return(), false);

        if (!body_->Analyze(sc))
            return false;

        has_break = sc.loop_has_break();
        has_return = sc.loop_has_return();
    }

    never_taken_ = constval.isValid() && !constval.get();
    always_taken_ = constval.isValid() && constval.get();

    if (never_taken_ && token_ == tWHILE) {
        // Loop is never taken, don't touch the return status.
    } else if ((token_ == tDO || always_taken_) && !has_break) {
        // Loop is always taken, and has no break statements.
        if (always_taken_ && has_return)
            sc.set_always_returns(true);

        // Loop body ends in a return and has no break statements.
        if (body_->flow_type() == Flow_Return)
            set_flow_type(Flow_Return);
    }

    // :TODO: endless loop warning?
    return true;
}

bool
ForStmt::Analyze(SemaContext& sc)
{
    bool ok = true;
    if (init_ && !init_->Analyze(sc))
        ok = false;
    if (cond_) {
        if (Expr* expr = cond_->AnalyzeForTest(sc))
            cond_ = expr;
        else
            ok = false;
    }
    if (advance_ && !advance_->Analyze(sc))
        ok = false;

    ke::Maybe<cell> constval;
    if (cond_ && cond_->val().ident == iCONSTEXPR)
        constval.init(cond_->val().constval);

    bool has_break = false;
    bool has_return = false;
    ke::Maybe<bool> always_returns;
    {
        AutoCollectSemaFlow flow(sc, &always_returns);
        ke::SaveAndSet<bool> auto_break(&sc.loop_has_break(), false);
        ke::SaveAndSet<bool> auto_continue(&sc.loop_has_continue(), false);
        ke::SaveAndSet<bool> auto_return(&sc.loop_has_return(), false);

        if (!body_->Analyze(sc))
            ok = false;

        has_break = sc.loop_has_break();
        has_return = sc.loop_has_return();
        has_continue_ = sc.loop_has_continue();
    }

    never_taken_ = constval.isValid() && !constval.get();
    always_taken_ = !cond_ || (constval.isValid() && constval.get());

    // If the body falls through, then implicitly there is a continue operation.
    if (body_->flow_type() != Flow_Break && body_->flow_type() != Flow_Return)
        has_continue_ = true;
    // If there is a non-constant conditional, there is also an implicit continue.
    if (!always_taken_)
        has_continue_ = true;

    if (never_taken_) {
        // Loop is never taken, don't touch the return status.
    } else if (always_taken_ && !has_break) {
        if (has_return) {
            // Loop is always taken, and has no break statements, and has a return statement.
            sc.set_always_returns(true);
        }
        if (body_->flow_type() == Flow_Return && !has_break)
            set_flow_type(Flow_Return);
    }

    if (scope_)
        TestSymbols(scope_, TRUE);
    return ok;
}

bool
SwitchStmt::Analyze(SemaContext& sc)
{
    bool tag_ok = expr_->Analyze(sc);
    const auto& v = expr_->val();
    if (tag_ok && (v.ident == iARRAY || v.ident == iREFARRAY))
        error(expr_->pos(), 33, "-unknown-");

    if (expr_->lvalue())
        expr_ = new RvalueExpr(expr_);

    ke::Maybe<bool> always_returns;
    ke::Maybe<FlowType> flow;

    auto update_flow = [&](FlowType other) -> void {
        if (flow) {
            if (*flow == Flow_None || other == Flow_None)
                *flow = Flow_None;
            else if (*flow != other)
                *flow = Flow_Mixed;
        } else {
            flow.init(other);
        }
    };

    std::unordered_set<cell> case_values;
    for (const auto& case_entry : cases_) {
        for (Expr* expr : case_entry.first) {
            if (!expr->Analyze(sc))
                continue;

            int tag;
            cell value;
            if (!expr->EvalConst(&value, &tag)) {
                error(expr->pos(), 8);
                continue;
            }
            if (tag_ok) {
                AutoErrorPos aep(expr->pos());
                matchtag(v.tag, tag, MATCHTAG_COERCE);
            }

            if (!case_values.count(value))
                case_values.emplace(value);
            else
                error(expr->pos(), 40, value);
        }

        AutoCollectSemaFlow flow(sc, &always_returns);
        case_entry.second->Analyze(sc);

        update_flow(case_entry.second->flow_type());
    }

    if (default_case_) {
        AutoCollectSemaFlow flow(sc, &always_returns);
        default_case_->Analyze(sc);

        update_flow(default_case_->flow_type());
    } else {
        always_returns.init(false);
        update_flow(Flow_None);
    }

    if (*always_returns)
        sc.set_always_returns(true);

    set_flow_type(*flow);

    // Return value doesn't really matter for statements.
    return true;
}

void
ReportFunctionReturnError(symbol* sym)
{
    auto symname = funcdisplayname(sym->name());

    // Normally we want to encourage return values. But for legacy code,
    // we allow "public int" to warn instead of error.
    //
    // :TODO: stronger enforcement when function result is used from call
    if (sym->tag == 0) {
        report(209) << symname;
    } else if (gTypes.find(sym->tag)->isEnum() || sym->tag == pc_tag_bool ||
               sym->tag == sc_rationaltag || !sym->retvalue_used)
    {
        report(242) << symname;
    } else {
        report(400) << symname;
    }
}

FunctionInfo::FunctionInfo(const token_pos_t& pos, const declinfo_t& decl)
  : pos_(pos),
    decl_(decl)
{
}

void
FunctionInfo::AddArg(VarDecl* arg)
{
    args_.emplace_back(FunctionArg{arg});
}

bool
FunctionInfo::IsVariadic() const
{
    return !args_.empty() && args_.back().decl->type().ident == iVARARGS;
}

bool
FunctionInfo::Analyze(SemaContext& outer_sc)
{
    SemaContext sc(sym_);

    if (sym_->skipped && !this_tag_)
        return true;

    sc.set_func(sym_);
    auto guard = ke::MakeScopeGuard([&]() -> void {
        sc.set_func(nullptr);
    });

    // :TODO: remove this when curfunc goes away.
    ke::SaveAndSet<symbol*> auto_curfunc(&curfunc, sym_);

    {
        AutoErrorPos error_pos(pos_);
        check_void_decl(&decl_, FALSE);

        if (decl_.opertok)
            check_operatortag(decl_.opertok, decl_.type.tag, decl_.name->chars());
    }

    bool was_prototyped = sym_->prototyped;
    if (!was_prototyped) {
        sym_->tag = decl_.type.tag;
        sym_->explicit_return_type = decl_.type.is_new;
    }

    if (sym_->is_public || sym_->forward) {
        if (decl_.type.numdim() > 0)
            error(pos_, 141);
    }

    /* if the function was used before being declared, and it has a tag for the
     * result, add a third pass (as second "skimming" parse) because the function
     * result may have been used with user-defined operators, which have now
     * been incorrectly flagged (as the return tag was unknown at the time of
     * the call)
     */
    if (!was_prototyped && (sym_->usage & uREAD) && sym_->tag != 0 && !decl_.opertok && body_)
        sc_reparse = TRUE; /* must add another pass to "initial scan" phase */

    bool ok = AnalyzeArgs(sc);

    // Must be after AnalyzeArgs() for argcompare to work.
    sym_->prototyped = true;

    if (sym_->defined && !is_forward_)
        error(21, sym_->name());

    if (sym_->native) {
        if (decl_.type.numdim() > 0) {
            report(83);
            return false;
        }
        sym_->retvalue = true;
        return true;
    }

    if (!body_) {
        if (sym_->is_public && !sym_->forward)
            error(pos_, 10);
        return true;
    }

    if (sym_->deprecated && !sym_->stock) {
        const char* ptr = sym_->documentation.c_str();
        error(234, sym_->name(), ptr); /* deprecated (probably a public function) */
    }

    sym_->defined = true;
    sym_->missing = false;

    if (this_tag_)
        sc_err_status = TRUE;

    ok &= body_->Analyze(sc);

    if (sc.returns_value()) {
        sym_->retvalue = true;
    } else {
        if (sym_->tag == pc_tag_void && sym_->forward && !decl_.type.tag &&
            !decl_.type.is_new)
        {
            // We got something like:
            //    forward void X();
            //    public X()
            //
            // Switch our decl type to void.
            decl_.type.tag = pc_tag_void;
        }
    }

    // Check that return tags match.
    if (was_prototyped && sym_->tag != decl_.type.tag)
        error(pos_, 180, type_to_name(sym_->tag), type_to_name(decl_.type.tag));

    if (scope_)
        TestSymbols(scope_, TRUE);

    if (!sc.always_returns()) {
        if (sym_->must_return_value())
            ReportFunctionReturnError(sym_);

        // We should always have a block statement for the body. If no '{' was
        // detected it would have been an error in the parsing pass.
        auto block = body_->AsBlockStmt();
        assert(block);

        // Synthesize a return statement.
        auto ret_stmt = new ReturnStmt(end_pos_, nullptr);
        block->stmts().push_back(ret_stmt);
        block->set_flow_type(Flow_Return);
    }

    sc_err_status = FALSE;
    return true;
}

bool
FunctionInfo::AnalyzeArgs(SemaContext& sc)
{
    std::vector<arginfo>& arglist = sym_->function()->args;

    size_t oldargcnt = 0;
    if (sym_->prototyped) {
        while (arglist[oldargcnt].type.ident != 0)
            oldargcnt++;
    }

    AutoCountErrors errors;

    size_t argcnt = 0;
    for (const auto& parsed_arg : args_) {
        const auto& var = parsed_arg.decl;
        const auto& typeinfo = var->type();
        symbol* argsym = var->sym();

        AutoErrorPos pos(var->pos());

        if (typeinfo.ident == iVARARGS) {
            if (!sym_->prototyped) {
                /* redimension the argument list, add the entry iVARARGS */
                sym_->function()->resizeArgs(argcnt + 1);

                arglist[argcnt].type.ident = iVARARGS;
                arglist[argcnt].type.tag = typeinfo.tag;
            } else {
                if (argcnt > oldargcnt || arglist[argcnt].type.ident != iVARARGS)
                    error(25); /* function definition does not match prototype */
            }
            argcnt++;
            continue;
        }

        Type* type = gTypes.find(typeinfo.semantic_tag());
        if (type->isEnumStruct()) {
            if (sym_->native)
                error(135, type->name());
        }

        /* Stack layout:
         *   base + 0*sizeof(cell)  == previous "base"
         *   base + 1*sizeof(cell)  == function return address
         *   base + 2*sizeof(cell)  == number of arguments
         *   base + 3*sizeof(cell)  == first argument of the function
         * So the offset of each argument is "(argcnt+3) * sizeof(cell)".
         */
        argsym->setAddr(static_cast<cell>((argcnt + 3) * sizeof(cell)));

        arginfo arg;
        arg.name = var->name();
        arg.type.ident = argsym->ident;
        arg.type.is_const = argsym->is_const;
        arg.type.tag = argsym->tag;
        arg.type.dim = typeinfo.dim;
        arg.type.declared_tag = typeinfo.enum_struct_tag();

        if (typeinfo.ident == iREFARRAY || typeinfo.ident == iARRAY) {
            if (var->Analyze(sc) && var->init_rhs())
                fill_arg_defvalue(var, &arg);
        } else {
            Expr* init = var->init_rhs();
            if (init && init->Analyze(sc)) {
                assert(typeinfo.ident == iVARIABLE || typeinfo.ident == iREFERENCE);
                arg.def = std::make_unique<DefaultArg>();

                int tag;
                cell val;
                if (!init->EvalConst(&val, &tag)) {
                    error(8);

                    // Populate to avoid errors.
                    val = 0;
                    tag = typeinfo.tag;
                }
                arg.def->tag = tag;
                arg.def->val = ke::Some(val);

                matchtag(arg.type.tag, arg.def->tag, MATCHTAG_COERCE);
            }
        }

        if (arg.type.ident == iREFERENCE)
            argsym->usage |= uREAD; /* because references are passed back */
        if (sym_->callback || sym_->stock || sym_->is_public)
            argsym->usage |= uREAD; /* arguments of public functions are always "used" */

        /* arguments of a public function may not have a default value */
        if (sym_->is_public && arg.def)
            error(59, var->name()->chars());

        if (!sym_->prototyped) {
            /* redimension the argument list, add the entry */
            sym_->function()->resizeArgs(argcnt + 1);
            arglist[argcnt] = std::move(arg);
        } else {
            /* check the argument with the earlier definition */
            if (argcnt > oldargcnt || !argcompare(&arglist[argcnt], &arg))
                report(181) << arg.name; /* function argument does not match prototype */
        }
        argcnt++;
    }
    return errors.ok();
}

bool
FunctionDecl::Analyze(SemaContext& sc)
{
    return info_->Analyze(sc);
}

void
StmtList::ProcessUses(SemaContext& sc)
{
    for (const auto& stmt : stmts_)
        stmt->ProcessUses(sc);
}

void
VarDecl::ProcessUses(SemaContext& sc)
{
    if (init_)
        init_rhs()->MarkAndProcessUses(sc);
}

void
IfStmt::ProcessUses(SemaContext& sc)
{
    cond_->MarkAndProcessUses(sc);
    on_true_->ProcessUses(sc);
    if (on_false_)
        on_false_->ProcessUses(sc);
}

void
ReturnStmt::ProcessUses(SemaContext& sc)
{
    if (expr_)
        expr_->MarkAndProcessUses(sc);
}

void
ExitStmt::ProcessUses(SemaContext& sc)
{
    if (expr_)
        expr_->MarkAndProcessUses(sc);
}

void
DoWhileStmt::ProcessUses(SemaContext& sc)
{
    cond_->MarkAndProcessUses(sc);
    body_->ProcessUses(sc);
}

void
ForStmt::ProcessUses(SemaContext& sc)
{
    if (init_)
        init_->ProcessUses(sc);
    if (cond_)
        cond_->MarkAndProcessUses(sc);
    if (advance_)
        advance_->ProcessUses(sc);
    body_->ProcessUses(sc);
}

void
SwitchStmt::ProcessUses(SemaContext& sc)
{
    expr_->MarkAndProcessUses(sc);

    for (const auto& entry : cases_) {
        for (const auto& expr : entry.first)
            expr->MarkAndProcessUses(sc);
        entry.second->ProcessUses(sc);
    }

    if (default_case_)
        default_case_->ProcessUses(sc);
}

void
FunctionInfo::ProcessUses(SemaContext& sc)
{
    if (!body_ || sym_->skipped)
        return;

    sc.set_func(sym_);
    auto guard = ke::MakeScopeGuard([&]() -> void {
        sc.set_func(nullptr);
    });

    ke::SaveAndSet<symbol*> set_curfunc(&curfunc, sym_);

    for (const auto& arg : args_)
        arg.decl->ProcessUses(sc);

    body_->ProcessUses(sc);
}

void
FunctionDecl::ProcessUses(SemaContext& sc)
{
    info_->ProcessUses(sc);
}

bool
PragmaUnusedStmt::Analyze(SemaContext& sc)
{
    for (const auto& sym : symbols_) {
        sym->usage |= uREAD;

        switch (sym->ident) {
            case iVARIABLE:
            case iREFERENCE:
            case iARRAY:
            case iREFARRAY:
                sym->usage |= uWRITTEN;
                break;
        }
    }
    return true;
}

bool
EnumStructDecl::Analyze(SemaContext& sc)
{
    bool ok = true;
    for (const auto& fun : methods_)
        ok &= fun->Analyze(sc);
    return ok;
}

void
EnumStructDecl::ProcessUses(SemaContext& sc)
{
    for (const auto& fun : methods_)
        fun->ProcessUses(sc);
}

bool
MethodmapDecl::Analyze(SemaContext& sc)
{
    bool ok = true;
    for (const auto& prop : properties_) {
        if (prop->getter)
            ok &= prop->getter->Analyze(sc);
        if (prop->setter)
            ok &= prop->setter->Analyze(sc);
    }
    for (const auto& method : methods_)
        ok &= method->decl->Analyze(sc);
    return ok;
}

void
MethodmapDecl::ProcessUses(SemaContext& sc)
{
    for (const auto& prop : properties_) {
        if (prop->getter)
            prop->getter->ProcessUses(sc);
        if (prop->setter)
            prop->setter->ProcessUses(sc);
    }
    for (const auto& method : methods_)
        method->decl->ProcessUses(sc);
}

void
check_void_decl(const typeinfo_t* type, int variable)
{
    if (type->tag != pc_tag_void)
        return;

    if (variable) {
        error(144);
        return;
    }

    if (type->numdim() > 0) {
        error(145);
        return;
    }
}

void
check_void_decl(const declinfo_t* decl, int variable)
{
    check_void_decl(&decl->type, variable);
}

int
argcompare(arginfo* a1, arginfo* a2)
{
    int result = 1;

    if (result)
        result = a1->type.ident == a2->type.ident; /* type/class */
    if (result)
        result = a1->type.is_const == a2->type.is_const; /* "const" flag */
    if (result)
        result = a1->type.tag == a2->type.tag;
    if (result)
        result = a1->type.dim == a2->type.dim; /* array dimensions & index tags */
    if (result)
        result = a1->type.declared_tag == a2->type.declared_tag;
    if (result)
        result = !!a1->def == !!a2->def; /* availability of default value */
    if (a1->def) {
        if (a1->type.ident == iREFARRAY) {
            if (result)
                result = !!a1->def->array == !!a2->def->array;
            if (result && a1->def->array)
                result = a1->def->array->total_size() == a2->def->array->total_size();
            /* ??? should also check contents of the default array (these troubles
             * go away in a 2-pass compiler that forbids double declarations, but
             * Pawn currently does not forbid them) */
        } else {
            if (result)
                result = a1->def->val.isValid() == a2->def->val.isValid();
            if (result && a1->def->val)
                result = a1->def->val.get() == a2->def->val.get();
        }
        if (result)
            result = a1->def->tag == a2->def->tag;
    }
    return result;
}

bool
IsLegacyEnumTag(SymbolScope* scope, int tag)
{
    Type* type = gTypes.find(tag);
    if (!type->isEnum())
        return false;
    symbol* sym = findconst(CompileContext::get(), scope, type->nameAtom(), -1);
    return sym->dim.enumlist != nullptr;
}
