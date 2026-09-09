// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2024-2026 AlliedModders LLC
// Copyright (c) ITB CompuPhase, 1997-2005

#include "array-helpers.h"

#include <amtl/am-maybe.h>
#include <amtl/am-raii.h>
#include <amtl/am-utility.h>
#include "errors.h"
#include "ir-node.h"
#include "lexer-inl.h"
#include "semantics.h"
#include "symbols.h"

namespace sp {
namespace cc {

static bool HasDynamicSizeofExpr(Expr* expr) {
    if (expr->is(ExprKind::SizeofExpr))
        return true;
    if (auto bin = expr->as<BinaryExpr>())
        return HasDynamicSizeofExpr(bin->left()) || HasDynamicSizeofExpr(bin->right());
    if (auto unary = expr->as<UnaryExpr>())
        return HasDynamicSizeofExpr(unary->expr());
    return false;
}

class ArrayTypeResolver
{
  public:
    ArrayTypeResolver(Semantics* sema, VarDeclBase* decl);
    ArrayTypeResolver(Semantics* sema, const token_pos_t& pos, typeinfo_t* type, int vclass);

    bool Resolve();

  private:
    bool ResolveSize();
    bool ResolveDimExprs();
    void ResolveRank(size_t rank, Expr* init);
    void SetRankSize(Expr* expr, int rank, int size);
    bool ResolveDimExpr(Expr* expr, ExprVal* v);

  private:
    Semantics* sema_;
    TypeManager* types_;
    const token_pos_t& pos_;
    VarDeclBase* decl_ = nullptr;
    typeinfo_t* type_;
    Expr* initializer_;
    std::vector<int> computed_;
    int vclass_;
    AutoCountErrors errors_;
    Type* es_;
};

static constexpr int kSizeUnknown = -1;
static constexpr int kSizeIndeterminate = -2;

// Large flat arrays can cause us to run out of local stack registers. The main
// motivation of flat arrays is to optimize for small vectors, so this cap is
// very generous. Once exceeded we fallback to fixed arrays which use a single
// heap allocation.
static constexpr int kMaxFlatArrayBytes = 4096;

// Flat arrays store their elements inline without GC tracking, so they
// can never contain heap items. Otherwise the runtime would miss those
// references during finalization.
static bool CanUseFlatArray(Type* element_type, int array_size) {
    if (element_type->isHeapItem())
        return false;

    int elt_size;
    if (int pod_load_size = element_type->podLoadSize(); pod_load_size != -1)
        elt_size = pod_load_size;
    else if (element_type->isIntPtr())
        elt_size = 8;
    else
        elt_size = 4;
    return array_size * elt_size <= kMaxFlatArrayBytes;
}

ArrayTypeResolver::ArrayTypeResolver(Semantics* sema, VarDeclBase* decl)
  : sema_(sema),
    types_(sema->cc().types()),
    pos_(decl->pos()),
    decl_(decl),
    type_(decl->mutable_type_info()),
    initializer_(decl->init_rhs()),
    computed_(type_->dim_exprs.size()),
    vclass_(decl->vclass()),
    es_(nullptr)
{
    Type* type = type_->type;
    if (type->isEnumStruct())
        es_ = type;
}

ArrayTypeResolver::ArrayTypeResolver(Semantics* sema, const token_pos_t& pos, typeinfo_t* type,
                                     int vclass)
  : sema_(sema),
    pos_(pos),
    type_(type),
    initializer_(nullptr),
    computed_(type_->dim_exprs.size()),
    vclass_(vclass)
{
}

bool ArrayTypeResolver::Resolve() {
    bool resolved_size = ResolveSize();

    assert(!type_->resolved_array);

    if (type_->type->isVoid()) {
        report(pos_, 145);
        return false;
    }

    for (size_t i = 0; i < computed_.size(); i++) {
        if (computed_[i] < 0) {
            assert(!resolved_size);
            computed_[i] = 0;
        }
    }

    // Always build a Type, so we don't have a null type lying around.
    auto types = CompileContext::get().types();
    if (computed_.size() == 1) {
        if (computed_[0] > 0 && CanUseFlatArray(type_->type, computed_[0]))
            type_->type = types->defineFlatArray(type_->type, computed_[0]);
        else
            type_->type = types->defineArray(type_->type, computed_[0]);
    } else {
        type_->type = types->defineArray(type_->type, computed_.data(), (int)computed_.size());
    }
    type_->resolved_array = true;
    return resolved_size;
}


bool ArrayTypeResolver::ResolveSize() {
    if (!type_->has_postdims)
        return true;

    // If the array has old-style dimensions, we analyze them now. This is
    // technically a violation of the normal parsing order. The experimental
    // compiler had a special constant evaluator that could suppress syntax
    // errors. We don't have that, so we have to live with this instead.
    //
    // Note that there shouldn't really be any difference in behavior, since
    // all usable symbols are already entered, and their types will not
    // change between binding and later analysis.
    if (!ResolveDimExprs())
        return false;

    // If this is an implicit dynamic array (old syntax), the initializer
    // cannot be used for computing a size.
    if (decl_ && decl_->implicit_dynamic_array())
        return true;

    // Traverse the initializer if present. For arguments, initializers
    // don't participate in determining the fixed size.
    if (vclass_ != sARGUMENT && initializer_) {
        ResolveRank(0, initializer_);

        // If we hit errors resolving the type, don't bother using any values
        // we computed along the way.
        if (!errors_.ok())
            return false;
    }

    // Any kind of indeterminate status gets forced back to 0. Semantic
    // analysis will catch type or other size errors in the initializer.
    bool indeterminate = false;
    for (size_t i = 0; i < type_->dim_exprs.size(); i++) {
        if (type_->dim_exprs[i])
            continue;

        if (computed_[i] < 0) {
            if (i != type_->dim_exprs.size() - 1)
                indeterminate = true;
            computed_[i] = 0;
        }
    }

    // If the declaration is new-style, like:
    //      int x[];
    //
    // It must either have a fixed width or a fixed initializer. If we get here
    // without any errors, it means one dimension was either dynamic or there
    // was no initializer.
    //
    // Note that we allow the last dimension to be indeterminate. This is a
    // compatibility hack because the old array initialization code allowed
    // it. In theory any dimension *can* be indeterminate, there is no
    // technical reason with our new array code to not support indeterminism.
    // But for now, we retain the old error.
    if (type_->is_new && indeterminate) {
        if (vclass_ == sARGUMENT && type_->dim_exprs.back()) {
            // As noted in ResolveDimExprs, we allow this for arguments as long
            // as the last dimension is filled.
        } else if (vclass_ == sLOCAL) {
            report(pos_, 159);
            return false;
        } else {
            report(pos_, 183);
            return false;
        }
    }
    return true;
}

void
ArrayTypeResolver::ResolveRank(size_t rank, Expr* init)
{
    if (StringExpr* expr = init->as<StringExpr>()) {
        if (rank != computed_.size() - 1) {
            // This is an error, but we'll let it get reported during semantic
            // analysis.
            return;
        }
        SetRankSize(expr, rank, expr->text()->length() + 1);
        return;
    }

    ArrayExpr* expr = init->as<ArrayExpr>();
    if (!expr) {
        // This is an error, but we just continue anyway. We'll let semantic
        // analysis report a more complete error.
        return;
    }

    // This happens with structs. Don't bother checking for a struct though.
    // Just continue and let a later pass figure out if there's an error.
    if (rank >= computed_.size())
        return;

    if (!type_->dim_exprs[rank] && expr->ellipses())
        report(expr->pos(), 41);

    SetRankSize(expr, rank, expr->exprs().size());

    for (const auto& child : expr->exprs())
        ResolveRank(rank + 1, child);
}

void
ArrayTypeResolver::SetRankSize(Expr* expr, int rank, int size)
{
    if (computed_[rank] == kSizeUnknown) {
        computed_[rank] = size;
        return;
    }
    if (computed_[rank] == size)
        return;

    if (rank == static_cast<int>(computed_.size()) - 1) {
        // The final rank is allowed to vary as long as the size was not
        // explicitly specified. If it was specified, we'll error during
        // semantic analysis, so there's no need to handle it now.
        if (!type_->dim_exprs[rank])
            computed_[rank] = kSizeIndeterminate;
    } else if (computed_[rank] > 0) {
        // Intermediate ranks must not vary in size.
        report(expr->pos(), 47);
        computed_[rank] = kSizeIndeterminate;
    }
}

bool ArrayTypeResolver::ResolveDimExprs() {
    for (size_t i = 0; i < type_->dim_exprs.size(); i++) {
        Expr* expr = type_->dim_exprs[i];
        if (!expr) {
            // We allow something like:
            //    f(const String:blah[])
            //
            // But new syntax must have a fixed size in the last dimension:
            //    f(const char blah[])
            //           illegal --^
            //
            // The last dimension restriction is a compromise due to
            // limitations of transitional syntax. Ideally, we would restrict
            // every dimension, but there is no practical alternative for:
            //
            //   typedef void f(int x[][2]);
            //
            // And this seems like a perfectly valid thing to want (a dynamic
            // array of fixed-size arrays).
            if (i == type_->dim_exprs.size() - 1 && vclass_ == sARGUMENT && type_->is_new) {
                report(pos_, 183);
                return false;
            }

            // Enum fields must always be fixed size.
            if (vclass_ == sENUMFIELD) {
                report(pos_, 183);
                return false;
            }

            // If we're an array, we need an initializer and this will be
            // checked later during size resolution. If we're an iREFARRAY,
            // it means an ambiguous old-style array like:
            //     new x[y][3];
            //
            // In this case, we'll verify that no initializer exists later
            // during semantic analysis.
            //
            // In both cases, we don't need to check anything here. For non-
            // local cases like global/static variables, arguments, or enum
            // fields, we'd have errored in ResolveDimExprs().
            computed_[i] = kSizeUnknown;
            continue;
        }

        ExprVal v;
        if (!ResolveDimExpr(expr, &v))
            return false;

        if (!IsValidIndexType(v.type())) {
            report(expr->pos(), 77) << v.type();
            return false;
        }

        if (v.ident != iCONSTEXPR) {
            // Non-constant expressions in postdims is illegal for transitional
            // syntax:
            //     int blah[y];
            //              ^-- no
            if (type_->is_new) {
                if (HasDynamicSizeofExpr(expr)) {
                    // This is support for backwards compatibility with plugins that make a declaration like:
                    // any data[sizeof(x)];
                    report(expr->pos(), 254);
                } else {
                    report(expr->pos(), 161) << type_->type;
                    return false;
                }
            }

            // Old-style dynamic arrays are only allowed in local scope.
            if (vclass_ != sLOCAL) {
                report(expr->pos(), 162);
                return false;
            }
            computed_[i] = 0;

            // sLOCAL guarantees we have a decl.
            decl_->set_implicit_dynamic_array();
        } else if (IsLegacyEnumType(sema_->current_scope(), v.type()) && v.sym() &&
                   v.sym()->as<EnumDecl>())
        {
            report(expr->pos(), 153);
            return false;
        } else {
            // Constant must be > 0.
            if (v.const_i32() <= 0) {
                report(expr->pos(), 9);
                return false;
            }
            computed_[i] = v.const_i32();
        }
    }
    return true;
}

bool ArrayTypeResolver::ResolveDimExpr(Expr* expr, ExprVal* v) {
    auto& sc = *sema_->context();
    if (!expr->Bind(sc))
        return false;

    if (auto sym_expr = expr->as<SymbolExpr>()) {
        // Special case this:
        //   enum X { ... };
        //   int blah[X];
        //
        // For backward compatibility with a huge number of plugins.
        auto decl = sym_expr->decl();
        if (auto ed = decl->as<EnumDecl>()) {
            *v = {};
            v->set_constval(ed->array_size());
            v->set_type(sc.cc().types()->type_int());
            return true;
        }
    }

    ir::Value* checked = sema_->CheckExpr(expr);
    if (!checked)
        return false;

    *v = checked->val();
    return true;
}

bool ResolveArrayType(Semantics* sema, VarDeclBase* decl) {
    ArrayTypeResolver resolver(sema, decl);
    return resolver.Resolve();
}

bool ResolveArrayType(Semantics* sema, const token_pos_t& pos, typeinfo_t* type, int vclass) {
    ArrayTypeResolver resolver(sema, pos, type, vclass);
    return resolver.Resolve();
}

class ArrayValidator final
{
  public:
    ArrayValidator(Semantics* sema, VarDeclBase* decl)
      : sema_(sema),
        types_(sema->cc().types()),
        decl_(decl),
        pos_(decl->pos()),
        init_(decl->init_rhs()),
        type_(decl->type())
    {
    }

    ArrayValidator(Semantics* sema, const typeinfo_t& type, Expr* init)
      : sema_(sema),
        types_(sema->cc().types()),
        decl_(nullptr),
        pos_(init->pos()),
        init_(init),
        type_(type.type)
    {}

    bool Validate();

    ir::Value* init_ir() const { return init_ir_; }

  private:
    bool ValidateInitializer();
    ir::Value* ValidateRank(ArrayType* rank, Expr* init);
    bool AddCells(size_t ncells);
    bool CheckArgument(SymbolExpr* init);

  private:
    Semantics* sema_;
    TypeManager* types_;
    VarDeclBase* decl_;
    token_pos_t pos_;
    Expr* init_;
    QualType type_;
    ArrayType* at_;
    ir::Value* init_ir_ = nullptr;
    unsigned total_cells_ = 0;
};

bool CheckArrayInitialization(Semantics* sema, const typeinfo_t& type, Expr* init,
                              ir::Value** out) {
    ArrayValidator av(sema, type, init);

    AutoCountErrors errors;
    if (!av.Validate() && errors.ok())
        return false;

    *out = av.init_ir();
    return true;
}

bool ArrayValidator::Validate() {
    at_ = type_->as<ArrayType>();

    if (init_) {
        if (!ValidateInitializer())
            return false;
        return true;
    }
    if (!at_)
        return true;

    // The array has no initializer, which means it was declared as a fixed
    // size array.
    auto iter = at_;
    do {
        if (iter->is_flat() && !iter->size() && decl_ && decl_->vclass() != sARGUMENT) {
            report(decl_->pos(), 46) << decl_->name();
            return true;
        }
        iter = iter->inner()->as<ArrayType>();
    } while (iter);

    // Check that the declared size does not overflow when multiplied by
    // sizeof(cell_t).
    //
    // Quick, non-recursive computation. For example take [3][4][5]:
    //   3 + (3 * 4) + (3 * 4 * 5)
    //
    // The calculation is not simply 3*4*5 because of indirection vectors.
    unsigned last_size = 1;

    iter = at_;
    do {
        if (!iter->size())
            break;
        if (!ke::IsUintMultiplySafe<uint32_t>(last_size, iter->size())) {
            report(pos_, 52);
            return false;
        }
        last_size *= iter->size();
        if (last_size >= kMaxCells) {
            report(pos_, 52);
            return false;
        }
        if (!AddCells(last_size))
            return false;
        iter = iter->inner()->as<ArrayType>();
    } while (iter);
    return true;
}

bool ArrayValidator::ValidateInitializer() {
    // As a special exception, array arguments can be initialized with a global
    // reference.
    if (decl_ && decl_->vclass() == sARGUMENT) {
        if (auto expr = init_->as<SymbolExpr>())
            return CheckArgument(expr);
    }

    // Check for dynamic initializers.
    if (auto ctor = init_->as<NewArrayExpr>()) {
        auto iter = at_;
        do {
            if (iter->size() && !(decl_ && decl_->implicit_dynamic_array())) {
                report(init_->pos(), 464);
                return false;
            }
            iter = iter->inner()->as<ArrayType>();
        } while (iter);

        ir::Value* na_node = sema_->CheckNewArrayExprForArrayInitializer(ctor);
        if (!na_node)
            return false;

        if (!sema_->CheckCoercion(na_node, at_, ctor->type(), CvtContext::Assignment))
            return false;
        init_ir_ = na_node;
        if (decl_)
            decl_->set_sema_init_rhs(na_node);
        return true;
    }

    // For dynamic (heap) arrays, allow initialization from another array
    // expression (reference copy).
    if (at_ && !at_->is_fixed()) {
        if (init_->as<ArrayExpr>() && decl_ && decl_->vclass() != sARGUMENT) {
            report(init_->pos(), 160);
            return false;
        }
        ir::Value* node = sema_->CheckRvalue(init_, at_);
        if (!node)
            return false;
        if (auto lval = node->as<ir::Lvalue>())
            node = new ir::Rvalue(lval);
        init_ir_ = node;
        if (decl_)
            decl_->set_sema_init_rhs(node);
        return sema_->CheckCoercion(node, at_, node->val().type(), CvtContext::Assignment);
    }

    // Not a dynamic array, check for a fixed initializer.
    init_ir_ = ValidateRank(at_, init_);
    return !!init_ir_;
}

bool ArrayValidator::CheckArgument(SymbolExpr* expr) {
    Decl* decl = expr->decl();
    if (!decl)
        return false;

    VarDecl* var = decl->as<VarDecl>();
    if (!var)
        return false;

    assert(var->vclass() == sGLOBAL || var->vclass() == sSTATIC);

    // Since default arguments are not analyzed by standard expression checkers
    // (such as CheckSymbolExpr), we must explicitly set the variable's semantic
    // value here. This ensures that the code generator recognizes this SymbolExpr
    // as an lvalue and correctly emits OP_LOAD_GLB to load the array address/pointer.
    ir::Value* node = new ir::Variable(expr, var);

    if (!sema_->CheckCoercion(node, type_, var->type(), CvtContext::Argument))
        return false;
    if (auto slice = sema_->ParamNeedsSlice(node, at_))
        node = slice;
    init_ir_ = node;
    decl_->set_sema_init_rhs(node);

    return true;
}

ir::Value* ArrayValidator::ValidateRank(ArrayType* rank, Expr* init) {
    if (auto next_rank = rank->inner()->as<ArrayType>()) {
        ArrayExpr* array = init->as<ArrayExpr>();
        if (!array) {
            report(init->pos(), 47);
            return nullptr;
        }
        if ((cell)array->exprs().size() != rank->size()) {
            report(init->pos(), 47);
            return nullptr;
        }

        if (!AddCells(array->exprs().size()))
            return nullptr;

        std::vector<ir::Value*> elts;
        for (auto& expr : array->exprs()) {
            ir::Value* n = ValidateRank(next_rank, expr);
            if (!n)
                return nullptr;
            elts.push_back(n);
        }
        return new ir::Array(array, elts, array->ellipses());
    }

    if (StringExpr* str = init->as<StringExpr>()) {
        if (!rank->isCharArray()) {
            Type* from = types_->defineArray(types_->type_char(),
                                             (cell)str->text()->length() + 1);
            report(init->pos(), 134) << from << rank;
            return nullptr;
        }

        auto bytes = str->text()->length() + 1;
        // The string initializer contributes |bytes| char elements.
        if (!AddCells(bytes))
            return nullptr;

        if (rank->size() && bytes > static_cast<size_t>(rank->size())) {
            report(str->pos(), 47);
            return nullptr;
        }
        return new ir::String(str);
    }

    // |rank_size| is the declared element count (0 if unbounded). It is used
    // both to bound the number of initializer elements and to track storage
    // size for the overflow guard. The compiler reasons in elements, not bytes;
    // byte layout is the VM's concern, so this is a plain element count.
    cell rank_size = rank->size();

    ArrayExpr* array = init->as<ArrayExpr>();
    if (!array) {
        // Detect and rewrite a very common anti-pattern for backwards
        // compatibility:
        //
        //    int x[10] = 0;
        if (rank->inner()->isEnumStruct() || !decl_ || at_->inner()->isArray() || !at_->size()) {
            report(init->pos(), 47);
            return nullptr;
        }

        ir::Value* node = sema_->CheckExpr(init);
        if (!node)
            return nullptr;

        if (node->val().ident != iCONSTEXPR) {
            report(init->pos(), 47);
            return nullptr;
        }

        report(init->pos(), 241);

        std::vector<Expr*> exprs = {init};

        array = new ArrayExpr(init->pos(), exprs, true);
        array->set_synthesized_for_compat();
        decl_->set_init(array);
    }

    if (auto es = rank->inner()->asEnumStruct()) {
        std::vector<ir::Value*> elts;
        for (const auto& expr : array->exprs()) {
            ir::Value* node = sema_->ValidateEnumStructInitializer(es, expr, nullptr);
            if (!node)
                continue;
            elts.push_back(node);
        }
        if (elts.size() != array->exprs().size())
            return nullptr;
        return new ir::Array(array, elts, array->ellipses());
    }

    if (rank_size) {
        if (rank_size < (cell)array->exprs().size()) {
            report(init->pos(), 47);
            return nullptr;
        }
    } else {
        // There is no actual reason to forbid this, as it works fine in the
        // current compiler. But we don't want to open any additional cans
        // of worms yet.
        if (decl_ && decl_->vclass() != sARGUMENT && !decl_->type_info().has_postdims) {
            report(init->pos(), 160);
            return nullptr;
        }
    }

    bool prev1 = false, prev2 = false;
    std::vector<ir::Value*> elts;
    for (auto& expr : array->exprs()) {
        ir::Value* n = sema_->CheckExpr(expr);
        elts.push_back(n);
        if (!n)
            continue;

        AutoErrorPos pos(expr->pos());

        if (expr->as<StringExpr>()) {
            report(expr, 47);
            continue;
        }

        const auto& v = n->val();
        if (v.ident != iCONSTEXPR) {
            report(expr, 8);
            continue;
        }

        sema_->CheckCoercion(n, rank->inner(), v.type(), CvtContext::Assignment);

        prev2 = prev1;
        if (v.ident == iCONSTEXPR)
            prev1 = true;
    }

    cell ncells = rank_size ? rank_size : array->exprs().size();
    if (!AddCells(ncells))
        return nullptr;

    if (array->ellipses()) {
        if (array->exprs().empty()) {
            // Invalid ellipses, array size unknown.
            report(array->pos(), 41);
            return nullptr;
        }
        if (rank->inner()->isInt64()) {
            report(array->exprs().back()->pos(), 68) << rank->inner();
            return nullptr;
        }
        if (prev1 && prev2 && !rank->inner()->isInt()) {
            // Unknown stepping type.
            report(array->exprs().back()->pos(), 68) << rank->inner();
            return nullptr;
        }
        if (!rank_size ||
            (rank_size == (cell)array->exprs().size() && !array->synthesized_for_compat()))
        {
            // Initialization data exceeds declared size.
            report(array->exprs().back()->pos(), 18);
            return nullptr;
        }
    }
    return new ir::Array(array, elts, array->ellipses());
}

bool ArrayValidator::AddCells(size_t ncells) {
    if (!ke::IsUintAddSafe<uint32_t>(total_cells_, ncells)) {
        report(pos_, 52);
        return false;
    }

    total_cells_ += ncells;
    if (total_cells_ >= kMaxCells) {
        report(pos_, 52);
        return false;
    }
    return true;
}

bool Semantics::AddImplicitDynamicInitializer(VarDeclBase* decl) {
    // Enum structs should be impossible here.
    typeinfo_t* type = decl->mutable_type_info();
    assert(!type->type->asEnumStruct());

    // If any one rank was dynamic, the entire array is considered dynamic. For
    // new-style fixed arrays we've thrown an error at this point. For old
    // style, we need to synthesize an initializer.
    //
    // Rewrite dim_exprs into an array initializer. If an initializer
    // already exists, leave it, because it's illegal and we want to error
    // in the semantic pass.
    //
    // Note that these declarations use old tag-based syntax, and therefore
    // do not work with enum structs, which create implicit dimensions.
    TypenameInfo ti = type->ToTypenameInfo();

    std::vector<Expr*> exprs;
    for (size_t i = 0; i < type->dim_exprs.size(); i++) {
        Expr* expr = type->dim_exprs[i];
        if (!expr) {
            report(decl->pos(), 184);
            return false;
        }
        exprs.emplace_back(expr);
    }

    assert(!decl->init_rhs());

    auto init = new NewArrayExpr(decl->pos(), ti, exprs);
    decl->set_init(init);
    if (!decl->autozero())
        init->set_no_autozero();
    return true;
}

bool Semantics::CheckArrayDeclaration(VarDeclBase* decl) {
    AutoCountErrors errors;

    if (decl->implicit_dynamic_array()) {
        assert(!decl->init_rhs());
        if (!AddImplicitDynamicInitializer(decl))
            return false;
    }

    ArrayValidator validator(this, decl);
    if (!validator.Validate() || !errors.ok())
        return false;

    if (decl->init()) {
        assert(validator.init_ir());
        decl->set_sema_init_rhs(validator.init_ir());
    }

    // We need an explicit initializer so that EmitArrayCtor() will generate
    // the appropriate NEWARRAY or NEWBULKARRAY opcode via EmitNewArrayExpr.
    if (!decl->init_rhs() && decl->vclass() != sARGUMENT) {
        if (auto array = decl->type()->as<ArrayType>()) {
            if (!array->is_fixed()) {
                report(decl->pos(), 478);
                return false;
            }
            if (!array->is_flat()) {
                if (!AddImplicitDynamicInitializer(decl))
                    return false;
                auto na = decl->init_rhs()->to<NewArrayExpr>();
                std::vector<ir::Value*> dim_nodes;
                for (auto dim : na->exprs()) {
                    ir::Value* dim_node = CheckExpr(dim, EXPR_ALLOW_TYPE_SYMS);
                    if (!dim_node)
                        return false;
                    dim_nodes.emplace_back(dim_node);
                }
                decl->set_sema_init_rhs(new ir::NewArray(na, dim_nodes));
            }
        }
    }

    return true;
}

} // namespace cc
} // namespace sp
