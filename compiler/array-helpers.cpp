// vim: set ts=8 sts=4 sw=4 tw=99 et:
//  Pawn compiler - Recursive descend expresion parser
//
//  Copyright (c) ITB CompuPhase, 1997-2005
//  Copyright (C) AlliedModders LLC, 2024
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

#include <amtl/am-maybe.h>
#include <amtl/am-utility.h>
#include "array-helpers.h"
#include "errors.h"
#include "lexer-inl.h"
#include "semantics.h"
#include "symbols.h"
#include "type-checker.h"

namespace sp {
namespace cc {

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
    ir::Value* ResolveDimExpr(Expr* expr);

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
        report(pos_, 39);
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
    type_->type = types->defineArray(type_->type, computed_.data(), computed_.size());
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

    if (rank == computed_.size() - 1) {
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
    std::vector<ir::Value*> dim_vals;
    bool implicit_dynamic_array = false;

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

        ir::Value* val = ResolveDimExpr(expr);
        if (!val)
            return false;

        if (!IsValidIndexType(*val->type())) {
            report(expr->pos(), 77) << val->type();
            return false;
        }

        auto cv = val->as<ir::Const>();
        if (!cv) {
            // Non-constant expressions in postdims is illegal for transitional
            // syntax:
            //     int blah[y];
            //              ^-- no
            if (type_->is_new) {
                report(expr->pos(), 161) << type_->type;
                return false;
            }

            // Old-style dynamic arrays are only allowed in local scope.
            if (vclass_ != sLOCAL) {
                report(expr->pos(), 162);
                return false;
            }
            computed_[i] = 0;
            implicit_dynamic_array = true;
        } else {
            // Constant must be > 0.
            if (cv->value() <= 0) {
                report(expr->pos(), 9);
                return false;
            }
            computed_[i] = cv->value();
        }

        dim_vals.resize(i + 1);
        dim_vals[i] = val;
    }

    if (implicit_dynamic_array) {
        if (decl_->init()) {
            report(decl_, 135);
            return false;
        }
        decl_->set_implicit_dynamic_array(std::move(dim_vals));
    }
    return true;
}

ir::Value* ArrayTypeResolver::ResolveDimExpr(Expr* expr) {
    auto& sc = *sema_->context();

    if (auto sym_expr = expr->as<SymbolExpr>()) {
        // Special case this:
        //   enum X { ... };
        //   int blah[X];
        //
        // For backward compatibility with a huge number of plugins.
        auto decl = sym_expr->decl();
        if (auto ed = decl->as<EnumDecl>())
            return new ir::Const(expr, sc.cc().types()->get_int(), ed->array_size());
    }

    return sema_->CheckRvalue(expr);
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
        type_(decl->type()),
        es_(nullptr)
    {
    }

    ArrayValidator(Semantics* sema, const typeinfo_t& type, Expr* init)
      : sema_(sema),
        types_(sema->cc().types()),
        decl_(nullptr),
        pos_(init->pos()),
        init_(init),
        type_(type.type),
        es_(nullptr)
    {}

    bool Validate(ir::Value** new_init);

  private:
    ir::Value* ValidateInitializer();
    ir::Value* ValidateRank(ArrayType* rank, Expr* init);
    ir::Value* ValidateEnumStruct(EnumStructDecl* es, Expr* init);
    ir::Value* CheckArgument(SymbolExpr* init);
    bool AddCells(size_t ncells);

  private:
    Semantics* sema_;
    TypeManager* types_;
    VarDeclBase* decl_;
    token_pos_t pos_;
    Expr* init_;
    Type* type_;
    ArrayType* at_;
    unsigned total_cells_ = 0;
    EnumStructDecl* es_;
};

bool CheckArrayInitialization(Semantics* sema, const typeinfo_t& type, Expr* init,
                              ir::Value** new_init)
{
    ArrayValidator av(sema, type, init);

    AutoCountErrors errors;

    if (!av.Validate(new_init)) {
        // Make sure all fail paths return an error.
        assert(!errors.ok());
        return false;
    }

    // Make sure there were no implicit errors. This should go away when
    // matchtag() goes away.
    assert(errors.ok());
    return true;
}

bool ArrayValidator::Validate(ir::Value** new_init) {
    es_ = type_->asEnumStruct();
    at_ = type_->as<ArrayType>();

    if (init_) {
        *new_init = ValidateInitializer();
        return *new_init != nullptr;
    }

    *new_init = nullptr;

    if (!at_)
        return true;

    // The array has no initializer, which means it was declared as a fixed
    // size array.
    auto iter = at_;
    do {
        if (!iter->size() && decl_ && decl_->vclass() != sARGUMENT) {
            report(decl_->pos(), 46) << decl_->name();
            return false;
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

ir::Value* ArrayValidator::ValidateInitializer() {
    // As a special exception, array arguments can be initialized with a global
    // reference.
    if (decl_ && decl_->vclass() == sARGUMENT) {
        if (auto expr = init_->as<SymbolExpr>())
            return CheckArgument(expr);
    }

    // Handle enum structs here (gross, yes).
    if (es_) {
        if (auto array = init_->as<ArrayExpr>())
            return ValidateEnumStruct(es_, array);

        report(448);
        return nullptr;
    }

    // Check for dynamic initializers.
    if (auto ctor = init_->as<NewArrayExpr>()) {
        auto iter = at_;
        do {
            assert(!iter->size() || (decl_ && decl_->implicit_dynamic_array()));
            iter = iter->inner()->as<ArrayType>();
        } while (iter);

        TypeChecker tc(ctor, at_, ctor->type(), TypeChecker::Assignment);
        if (!tc.Check())
            return nullptr;

        return sema_->CheckNewArrayExprForArrayInitializer(ctor);
    }

    // Probably not a dynamic array, check for a fixed initializer.
    return ValidateRank(at_, init_);
}

cell CalcArraySize(Type* type) {
    auto array = type->to<ArrayType>();

    cell size = 0;
    cell last_size = 1;
    do {
        cell length = array->size();
        assert(length);

        auto next_array = array->inner()->as<ArrayType>();

        if (!next_array) {
            if (array->inner()->isChar())
                length = char_array_cells(length);
            else if (auto es = array->inner()->asEnumStruct())
                length *= es->array_size();
        }

        last_size *= length;
        size += last_size;

        array = next_array;
    } while (array);
    return size;
}

ir::Value* ArrayValidator::CheckArgument(SymbolExpr* expr) {
    Decl* decl = expr->decl();
    if (!decl)
        return nullptr;

    VarDecl* var = decl->as<VarDecl>();
    if (!var)
        return nullptr;

    assert(var->vclass() == sGLOBAL);

    TypeChecker tc(expr, type_, var->type(), TypeChecker::Argument);
    if (!tc.Check())
        return nullptr;

    // :TODO: make VariableRef
    assert(false);
    return nullptr;
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

        std::vector<ir::Value*> values;
        for (const auto& expr : array->exprs()) {
            auto val = ValidateRank(next_rank, expr);
            if (!val)
                return nullptr;
            values.emplace_back(val);
        }
        return new ir::ArrayInitializer(array, QualType(rank), std::move(values));
    }

    if (StringExpr* str = init->as<StringExpr>()) {
        if (!rank->isCharArray()) {
            report(init->pos(), 134) << str->val().type() << rank;
            return nullptr;
        }

        auto bytes = str->text()->length() + 1;
        auto cells = char_array_cells(bytes);
        if (!AddCells(cells))
            return nullptr;

        if (rank->size() && bytes > rank->size()) {
            report(str->pos(), 47);
            return nullptr;
        }

        return new ir::CharArrayLiteral(str, QualType(rank));
    }

    cell rank_size = 0;
    if (int dim_size = rank->size()) {
        if (rank->isCharArray())
            rank_size = char_array_cells(dim_size);
        else
            rank_size = dim_size;
    }

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

        auto val = sema_->CheckRvalue(init);
        if (!val)
            return nullptr;
        if (!val->as<ir::Const>()) {
            report(init->pos(), 47);
            return nullptr;
        }
        if (!rank_size) {
            report(init->pos(), 47);
            return nullptr;
        }
        if (!AddCells(rank_size))
            return nullptr;

        report(init->pos(), 241);

        TypeChecker tc(init, QualType(rank->inner()), val->type(), TypeChecker::Assignment);
        if (!tc.Check())
            return nullptr;

        std::vector<ir::Value*> values(rank_size, val);
        return new ir::ArrayInitializer(array, QualType(rank), std::move(values));
    }

    if (auto es = rank->inner()->asEnumStruct()) {
        std::vector<ir::Value*> values;
        for (const auto& expr : array->exprs()) {
            auto val = ValidateEnumStruct(es, expr);
            if (!val)
                return nullptr;
            values.emplace_back(val);
        }
        return new ir::ArrayInitializer(array, QualType(rank), std::move(values));
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

    ke::Maybe<cell> prev1, prev2;
    std::vector<ir::Value*> values;
    for (const auto& expr : array->exprs()) {
        if (expr->as<StringExpr>()) {
            report(expr, 47);
            return nullptr;
        }

        auto val = sema_->CheckRvalue(expr);
        if (!val)
            return nullptr;

        auto cv = val->as<ir::Const>();
        if (!cv) {
            report(expr, 8);
            return nullptr;
        }

        matchtag(rank->inner(), val->type().ptr(), MATCHTAG_COERCE);

        prev2 = prev1;
        prev1 = ke::Some(cv->value());

        values.emplace_back(cv);
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
        if (prev1.isValid() && prev2.isValid() && !rank->inner()->isInt()) {
            // Unknown stepping type.
            report(array->exprs().back()->pos(), 68) << rank->inner();
            return nullptr;
        }
        if (!rank_size || rank_size == (cell)array->exprs().size()) {
            // Initialization data exceeds declared size.
            report(array->exprs().back()->pos(), 18);
            return nullptr;
        }
    }

    return new ir::ArrayInitializer(array, QualType(rank), std::move(values));
}

ir::Value* ArrayValidator::ValidateEnumStruct(EnumStructDecl* es, Expr* init) {
    ArrayExpr* array = init->as<ArrayExpr>();
    if (!array) {
        report(init->pos(), 47);
        return nullptr;
    }

    const auto& field_list = es->fields();
    auto field_iter = field_list.begin();

    std::vector<ir::Value*> values;
    for (const auto& expr : array->exprs()) {
        if (field_iter == field_list.end()) {
            report(expr->pos(), 91);
            return nullptr;
        }

        auto field = (*field_iter);

        // Advance early so we can use |continue|.
        field_iter++;

        const auto& type = field->type_info();
        if (type.type->isArray()) {
            ir::Value* val;
            if (!CheckArrayInitialization(sema_, type, expr, &val))
                return nullptr;

            assert(val);
            values.emplace_back(val);
        } else {
            AutoErrorPos pos(expr->pos());

            auto val = sema_->CheckRvalue(expr);
            if (!val)
                return nullptr;

            if (!val->as<ir::Const>()) {
                report(expr, 8);
                return nullptr;
            }

            matchtag(type.type, val->type().ptr(), MATCHTAG_COERCE | MATCHTAG_ENUM_ASSN);
            values.emplace_back(val);
        }
    }

    assert(values.size() == array->exprs().size());

    if (array->ellipses()) {
        report(array->pos(), 80);
        return nullptr;
    }
    return new ir::ArrayInitializer(array, QualType(es->type()), std::move(values));
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

bool Semantics::CheckArrayDeclaration(VarDeclBase* decl, ir::Value** new_init) {
    AutoCountErrors errors;

    if (decl->implicit_dynamic_array()) {
        assert(!decl->init());
        *new_init = BuildImplicitDynamicInitializer(decl);
        return *new_init != nullptr;
    }

    ArrayValidator validator(this, decl);
    if (!validator.Validate(new_init)) {
        assert(!errors.ok());
        return false;
    }

    assert(errors.ok());
    return true;
}

class CompoundEmitter final
{
  public:
    CompoundEmitter(QualType type, ir::Value* init)
      : type_(type),
        init_(init),
        pending_zeroes_(0)
    {}

    void Emit();

    tr::vector<cell>& iv() {
        return iv_;
    }
    tr::vector<cell>& data() {
        return data_;
    }
    size_t pending_zeroes() const {
        return pending_zeroes_;
    }
    size_t data_size() const {
        return data_.size() + pending_zeroes_;
    }
    void add_data(cell value);

  private:
    static const int kDataFlag = 0x80000000;

    cell Emit(ArrayType* type, ir::Value* expr);

    size_t AddString(ir::CharArrayLiteral* init);
    void AddInlineArray(LayoutFieldDecl* field, ir::ArrayInitializer* init);
    void AddInlineEnumStruct(EnumStructDecl* es, ir::ArrayInitializer* init);
    void EmitPadding(size_t rank_size, QualType type, size_t emitted, bool ellipses,
                     const ke::Maybe<cell> prev1, const ke::Maybe<cell> prev2);

  private:
    QualType type_;
    ir::Value* init_;
    tr::vector<cell> iv_;
    tr::vector<cell> data_;
    size_t pending_zeroes_;
};

void CompoundEmitter::Emit() {
    if (auto es = type_->asEnumStruct()) {
        if (init_)
            AddInlineEnumStruct(es, init_->to<ir::ArrayInitializer>());
        EmitPadding(1, type_, data_size(), false, {}, {});
    } else {
        Emit(type_->to<ArrayType>(), init_);
    }

    // Leaf addresses (those pointing into data) need to be adjusted, since
    // the data block will be appended directly after the iv.
    for (size_t i = 0; i < iv_.size(); i++) {
        if (!(iv_[i] & kDataFlag))
            continue;
        iv_[i] &= ~kDataFlag;
        iv_[i] += ((cell)iv_.size() * sizeof(cell));
        assert(iv_[i] >= 0 && iv_[i] < INT_MAX);
    }
}

cell CompoundEmitter::Emit(ArrayType* rank, ir::Value* init) {
    if (rank->inner()->isArray()) {
        assert(rank->size());

        size_t start = iv_.size();
        assert(!(start & kDataFlag));

        iv_.resize(start + rank->size());

        auto array = init ? init->as<ir::ArrayInitializer>() : nullptr;
        assert(!array || (array->values().size() == size_t(rank->size())));

        // :TODO: test when sizeof(array) < sizeof(rank)
        auto inner = rank->inner()->to<ArrayType>();
        for (int i = 0; i < rank->size(); i++) {
            auto child = array ? array->values().at(i) : nullptr;

            // Note: use a temporary to store the result of Emit(), since
            // the address of iv_[start+i] could be evaluated and cached,
            // then invalidated by a resize.
            cell addr = Emit(inner, child);
            iv_[start + i] = addr;
        }
        return start * sizeof(cell);
    }

    size_t start = data_size();
    assert(!(start & kDataFlag));

    bool ellipses = false;
    ke::Maybe<cell> prev1, prev2;
    if (!init) {
        assert(rank->size());
    } else if (auto array = init->as<ir::ArrayInitializer>()) {
        for (const auto& item : array->values()) {
            if (auto inner = item->as<ir::ArrayInitializer>()) {
                // Subarrays can only appear in an enum struct. Normal 2D cases
                // would flow through the check at the start of this function.
                auto es = rank->inner()->asEnumStruct();
                AddInlineEnumStruct(es, inner);
            } else {
                auto cv = item->as<ir::Const>();
                add_data(cv->value());
                prev2 = prev1;
                prev1 = ke::Some(cv->value());
            }
        }
        // Note: expr() can be null on legacy statements like:
        //     int x[10] = 1;
        ellipses = array->HasEllipses();
    } else if (auto inner = init->as<ir::CharArrayLiteral>()) {
        AddString(inner);
    } else {
        assert(false);
    }

    if (rank->inner()->isEnumStruct()) {
        assert(!prev1.isValid());
        assert(!prev2.isValid());
        assert(!ellipses);
    }

    size_t emitted = data_size() - start;

    // This only works because enum structs are flattened and don't support
    // internal IVs. No plans to change this as it would greatly increase
    // complexity unless we radically changed arrays.
    EmitPadding(rank->size(), QualType(rank->inner()), emitted, ellipses, prev1, prev2);

    return (start * sizeof(cell)) | kDataFlag;
}

void CompoundEmitter::AddInlineEnumStruct(EnumStructDecl* es, ir::ArrayInitializer* init) {
    auto field_list = &es->fields();
    auto field_iter = field_list->begin();

    for (const auto& item : init->values()) {
        if (auto inner = item->as<ir::CharArrayLiteral>()) {
            // Substrings can only appear in an enum struct. Normal 2D
            // cases would flow through the outer string check.
            size_t emitted = AddString(inner);

            auto field = (*field_iter);
            assert(field);

            auto rank_type = field->type()->to<ArrayType>();
            EmitPadding(rank_type->size(), QualType(rank_type->inner()), emitted, false, {}, {});
        } else if (auto inner = item->as<ir::ArrayInitializer>()) {
            // Subarrays can only appear in an enum struct. Normal 2D cases
            // would flow through the check at the start of this function.
            auto field = (*field_iter);
            AddInlineArray(field, inner);
        } else {
            auto cv = item->as<ir::Const>();
            add_data(cv->value());
        }

        assert(field_iter != field_list->end());
        field_iter++;
    }
}

void CompoundEmitter::AddInlineArray(LayoutFieldDecl* field, ir::ArrayInitializer* array) {
    ke::Maybe<cell> prev1, prev2;

    for (const auto& item : array->values()) {
        auto cv = item->as<ir::Const>();
        add_data(cv->value());
        prev2 = prev1;
        prev1 = ke::Some(cv->value());
    }

    auto rank_size = field->type()->to<ArrayType>()->size();
    EmitPadding(rank_size, QualType(field->type()), array->values().size(),
                array->HasEllipses(), prev1, prev2);
}

void
CompoundEmitter::EmitPadding(size_t rank_size, QualType type, size_t emitted, bool ellipses,
                             const ke::Maybe<cell> prev1, const ke::Maybe<cell> prev2)
{
    // Pad remainder to zeroes if the array was explicitly sized.
    if (type->isChar())
        rank_size = char_array_cells(rank_size);
    else if (auto es = type->asEnumStruct())
        rank_size *= es->array_size();

    if (rank_size > emitted) {
        if (ellipses) {
            cell step = 0;
            if (prev2.isValid())
                step = prev1.get() - prev2.get();

            cell next_value = prev1.get() + step;
            while (emitted < rank_size) {
                add_data(next_value);
                next_value += step;
                emitted++;
            }
        } else {
            pending_zeroes_ += rank_size - emitted;
        }
    }
}

size_t CompoundEmitter::AddString(ir::CharArrayLiteral* init) {
    std::vector<cell> out;
    litadd_str(init->expr()->text()->chars(), init->expr()->text()->length(), &out);

    for (const auto& val : out)
        add_data(val);

    return out.size();
}

void
CompoundEmitter::add_data(cell value)
{
    if (!value) {
        pending_zeroes_++;
        return;
    }

    if (pending_zeroes_) {
        data_.resize(data_.size() + pending_zeroes_);
        pending_zeroes_ = 0;
    }
    data_.emplace_back(value);
}

void BuildCompoundInitializer(QualType type, ir::Value* init, ArrayData* array,
                              std::optional<cell_t> base_address)
{
    CompoundEmitter emitter(type, init);
    emitter.Emit();

    array->iv = std::move(emitter.iv());
    array->data = std::move(emitter.data());
    array->zeroes = emitter.pending_zeroes();

    if (base_address) {
        for (auto& v : array->iv)
            v += *base_address;
    }
}

} // namespace cc
} // namespace sp
