// vim: set ts=8 sts=4 sw=4 tw=99 et:
//  Pawn compiler - Recursive descend expresion parser
//
//  Copyright (c) ITB CompuPhase, 1997-2005
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
//
//  Version: $Id$

#include <amtl/am-maybe.h>
#include <amtl/am-utility.h>
#include "array-helpers.h"
#include "errors.h"
#include "lexer-inl.h"
#include "semantics.h"
#include "symbols.h"
#include "type-checker.h"

class ArraySizeResolver
{
  public:
    ArraySizeResolver(Semantics* sema, VarDeclBase* decl);
    ArraySizeResolver(Semantics* sema, const token_pos_t& pos, typeinfo_t* type, int vclass);

    void Resolve();

  private:
    bool ResolveDimExprs();
    void PrepareDimArray();
    void ResolveRank(int rank, Expr* init);
    void SetRankSize(Expr* expr, int rank, int size);
    bool ResolveDimExpr(Expr* expr, value* v);

  private:
    Semantics* sema_;
    TypeDictionary* types_;
    const token_pos_t& pos_;
    typeinfo_t* type_;
    Expr* initializer_;
    std::vector<int> computed_;
    int vclass_;
    AutoCountErrors errors_;
    Type* es_;
};

static constexpr int kSizeUnknown = -1;
static constexpr int kSizeIndeterminate = -2;

ArraySizeResolver::ArraySizeResolver(Semantics* sema, VarDeclBase* decl)
  : sema_(sema),
    types_(sema->cc().types()),
    pos_(decl->pos()),
    type_(decl->mutable_type()),
    initializer_(decl->init_rhs()),
    computed_(type_->dim.size()),
    vclass_(decl->vclass()),
    es_(nullptr)
{
    Type* type = types_->find(type_->semantic_tag());
    if (type->isEnumStruct())
        es_ = type;
}

ArraySizeResolver::ArraySizeResolver(Semantics* sema, const token_pos_t& pos, typeinfo_t* type,
                                     int vclass)
  : sema_(sema),
    pos_(pos),
    type_(type),
    initializer_(nullptr),
    computed_(type_->dim.size()),
    vclass_(vclass)
{
}

void
ArraySizeResolver::Resolve()
{
    assert(type_->ident == iARRAY);
    assert(type_->has_postdims);

    // If the array has old-style dimensions, we analyze them now. This is
    // technically a violation of the normal parsing order. The experimental
    // compiler had a special constant evaluator that could suppress syntax
    // errors. We don't have that, so we have to live with this instead.
    //
    // Note that there shouldn't really be any difference in behavior, since
    // all usable symbols are already entered, and their types will not
    // change between binding and later analysis.
    if (!ResolveDimExprs())
        return;

    PrepareDimArray();

    // If the array was converted to an iREFARRAY, we can skip everything
    // else because the initializer cannot be used for computing a size.
    if (type_->ident == iREFARRAY)
        return;

    // Traverse the initializer if present. For arguments, initializers
    // don't participate in determining the fixed size.
    if (vclass_ != sARGUMENT && initializer_) {
        ResolveRank(0, initializer_);

        // If we hit errors resolving the type, don't bother using any values
        // we computed along the way.
        if (!errors_.ok())
            return;
    }

    // Any kind of indeterminate status gets forced back to 0. Semantic
    // analysis will catch type or other size errors in the initializer.
    bool indeterminate = false;
    for (int i = 0; i < type_->numdim(); i++) {
        if (type_->dim[i])
            continue;

        if (computed_[i] >= 0)
            type_->dim[i] = computed_[i];
        else if (i != type_->numdim() - 1)
            indeterminate = true;
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
        if (vclass_ == sARGUMENT && type_->dim[type_->numdim() - 1]) {
            // As noted in ResolveDimExprs, we allow this for arguments as long
            // as the last dimension is filled.
        } else if (vclass_ == sLOCAL) {
            error(pos_, 159);
        } else {
            error(pos_, 183);
        }
    }
}

void
ArraySizeResolver::PrepareDimArray()
{
    for (int i = 0; i < type_->numdim(); i++) {
        if (type_->dim[i] != 0) {
            computed_[i] = type_->dim[i];
        } else {
            // If we're an iARRAY, we need an initializer and this will be
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
        }
    }
}

void
ArraySizeResolver::ResolveRank(int rank, Expr* init)
{
    if (es_ && type_->is_implicit_dim(rank)) {
        // Enum struct assignments are validated later - the size of the last
        // rank is fixed by the compiler.
        assert(type_->dim[rank] > 0);
        return;
    }

    if (StringExpr* expr = init->as<StringExpr>()) {
        if (rank != type_->numdim() - 1) {
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

    if (!type_->dim[rank] && expr->ellipses())
        error(expr->pos(), 41);

    SetRankSize(expr, rank, expr->exprs().size());

    for (const auto& child : expr->exprs())
        ResolveRank(rank + 1, child);
}

void
ArraySizeResolver::SetRankSize(Expr* expr, int rank, int size)
{
    if (computed_[rank] == kSizeUnknown) {
        computed_[rank] = size;
        return;
    }
    if (computed_[rank] == size)
        return;

    if (rank == type_->numdim() - 1) {
        // The final rank is allowed to vary as long as the size was not
        // explicitly specified. If it was specified, we'll error during
        // semantic analysis, so there's no need to handle it now.
        if (!type_->dim[rank])
            computed_[rank] = kSizeIndeterminate;
    } else if (computed_[rank] > 0) {
        // Intermediate ranks must not vary in size.
        error(expr->pos(), 47);
        computed_[rank] = kSizeIndeterminate;
    }
}

bool
ArraySizeResolver::ResolveDimExprs()
{
    for (int i = 0; i < type_->numdim(); i++) {
        Expr* expr = type_->get_dim_expr(i);
        if (!expr) {
            // If we implicitly added a final dim, skip it here.
            if (type_->is_implicit_dim(i))
                continue;

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
            if (i == type_->numdim() - 1 && vclass_ == sARGUMENT && type_->is_new) {
                error(pos_, 183);
                return false;
            }

            // Enum fields must always be fixed size.
            if (vclass_ == sENUMFIELD) {
                error(pos_, 183);
                return false;
            }
            continue;
        }

        assert(!type_->is_implicit_dim(i));

        value v;
        if (!ResolveDimExpr(expr, &v))
            return false;

        if (!is_valid_index_tag(v.tag)) {
            report(expr->pos(), 77) << types_->find(v.tag)->prettyName();
            return false;
        }

        if (v.ident != iCONSTEXPR) {
            // Non-constant expressions in postdims is illegal for transitional
            // syntax:
            //     int blah[y];
            //              ^-- no
            if (type_->is_new) {
                report(expr->pos(), 161) << type_to_name(type_->tag());
                return false;
            }

            // Old-style dynamic arrays are only allowed in local scope.
            if (vclass_ != sLOCAL) {
                error(expr->pos(), 162);
                return false;
            }
            assert(type_->dim[i] == 0);

            // The array type must automatically become iREFARRAY.
            type_->ident = iREFARRAY;
        } else if (IsLegacyEnumTag(sema_->current_scope(), v.tag) && v.sym && v.sym->enumroot) {
            error(expr->pos(), 153);
            return false;
        } else {
            // Constant must be > 0.
            if (v.constval() <= 0) {
                error(expr->pos(), 9);
                return false;
            }
            type_->dim[i] = v.constval();
        }
    }
    return true;
}

bool
ArraySizeResolver::ResolveDimExpr(Expr* expr, value* v)
{
    auto& sc = *sema_->context();
    if (!expr->Bind(sc))
        return false;

    if (auto sym_expr = expr->as<SymbolExpr>()) {
        // Special case this:
        //   enum X { ... };
        //   int blah[X];
        //
        // For backward compatibility with a huge number of plugins.
        auto sym = sym_expr->sym();
        auto type = types_->find(sym->tag);
        if (sym->enumroot && !type->asEnumStruct() && sym->ident == iCONSTEXPR) {
            *v = {};
            v->set_constval(sym->addr());
            return true;
        }
    }

    if (!sema_->CheckExpr(expr))
        return false;

    *v = expr->val();
    return true;
}

void
ResolveArraySize(Semantics* sema, VarDeclBase* decl)
{
    assert(decl->type().ident == iARRAY);

    ArraySizeResolver resolver(sema, decl);
    resolver.Resolve();
}

void
ResolveArraySize(Semantics* sema, const token_pos_t& pos, typeinfo_t* type, int vclass)
{
    assert(type->ident == iARRAY);

    ArraySizeResolver resolver(sema, pos, type, vclass);
    resolver.Resolve();
}

class FixedArrayValidator final
{
  public:
    FixedArrayValidator(Semantics* sema, VarDeclBase* decl)
      : sema_(sema),
        types_(sema->cc().types()),
        decl_(decl),
        pos_(decl->pos()),
        init_(decl->init_rhs()),
        type_(decl->type()),
        es_(nullptr)
    {
    }

    FixedArrayValidator(Semantics* sema, const typeinfo_t& type, Expr* init)
      : sema_(sema),
        types_(sema->cc().types()),
        decl_(nullptr),
        pos_(init->pos()),
        init_(init),
        type_(type),
        es_(nullptr)
    {}

    bool Validate();

  private:
    bool ValidateRank(int rank, Expr* init);
    bool ValidateEnumStruct(Expr* init);
    bool AddCells(size_t ncells);
    bool CheckArgument(Expr* init);

  private:
    Semantics* sema_;
    TypeDictionary* types_;
    VarDeclBase* decl_;
    token_pos_t pos_;
    Expr* init_;
    const typeinfo_t& type_;
    unsigned total_cells_ = 0;
    Type* es_;
};

bool
CheckArrayInitialization(Semantics* sema, const typeinfo_t& type, Expr* init)
{
    FixedArrayValidator av(sema, type, init);

    AutoCountErrors errors;
    return av.Validate() && errors.ok();
}

bool
FixedArrayValidator::Validate()
{
    Type* type = types_->find(type_.semantic_tag());
    if (type->isEnumStruct())
        es_ = type;

    if (init_) {
        if (decl_ && decl_->vclass() == sARGUMENT)
            return CheckArgument(init_);
        if (!ValidateRank(0, init_))
            return false;
        return true;
    }

    for (int i = 0; i < type_.numdim(); i++) {
        if (!type_.dim[i] && decl_ && decl_->vclass() != sARGUMENT) {
            report(decl_->pos(), 46) << decl_->name()->chars();
            return true;
        }
    }

    // Quick, non-recursive computation. For example take [3][4][5]:
    //   3 + (3 * 4) + (3 * 4 * 5)
    //
    // The calculation is not simply 3*4*5 because of indirection vectors.
    unsigned last_size = 1;
    for (int i = 0; i < type_.numdim(); i++) {
        if (!type_.dim[i])
            break;
        if (!ke::IsUintMultiplySafe<uint32_t>(last_size, type_.dim[i])) {
            error(pos_, 52);
            return false;
        }
        last_size *= type_.dim[i];
        if (last_size >= kMaxCells) {
            error(pos_, 52);
            return false;
        }
        if (!AddCells(last_size))
            return false;
    }
    return true;
}

cell
CalcArraySize(symbol* sym)
{
    auto types = CompileContext::get().types();

    cell size = 0;
    cell last_size = 1;
    for (int i = 0; i < sym->dim_count(); i++) {
        cell length = sym->dim(i);
        assert(length);

        if (i == sym->dim_count() - 1 && sym->tag == types->tag_string())
            length = char_array_cells(length);

        last_size *= length;
        size += last_size;
    }
    return size;
}

bool
FixedArrayValidator::CheckArgument(Expr* init)
{
    // As a special exception, array arguments can be initialized with a global
    // reference.
    SymbolExpr* expr = init->as<SymbolExpr>();
    if (!expr)
        return ValidateRank(0, init);

    symbol* sym = expr->sym();
    if (!sym) {
        // This failed to bind, and we're still in the binding phase, so just
        // return false.
        return false;
    }

    assert(sym->vclass == sGLOBAL);

    if (sym->ident != iARRAY && sym->ident != iREFARRAY) {
        report(expr->pos(), 134) << type_to_name(type_.tag()) << "array";
        return false;
    }
    if (type_.tag() != sym->tag) {
        report(expr->pos(), 134) << type_to_name(type_.tag()) << type_to_name(sym->tag);
        return false;
    }

    std::vector<int> dim;
    for (int i = 0; i < sym->dim_count(); i++)
        dim.emplace_back(sym->dim(i));

    if (dim.size() != type_.dim.size()) {
        report(expr->pos(), 19) << type_.numdim() << dim.size();
        return false;
    }

    // No more checks needed if we want an iREFARRAY, since iARRAYs convert
    // implicitly.
    if (type_.ident == iREFARRAY)
        return true;

    for (size_t i = 0; i < type_.dim.size(); i++) {
        if (type_.dim[i] && type_.dim[i] != dim[i]) {
            error(expr->pos(), 48);
            return false;
        }
    }
    return true;
}

bool
FixedArrayValidator::ValidateRank(int rank, Expr* init)
{
    if (rank != type_.numdim() - 1) {
        ArrayExpr* array = init->as<ArrayExpr>();
        if (!array) {
            error(init->pos(), 47);
            return false;
        }
        if ((cell)array->exprs().size() != type_.dim[rank]) {
            error(init->pos(), 47);
            return false;
        }

        if (!AddCells(array->exprs().size()))
            return false;

        for (const auto& expr : array->exprs()) {
            if (!ValidateRank(rank + 1, expr))
                return false;
        }
        return true;
    }

    // If the last rank is an enum struct, we take a separate verification path.
    if (es_ && type_.is_implicit_dim(rank)) {
       if (!ValidateEnumStruct(init))
           return false;
       if (!AddCells(type_.dim[rank]))
           return false;
       return true;
    }

    if (StringExpr* str = init->as<StringExpr>()) {
        if (type_.tag() != types_->tag_string()) {
            report(init->pos(), 134) << types_->find(types_->tag_string())->prettyName()
                                     << types_->find(type_.tag())->prettyName();
            return false;
        }

        auto cells = char_array_cells(str->text()->length() + 1);
        if (!AddCells(cells))
            return false;

        if (type_.dim[rank] && cells > type_.dim[rank]) {
            error(str->pos(), 47);
            return false;
        }
        return true;
    }

    cell rank_size = (type_.tag() == types_->tag_string() && type_.dim[rank])
                     ? char_array_cells(type_.dim[rank])
                     : type_.dim[rank];

    ArrayExpr* array = init->as<ArrayExpr>();
    if (!array) {
        // Detect and rewrite a very common anti-pattern for backwards
        // compatibility:
        //
        //    int x[10] = 0;
        if (!decl_ || type_.numdim() != 1) {
            error(init->pos(), 47);
            return false;
        }

        if (!sema_->CheckExpr(init))
            return false;

        if (init->val().ident != iCONSTEXPR) {
            error(init->pos(), 47);
            return false;
        }

        report(init->pos(), 241);

        std::vector<Expr*> exprs = {init};

        array = new ArrayExpr(init->pos(), exprs, true);
        array->set_synthesized_for_compat();
        decl_->set_init(array);
    }

    if (rank_size && rank_size < (cell)array->exprs().size()) {
        error(init->pos(), 47);
        return false;
    }

    ke::Maybe<cell> prev1, prev2;
    for (const auto& expr : array->exprs()) {
        if (!sema_->CheckExpr(expr))
            continue;

        AutoErrorPos pos(expr->pos());

        if (expr->as<StringExpr>()) {
            error(47);
            continue;
        }

        const auto& v = expr->val();
        if (v.ident != iCONSTEXPR) {
            error(8);
            continue;
        }

        matchtag(type_.tag(), v.tag, MATCHTAG_COERCE);

        prev2 = prev1;
        prev1 = ke::Some(v.constval());
    }

    cell ncells = rank_size ? rank_size : array->exprs().size();
    if (!AddCells(ncells))
        return false;

    if (array->ellipses()) {
        if (array->exprs().empty()) {
            // Invalid ellipses, array size unknown.
            error(array->pos(), 41);
            return true;
        }
        if (prev1.isValid() && prev2.isValid() && type_.tag()) {
            // Unknown stepping type.
            report(array->exprs().back()->pos(), 68) << type_to_name(type_.tag());
            return false;
        }
        if (!rank_size ||
            (rank_size == (cell)array->exprs().size() && !array->synthesized_for_compat()))
        {
            // Initialization data exceeds declared size.
            error(array->exprs().back()->pos(), 18);
            return false;
        }
    }
    return true;
}

bool
FixedArrayValidator::ValidateEnumStruct(Expr* init)
{
    assert(type_.dim[type_.numdim() - 1] > 0);

    ArrayExpr* array = init->as<ArrayExpr>();
    if (!array) {
        error(init->pos(), 47);
        return false;
    }

    symbol* esroot = es_->asEnumStruct();
    auto& field_list = esroot->data()->asEnumStruct()->fields;
    auto field_iter = field_list.begin();

    for (const auto& expr : array->exprs()) {
        if (field_iter == field_list.end()) {
            error(expr->pos(), 91);
            return false;
        }

        symbol* field = *field_iter;

        // Advance early so we can use |continue|.
        field_iter++;

        typeinfo_t type = TypeInfoFromSymbol(field);
        if (type.ident == iARRAY) {
            if (!CheckArrayInitialization(sema_, type, expr))
                continue;
        } else {
            AutoErrorPos pos(expr->pos());

            if (!sema_->CheckExpr(expr))
                continue;

            const auto& v = expr->val();
            if (v.ident != iCONSTEXPR) {
                error(8);
                continue;
            }

            matchtag(field->tag, v.tag, MATCHTAG_COERCE | MATCHTAG_ENUM_ASSN);
        }
    }

    if (array->ellipses()) {
        error(array->pos(), 80);
        return false;
    }
    return true;
}

bool
FixedArrayValidator::AddCells(size_t ncells)
{
    if (!ke::IsUintAddSafe<uint32_t>(total_cells_, ncells)) {
        error(pos_, 52);
        return false;
    }

    total_cells_ += ncells;
    if (total_cells_ >= kMaxCells) {
        error(pos_, 52);
        return false;
    }
    return true;
}

bool
Semantics::AddImplicitDynamicInitializer(VarDeclBase* decl)
{
    // Enum structs should be impossible here.
    typeinfo_t* type = decl->mutable_type();
    assert(!types_->find(type->tag())->asEnumStruct());

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
    for (int i = 0; i < type->numdim(); i++) {
        Expr* expr = type->get_dim_expr(i);
        if (!expr) {
            error(decl->pos(), 184);
            return false;
        }
        exprs.emplace_back(expr);
    }

    auto init = new NewArrayExpr(decl->pos(), ti, exprs);
    if (!CheckNewArrayExprForArrayInitializer(init))
        return false;
    if (!decl->init_rhs())
        decl->set_init(init);
    if (!decl->autozero())
        init->set_no_autozero();

    // Since we could have added EmitOnlyExprs during analysis, make sure
    // we don't analyze these again. This is a pretty gross hack we should
    // look to remove in the future.
    init->set_analysis_result(true);
    return true;
}

bool Semantics::CheckArrayDeclaration(VarDeclBase* decl) {
    AutoCountErrors errors;
    const auto& type = decl->type();
    if (type.ident == iARRAY || decl->vclass() == sARGUMENT) {
        FixedArrayValidator validator(this, decl);
        if (!validator.Validate() || !errors.ok())
            return false;
        return true;
    }

    // The array is dynamic, and either a declaration like:
    //   int[] x
    //
    // Or:
    //   new x[y]
    assert(type.ident == iREFARRAY);

    Expr* init = decl->init_rhs();
    if (!init) {
        if (decl->vclass() == sARGUMENT)
            return true;

        if (type.is_new) {
            error(decl->pos(), 101);
            return false;
        }
        return AddImplicitDynamicInitializer(decl);
    }

    if (!CheckExprForArrayInitializer(init))
        return false;

    if (type.is_new && type.isCharArray()) {
        if (init->as<StringExpr>())
            return true;
    }

    if (decl->vclass() == sARGUMENT) {
        error(init->pos(), 185);
        return false;
    }

    NewArrayExpr* ctor = init->as<NewArrayExpr>();
    if (!ctor) {
        error(init->pos(), 160);
        return false;
    }

    if (ctor->tag() != type.semantic_tag()) {
        report(ctor->pos(), 164) << type_to_name(ctor->tag()) << type_to_name(type.semantic_tag());
        return false;
    }

    size_t expected_dims = type.numdim();
    if (types_->find(type.semantic_tag())->isEnumStruct())
        expected_dims--;
    if (expected_dims != ctor->exprs().size()) {
        report(436) << (int)expected_dims << (int)ctor->exprs().size();
        return false;
    }

    return true;
}

class ArrayEmitter final
{
  public:
    ArrayEmitter(const typeinfo_t& type, Expr* init)
      : type_(type),
        es_(nullptr),
        init_(init),
        pending_zeroes_(0)
    {
        Type* t = CompileContext::get().types()->find(type.semantic_tag());
        if (t->asEnumStruct())
            es_ = t;
    }

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

    cell Emit(int rank, Expr* expr);

    size_t AddString(StringExpr* expr);
    void AddInlineArray(symbol* field, ArrayExpr* expr);
    void EmitPadding(size_t rank_size, int tag, size_t emitted, bool ellipses,
                     const ke::Maybe<cell> prev1, const ke::Maybe<cell> prev2);

  private:
    const typeinfo_t& type_;
    Type* es_;
    Expr* init_;
    tr::vector<cell> iv_;
    tr::vector<cell> data_;
    size_t pending_zeroes_;
};

void
ArrayEmitter::Emit()
{
    Emit(0, init_);

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

cell
ArrayEmitter::Emit(int rank, Expr* init)
{
    if (rank != type_.numdim() - 1) {
        assert(type_.dim[rank]);

        size_t start = iv_.size();
        assert(!(start & kDataFlag));

        iv_.resize(start + type_.dim[rank]);

        ArrayExpr* array = init ? init->as<ArrayExpr>() : nullptr;
        assert(!array || (array->exprs().size() == size_t(type_.dim[rank])));

        for (int i = 0; i < type_.dim[rank]; i++) {
            Expr* child = array ? array->exprs().at(i) : nullptr;

            // Note: use a temporary to store the result of Emit(), since
            // the address of iv_[start+i] could be evaluated and cached,
            // then invalidated by a resize.
            cell addr = Emit(rank + 1, child);
            iv_[start + i] = addr;
        }
        return start * sizeof(cell);
    }

    size_t start = data_size();
    assert(!(start & kDataFlag));

    bool ellipses = false;
    ke::Maybe<cell> prev1, prev2;
    if (!init) {
        assert(type_.dim[rank]);
    } else if (ArrayExpr* array = init->as<ArrayExpr>()) {
        PoolArray<symbol*>* field_list = nullptr;
        symbol** field_iter = nullptr;
        if (es_) {
            symbol* esroot = es_->asEnumStruct();
            field_list = &esroot->data()->asEnumStruct()->fields;
            field_iter = field_list->begin();
        }

        for (const auto& item : array->exprs()) {
            if (StringExpr* expr = item->as<StringExpr>()) {
                // Substrings can only appear in an enum struct. Normal 2D
                // cases would flow through the outer string check.
                assert(es_);

                size_t emitted = AddString(expr);

                symbol* field = *field_iter;
                assert(field);

                EmitPadding(field->dim(0), field->tag, emitted, false, {}, {});
            } else if (ArrayExpr* expr = item->as<ArrayExpr>()) {
                // Subarrays can only appear in an enum struct. Normal 2D cases
                // would flow through the check at the start of this function.
                assert(es_);
                symbol* field = *field_iter;
                AddInlineArray(field, expr);
            } else {
                assert(item->val().ident == iCONSTEXPR);
                add_data(item->val().constval());
                prev2 = prev1;
                prev1 = ke::Some(item->val().constval());
            }

            if (field_list) {
                assert(field_iter != field_list->end());
                field_iter++;
            }
        }
        ellipses = array->ellipses();
    } else if (StringExpr* expr = init->as<StringExpr>()) {
        AddString(expr);
    } else {
        assert(false);
    }

    size_t emitted = data_size() - start;

    EmitPadding(type_.dim[rank], type_.tag(), emitted, ellipses, prev1, prev2);

    return (start * sizeof(cell)) | kDataFlag;
}

void
ArrayEmitter::AddInlineArray(symbol* field, ArrayExpr* array)
{
    ke::Maybe<cell> prev1, prev2;

    for (const auto& item : array->exprs()) {
        assert(item->val().ident == iCONSTEXPR);
        add_data(item->val().constval());
        prev2 = prev1;
        prev1 = ke::Some(item->val().constval());
    }

    EmitPadding(field->dim(0), field->tag, array->exprs().size(),
                array->ellipses(), prev1, prev2);
}

void
ArrayEmitter::EmitPadding(size_t rank_size, int tag, size_t emitted, bool ellipses,
                          const ke::Maybe<cell> prev1, const ke::Maybe<cell> prev2)
{
    // Pad remainder to zeroes if the array was explicitly sized.
    if (tag == CompileContext::get().types()->tag_string())
        rank_size = char_array_cells(rank_size);

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

size_t
ArrayEmitter::AddString(StringExpr* expr)
{
    std::vector<cell> out;
    litadd_str(expr->text()->chars(), expr->text()->length(), &out);

    for (const auto& val : out)
        add_data(val);

    return out.size();
}

void
ArrayEmitter::add_data(cell value)
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

void
BuildArrayInitializer(const typeinfo_t& type, Expr* init, ArrayData* array)
{
    ArrayEmitter emitter(type, init);
    emitter.Emit();

    array->iv = std::move(emitter.iv());
    array->data = std::move(emitter.data());
    array->zeroes = emitter.pending_zeroes();
}

void
BuildArrayInitializer(VarDeclBase* decl, ArrayData* array, cell base_address)
{
    BuildArrayInitializer(decl->type(), decl->init_rhs(), array);

    for (auto& v : array->iv)
        v += base_address;
}
