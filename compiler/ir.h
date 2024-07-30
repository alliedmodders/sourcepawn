// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
//  Copyright (c) AlliedModders 2024
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

#include <limits>
#include <memory>

#include <amtl/am-bits.h>
#include "ast-types.h"
#include "label.h"
#include "parse-node.h"
#include "pool-objects.h"
#include "qualtype.h"
#include "stl/stl-vector.h"

namespace sp {
namespace cc {
namespace ir {

class Function;
class IndexOp;
class Variable;

class Node : public PoolObject {
  protected:
    explicit Node(IrKind kind, ParseNode* pn)
      : kind_(kind)
    {
        pn_u.pn = pn;
    }

  public:
    ParseNode* pn() const { return pn_u.pn; }
    IrKind kind() const { return kind_; }

    template <typename T> T* as() {
        if (T::is_a(this))
            return reinterpret_cast<T*>(this);
        return nullptr;
    }
    template <typename T> T* to() {
        assert(T::is_a(this));
        return reinterpret_cast<T*>(this);
    }

  protected:
    IrKind kind_ : 8;
    union {
        ParseNode* pn;
        Expr* expr;
        Stmt* stmt;
        Decl* decl;
        UnaryExpr* unary;
        VarDecl* var_decl;
        FunctionDecl* fun_decl;
        StringExpr* string_expr;
        DoWhileStmt* do_while_stmt;
        ArgDecl* arg_decl;
        ArrayExpr* array_expr;
        ForStmt* for_stmt;
        IncDecExpr* incdec_expr;
        ThisExpr* this_expr;
        CallExpr* call_expr;
    } pn_u;
};

class Module final : public std::enable_shared_from_this<Module> {
  public:
    tr::vector<Function*>& functions() { return functions_; }
    tr::vector<Variable*>& globals() { return globals_; }

  private:
    tr::vector<Function*> functions_;
    tr::vector<Variable*> globals_;
};

class Value : public Node {
  public:
    Value(IrKind kind, ParseNode* pn, QualType type)
      : Node(kind, pn),
        type_(type)
    {}

    QualType type() const { return type_; }

    bool HasSideEffects();

  private:
    QualType type_;
};

class Insn : public Node {
  public:
    Insn(IrKind kind, ParseNode* pn)
      : Node(kind, pn)
    {}

    Insn* next() const { return next_; }
    void set_next(Insn* node) { next_ = node; }

  private:
    Insn* next_ = nullptr;
};

class InsnBlock final : public PoolObject {
    static constexpr uintptr_t kBits = 1;

  public:
    explicit InsnBlock(Insn* list, bool has_heap_allocs)
      : list_(list)
    {
        if (has_heap_allocs)
            list_ = ke::SetPointerBits<Insn>(list_, 1);
    }

    Insn* list() const { return ke::ClearPointerBits<kBits, Insn>(list_); }
    bool has_heap_allocs() const { return ke::GetPointerBits<kBits>(list_) == 1; }

  private:
    Insn* list_;
};

class NodeListBuilder final {
  public:
    NodeListBuilder() {}
    NodeListBuilder(const NodeListBuilder&) = delete;
    NodeListBuilder(NodeListBuilder&&) = delete;

    explicit NodeListBuilder(NodeListBuilder** prev_loc)
      : prev_(*prev_loc),
        prev_loc_(prev_loc)
    {
        *prev_loc_ = this;
    }

    ~NodeListBuilder() {
        if (prev_loc_) {
            assert(*prev_loc_ == this);
            *prev_loc_ = prev_;
        }
    }

    void add(Insn* node) {
        if (!first_) {
            first_ = node;
            last_ = node;
        } else {
            last_->set_next(node);
            last_ = node;
        }
    }

    template <typename T, typename... Args>
    T* emplace(Args&&... args) {
        auto ir = new T(std::forward<Args>(args)...);
        add(ir);
        return ir;
    }

    InsnBlock* Finish() {
        auto block = new InsnBlock(first_, has_heap_allocs_);
        first_ = nullptr;
        last_ = nullptr;
        has_heap_allocs_ = false;
        return block;
    }

    void set_has_heap_allocs() { has_heap_allocs_ = true; }

    NodeListBuilder& operator =(const NodeListBuilder) = delete;
    NodeListBuilder& operator =(NodeListBuilder&&) = delete;

  private:
    NodeListBuilder* prev_ = nullptr;
    NodeListBuilder** prev_loc_ = nullptr;
    Insn* first_ = nullptr;
    Insn* last_ = nullptr;
    bool has_heap_allocs_ = false;
};

class DeclNode : public Insn {
  public:
    DeclNode(IrKind kind, Decl* decl)
      : Insn(kind, decl)
    {}
};

class Variable : public DeclNode {
  public:
    Variable(VarDeclBase* var, ir::Value* init)
      : DeclNode(IrKind::Variable, var),
        init_(init),
        read_(false),
        written_(false)
    {}

    static constexpr cell_t kInvalidAddr = std::numeric_limits<cell_t>::min();

    cell_t addr() const {
        assert(addr_ != kInvalidAddr);
        return addr_;
    }
    void set_addr(cell_t addr) {
        assert(addr_ == kInvalidAddr);
        addr_ = addr;
    }

    VarDeclBase* decl() const { return pn_u.var_decl; }
    ir::Value* init() const { return init_; }
    bool read() const { return read_; }
    bool written() const { return written_; }

    void set_read() { read_ = true; }
    void set_written() { written_ = true; }

    static bool is_a(Node* node) {
        return node->kind() == IrKind::Variable || node->kind() == IrKind::Argument;
    }

  protected:
    ir::Value* init_;

  private:
    cell_t addr_ = kInvalidAddr;
    bool read_ : 1;
    bool written_ : 1;
};

class Argument final : public Variable {
  public:
    explicit Argument(ArgDecl* var, ir::Value* init)
      : Variable(var, init)
    {}

    VarDeclBase* arg_decl() const { return pn_u.arg_decl; }

    static bool is_a(Node* node) { return node->kind() == IrKind::Argument; }
};

class Function final : public DeclNode {
  public:
    explicit Function(FunctionDecl* decl)
      : DeclNode(IrKind::Function, decl)
    {}

    void AddReferenceTo(ir::Function* other);

    static bool is_a(Node* node) { return node->kind() == IrKind::Function; }

    FunctionDecl* decl() const { return pn_u.fun_decl; }
    Label& label() { return label_; }
    Label& public_id() { return public_id_; }
    PoolForwardList<Function*>* refers_to() { return refers_to_; }

    InsnBlock* body() const { return body_; }
    void set_body(InsnBlock* body) { body_ = body; }

    uint32_t pcode_end() const { return pcode_end_; }
    void set_pcode_end(uint32_t end) { pcode_end_ = end; }

    int32_t max_local_stack() const { return max_local_stack_; }
    void set_max_local_stack(int32_t stack) { max_local_stack_ = stack; }

    bool is_live() const { return is_live_; }
    void set_is_live() { is_live_ = true; }

    int32_t max_callee_stack() const { return max_callee_stack_; }
    void set_max_callee_stack(int32_t value) { max_callee_stack_ = value; }

    const PoolArray<Argument*>& argv() const { return argv_; }
    void set_argv(PoolArray<Argument*>&& argv) { argv_ = std::move(argv); }

    uint32_t num_slots() { return num_slots_; }
    void set_num_slots(uint32_t slots) { num_slots_ = slots; }

    struct ReturnArrayInfo : public PoolObject {
        cell_t hidden_address = 0;
        cell_t iv_size = 0;
        cell_t dat_addr = 0;
        cell_t zeroes = 0;
    };
    ReturnArrayInfo* return_array() const { return return_array_; }
    void set_return_array(ReturnArrayInfo* base) { return_array_ =  base; }

    static constexpr uint32_t kInvalidSlot = std::numeric_limits<uint32_t>::max();

  private:
    InsnBlock* body_ = nullptr;
    PoolForwardList<Function*>* refers_to_ = nullptr;
    Label label_;
    Label public_id_;
    uint32_t pcode_end_ = 0;
    int32_t max_local_stack_ = 0;
    int32_t max_callee_stack_ = 0;
    uint32_t num_slots_ = 0;
    bool is_live_ = false;
    PoolArray<Argument*> argv_;
    ReturnArrayInfo* return_array_ = nullptr;
};

class Return final : public Insn {
  public:
    Return(ReturnStmt* stmt, Value* val)
      : Insn(IrKind::Return, stmt),
        val_(val)
    {}

    Value* val() const { return val_; }

    static bool is_a(Node* node) { return node->kind() == IrKind::Return; }

  private:
    Value* val_;
};

class Exit final : public Insn {
  public:
    Exit(ExitStmt* stmt, Value* val)
      : Insn(IrKind::Exit, stmt)
    {}

    Value* val() const { return val_; }

    static bool is_a(Node* node) { return node->kind() == IrKind::Exit; }

  private:
    Value* val_;
};

class Assert final : public Insn {
  public:
    Assert(AssertStmt* stmt, Value* val)
      : Insn(IrKind::Assert, stmt)
    {}

    Value* val() const { return val_; }

    static bool is_a(Node* node) { return node->kind() == IrKind::Assert; }

  private:
    Value* val_;
};

class ValueInsn final : public Insn {
  public:
    ValueInsn(ExprStmt* stmt, Value* val)
      : Insn(IrKind::ValueInsn, stmt),
        val_(val)
    {}

    Value* val() const { return val_; }

    static bool is_a(Node* node) { return node->kind() == IrKind::ValueInsn; }

  private:
    Value* val_;
};

class Break final : public Insn {
  public:
    explicit Break(BreakStmt* stmt)
      : Insn(IrKind::Break, stmt)
    {}

    static bool is_a(Node* node) { return node->kind() == IrKind::Break; }
};

class Continue final : public Insn {
  public:
    explicit Continue(ContinueStmt* stmt)
      : Insn(IrKind::Continue, stmt)
    {}

    static bool is_a(Node* node) { return node->kind() == IrKind::Continue; }
};

class If final : public Insn {
  public:
    If(IfStmt* stmt, Value* cond, InsnBlock* on_true, InsnBlock* on_false)
      : Insn(IrKind::If, stmt),
        cond_(cond),
        on_true_(on_true),
        on_false_(on_false)
    {}

    Value* cond() const { return cond_; }
    InsnBlock* on_true() const { return on_true_; }
    InsnBlock* on_false() const { return on_false_; }

    static bool is_a(Node* node) { return node->kind() == IrKind::If; }

  private:
    Value* cond_;
    InsnBlock* on_true_;
    InsnBlock* on_false_;
};

class DoWhile final : public Insn {
  public:
    DoWhile(DoWhileStmt* stmt, Value* cond, InsnBlock* body)
      : Insn(IrKind::DoWhile, stmt),
        cond_(cond),
        body_(body)
    {}

    DoWhileStmt* stmt() const { return pn_u.do_while_stmt; }
    Value* cond() const { return cond_; }
    InsnBlock* body() const { return body_; }

    static bool is_a(Node* node) { return node->kind() == IrKind::DoWhile; }

  private:
    Value* cond_;
    InsnBlock* body_;
};

class Delete final : public Insn {
  public:
    Delete(DeleteStmt* stmt, Value* val, Function* dtor)
      : Insn(IrKind::Delete, stmt),
        val_(val),
        dtor_(dtor)
    {}

    Value* val() const { return val_; }
    Function* dtor() const { return dtor_; }

    static bool is_a(Node* node) { return node->kind() == IrKind::Delete; }

  private:
    Value* val_;
    Function* dtor_;
};

class ForLoop final : public Insn {
  public:
    ForLoop(ForStmt* stmt, InsnBlock* init, Value* cond, InsnBlock* advance, InsnBlock* body)
      : Insn(IrKind::ForLoop, stmt),
        init_(init),
        cond_(cond),
        advance_(advance),
        body_(body)
    {}

    ForStmt* stmt() const { return pn_u.for_stmt; }
    InsnBlock* init() const { return init_; }
    Value* cond() const { return cond_; }
    InsnBlock* advance() const { return advance_; }
    InsnBlock* body() const { return body_; }

    static bool is_a(Node* node) { return node->kind() == IrKind::ForLoop; }

  private:
    InsnBlock* init_;
    Value* cond_;
    InsnBlock* advance_;
    InsnBlock* body_;
};

class Switch final : public Insn {
  public:
    typedef std::pair<PoolArray<cell_t>, InsnBlock*> Case;

    Switch(SwitchStmt* stmt, Value* cond, std::vector<Case>&& cases, InsnBlock* default_case)
      : Insn(IrKind::Switch, stmt),
        cond_(cond),
        cases_(std::move(cases)),
        default_case_(default_case)
    {}

    Value* cond() const { return cond_; }
    const PoolArray<Case>& cases() const { return cases_; }
    InsnBlock* default_case() const { return default_case_; }

    static bool is_a(Node* node) { return node->kind() == IrKind::Switch; }

  private:
    Value* cond_;
    PoolArray<Case> cases_;
    InsnBlock* default_case_;
};

class Const final : public Value {
  public:
    Const(Expr* expr, QualType type, cell_t value)
      : Value(IrKind::ConstVal, expr, type),
        value_(value)
    {}

    cell_t value() const { return value_; }

    static bool is_a(Node* op) { return op->kind() == IrKind::ConstVal; }

  private:
    cell_t value_;
};

class CharArrayLiteral final : public Value {
  public:
    CharArrayLiteral(StringExpr* expr, QualType type)
      : Value(IrKind::CharArrayLiteral, expr, type)
    {}

    StringExpr* expr() const { return pn_u.string_expr; }

    static bool is_a(Node* op) { return op->kind() == IrKind::CharArrayLiteral; }
};

class UnaryOp final : public Value {
  public:
    UnaryOp(UnaryExpr* expr, QualType type, Value* val)
      : Value(IrKind::UnaryOp, expr, type),
        val_(val)
    {}

    Value* val() const { return val_; }
    UnaryExpr* expr() const { return pn_u.unary; }

    static bool is_a(Node* op) { return op->kind() == IrKind::UnaryOp; }

  private:
    Value* val_;
};

class CallUserOp final : public Value {
  public:
    CallUserOp(Expr* expr, QualType type, Function* target, Value* first = nullptr,
               Value* second = nullptr, bool swapped = false)
      : Value(IrKind::CallUserOp, expr, type),
        target_(target),
        first_(first),
        second_(second),
        swapped_(swapped)
    {}

    Function* target() const { return target_; }
    Value* first() const { return first_; }
    Value* second() const { return second_; }
    bool swapped() const { return swapped_; }

    static bool is_a(Node* op) { return op->kind() == IrKind::CallUserOp; }

  private:
    Function* target_;
    Value* first_;
    Value* second_;
    bool swapped_;
};

class TypeRef final : public Value {
  public:
    TypeRef(Expr* expr, QualType type)
      : Value(IrKind::TypeRef, expr, type)
    {}

    static bool is_a(Node* op) { return op->kind() == IrKind::TypeRef; }
};

class FunctionRef : public Value {
  public:
    FunctionRef(Expr* expr, QualType type, Function* fun)
      : FunctionRef(IrKind::FunctionRef, expr, type, fun)
    {}

    Function* fun() const { return fun_; }

    static bool is_a(Node* op) {
        return op->kind() == IrKind::FunctionRef ||
               op->kind() == IrKind::BoundFunction;
    }

  protected:
    FunctionRef(IrKind kind, Expr* expr, QualType type, Function* fun)
      : Value(kind, expr, type),
        fun_(fun)
    {}

  private:
    Function* fun_;
};

class BoundFunction final : public FunctionRef {
  public:
    BoundFunction(Expr* expr, QualType type, ir::Value* val, ir::Function* fun)
      : FunctionRef(IrKind::BoundFunction, expr, type, fun),
        val_(val)
    {}

    ir::Value* val() const { return val_; }

    static bool is_a(Node* op) { return op->kind() == IrKind::BoundFunction; }

  private:
    ir::Value* val_;
};

class CommaOp final : public Value {
  public:
    CommaOp(Expr* expr, QualType type, const std::vector<Value*>& values)
      : Value(IrKind::CommaOp, expr, type)
    {
        new (&values_) decltype(values_)(values);
    }

    const PoolArray<Value*>& values() const { return values_; }

    static bool is_a(Node* op) { return op->kind() == IrKind::CommaOp; }

  private:
    PoolArray<Value*> values_;
};

class CallOp final : public Value {
  public:
    CallOp(Expr* expr, QualType type, ir::FunctionRef* target, const std::vector<Value*>& values)
      : Value(IrKind::CallOp, expr, type),
        target_(target)
    {
        new (&values_) decltype(values_)(values);
    }

    ir::FunctionRef* target() const { return target_; }
    const PoolArray<Value*>& argv() const { return values_; }
    CallExpr* expr() const { return pn_u.call_expr; }

    static bool is_a(Node* op) { return op->kind() == IrKind::CallOp; }

  private:
    ir::FunctionRef* target_;
    PoolArray<Value*> values_;
};

class BinaryOp final : public Value {
  public:
    BinaryOp(Expr* expr, QualType type, int token, Value* left, Value* right)
      : Value(IrKind::BinaryOp, expr, type),
        token_(token),
        left_(left),
        right_(right)
    {}

    Value* left() const { return left_; }
    Value* right() const { return right_; }
    int token() const { return token_; }

    static bool is_a(Node* op) { return op->kind() == IrKind::BinaryOp; }

  private:
    int token_;
    Value* left_;
    Value* right_;
};

class TernaryOp final : public Value {
  public:
    TernaryOp(Expr* expr, QualType type, Value* select, Value* on_true, Value* on_false)
      : Value(IrKind::TernaryOp, expr, type),
        select_(select),
        on_true_(on_true),
        on_false_(on_false)
    {}

    Value* select() const { return select_; }
    Value* on_true() const { return on_true_; }
    Value* on_false() const { return on_false_; }

    static bool is_a(Node* op) { return op->kind() == IrKind::TernaryOp; }

  private:
    Value* select_;
    Value* on_true_;
    Value* on_false_;
};

class NewArray final : public Value {
  public:
    NewArray(Expr* expr, QualType type, std::vector<ir::Value*>&& dims)
      : Value(IrKind::NewArray, expr, type)
    {
      new (&dims_) PoolArray<ir::Value*>(dims);
    }
    NewArray(VarDeclBase* decl, QualType type, PoolArray<ir::Value*>&& dims)
      : Value(IrKind::NewArray, decl, type),
        dims_(std::move(dims))
    {}

    PoolArray<ir::Value*>& dims() { return dims_; }

    static bool is_a(Node* op) { return op->kind() == IrKind::NewArray; }

  private:
    PoolArray<ir::Value*> dims_;
};

class ArrayInitializer final : public Value {
  public:
    ArrayInitializer(Expr* expr, QualType type, std::vector<ir::Value*>&& values)
      : Value(IrKind::ArrayInitializer, expr, type),
        values_(std::move(values))
    {}

    Expr* expr() const { return pn_u.expr; }
    const PoolArray<ir::Value*>& values() const { return values_; }
    bool HasEllipses() const;

    static bool is_a(Node* node) { return node->kind() == IrKind::ArrayInitializer; }

  private:
    PoolArray<ir::Value*> values_;
};

class CastOp final : public Value {
  public:
    CastOp(Expr* expr, QualType type, ir::Value* val)
      : Value(IrKind::CastOp, expr, type),
        val_(val)
    {}

    ir::Value* val() const { return val_; }

    static bool is_a(Node* node) { return node->kind() == IrKind::CastOp; }

  private:
    ir::Value* val_;
};

class SliceArray final : public Value {
  public:
    SliceArray(Expr* expr, QualType type, ir::IndexOp* index_op)
      : Value(IrKind::SliceArray, expr, type),
        index_op_(index_op)
    {}

    ir::IndexOp* index_op() const { return index_op_; }

    static bool is_a(Node* node) { return node->kind() == IrKind::SliceArray; }

  private:
    ir::IndexOp* index_op_;
};

class Lvalue : public Value {
  public:
    Lvalue(IrKind kind, Expr* expr, QualType type)
      : Value(kind, expr, type)
    {}

    static bool is_a(Node* op) {
        return op->kind() == IrKind::VariableRef ||
               op->kind() == IrKind::IndexOp ||
               op->kind() == IrKind::PropertyRef ||
               op->kind() == IrKind::FieldRef ||
               op->kind() == IrKind::StackRef;
    }

    // Returns true if address calculation requires evaluating more than one
    // operation and thus might be considered expensive.
    bool HasComplexAddressCalculation();

    // Returns true if this l-value is addressable, and false otherwise.
    bool IsAddressable() { return kind() != IrKind::PropertyRef; }
};

class Load final : public Value {
  public:
    Load(Expr* expr, QualType type, Lvalue* lval)
      : Value(IrKind::Load, expr, type),
        lval_(lval)
    {}

    Lvalue* lval() const { return lval_; }

    static bool is_a(Node* op) { return op->kind() == IrKind::Load; }

  private:
    Lvalue* lval_;
};

class Store : public Value {
  public:
    Store(Expr* expr, QualType type, Lvalue* lval, Value* val)
      : Store(IrKind::Store, expr, type, lval, val)
    {}

    Lvalue* lval() const { return lval_; }
    Value* val() const { return val_; }

    // Not a StoreWithTemp, since it's a different op. We just use the base
    // class for convenience.
    static bool is_a(Node* op) { return op->kind() == IrKind::Store; }

  protected:
    Store(IrKind kind, Expr* expr, QualType type, Lvalue* lval, Value* val)
      : Value(kind, expr, type),
        lval_(lval),
        val_(val)
    {}

  private:
    Lvalue* lval_;
    Value* val_;
};

class StoreWithTemp final : public Store {
  public:
    StoreWithTemp(Expr* expr, QualType type, Lvalue* lval, Value* val, uint32_t temp_slot)
      : Store(IrKind::StoreWithTemp, expr, type, lval, val),
        temp_slot_(temp_slot)
    {}

    uint32_t temp_slot() const { return temp_slot_; }

    static bool is_a(Node* op) { return op->kind() == IrKind::StoreWithTemp; }

  private:
    uint32_t temp_slot_;
};

// Reference a stack slot that can hold one value. If |val| is non-null, then
// the stack ref is initialized with this value.
class StackRef final : public Lvalue {
  public:
    explicit StackRef(Expr* expr, QualType type, uint32_t slot)
      : Lvalue(IrKind::StackRef, expr, type),
        slot_(slot)
    {}

    uint32_t slot() const { return slot_; }

    static bool is_a(Node* op) { return op->kind() == IrKind::StackRef; }

  private:
    uint32_t slot_;
};

class IncDec : public Value {
  public:
    IncDec(IncDecExpr* expr, QualType type, Lvalue* lval, bool used)
      : IncDec(IrKind::IncDec, expr, type, lval, used)
    {}

    Lvalue* lval() const { return lval_; }
    bool used() const { return used_; }
    IncDecExpr* expr() const { return pn_u.incdec_expr; }

    static bool is_a(Node* op) {
        return op->kind() == IrKind::IncDec ||
               op->kind() == IrKind::IncDecUserOp;
    }

  protected:
    IncDec(IrKind kind, IncDecExpr* expr, QualType type, Lvalue* lval, bool used)
      : Value(kind, expr, type),
        lval_(lval),
        used_(used)
    {}

  private:
    Lvalue* lval_;
    bool used_;
};

// Temporary slot for passing the loaded value.
class IncDecUserOp final : public IncDec {
  public:
    IncDecUserOp(IncDecExpr* expr, QualType type, Lvalue* lval, Function* target,
                 bool used)
      : IncDec(IrKind::IncDecUserOp, expr, type, lval, used),
        target_(target)
    {}

    Function* target() const { return target_; }

    static bool is_a(Node* op) { return op->kind() == IrKind::IncDecUserOp; }

  private:
    Function* target_;
};

class VariableRef final : public Lvalue {
  public:
    VariableRef(Expr* expr, QualType type, Variable* var)
      : Lvalue(IrKind::VariableRef, expr, type),
        var_(var)
    {}

    Variable* var() const { return var_; }

    static bool is_a(Node* op) { return op->kind() == IrKind::VariableRef; }

  private:
    Variable* var_;
};

class IndexOp final : public Lvalue {
  public:
    IndexOp(Expr* expr, QualType type, Value* base, Value* index)
      : Lvalue(IrKind::IndexOp, expr, type),
        base_(base),
        index_(index)
    {}

    Value* base() const { return base_; }
    Value* index() const { return index_; }
    Expr* expr() const { return pn_u.expr; }

    static bool is_a(Node* op) { return op->kind() == IrKind::IndexOp; }

  private:
    Value* base_;
    Value* index_;
};

class FieldRef final : public Lvalue {
  public:
    FieldRef(Expr* expr, QualType type, Value* base, LayoutFieldDecl* field)
      : Lvalue(IrKind::FieldRef, expr, type),
        base_(base),
        field_(field)
    {}

    Value* base() const { return base_; }
    LayoutFieldDecl* field() const { return field_; }

    static bool is_a(Node* op) { return op->kind() == IrKind::FieldRef; }

  private:
    Value* base_;
    LayoutFieldDecl* field_;
};

class PropertyRef final : public Lvalue {
  public:
    PropertyRef(Expr* expr, QualType type, Value* val, MethodmapPropertyDecl* decl)
      : Lvalue(IrKind::PropertyRef, expr, type),
        val_(val),
        decl_(decl)
    {}

    Value* val() const { return val_; }
    MethodmapPropertyDecl* decl() const { return decl_; }
    ir::Function* getter() const { return getter_; }
    ir::Function* setter() const { return setter_; }

    void BindGetter(ir::Function* getter);
    void BindSetter(ir::Function* setter);

    static bool is_a(Node* op) { return op->kind() == IrKind::PropertyRef; }

  private:
    Value* val_;
    MethodmapPropertyDecl* decl_;

    // This is a huge hack. Maybe we need a dictionary of PropertyDecls
    // to ir::Functions.
    ir::Function* getter_ = nullptr;
    ir::Function* setter_ = nullptr;
};

// This is effectively a macro operation for:
//     CommaOp(
//             Store(StackRef(temp_slot), value),
//             AddressOf(StackRef(temp_slot)))
class TempValueRef final : public Value {
  public:
    TempValueRef(Expr* expr, QualType type, Value* val, uint32_t slot)
      : Value(IrKind::TempValueRef, expr, type),
        val_(val),
        slot_(slot)
    {}

    ir::Value* val() const { return val_; }
    uint32_t slot() const { return slot_; }

    static bool is_a(Node* op) { return op->kind() == IrKind::TempValueRef; }

  private:
    ir::Value* val_;
    uint32_t slot_;
};

// Calculate the address of an l-value. Note that this is different from
// EmitLoadStorePrologue, which may return a reference (eg, the address of
// an IndexOp that points to another array). AddressOf computes an address
// of the effective value.
class AddressOf final : public Value {
  public:
    AddressOf(Expr* expr, QualType type, Lvalue* val)
      : Value(IrKind::AddressOf, expr, type),
        lval_(val)
    {}

    Lvalue* lval() const { return lval_; }

    static bool is_a(Node* op) { return op->kind() == IrKind::AddressOf; }

  private:
    Lvalue* lval_;
};

} // namespace ir
} // namespace cc
} // namespace sp
