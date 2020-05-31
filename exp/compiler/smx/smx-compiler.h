// vim: set sts=2 ts=8 sw=2 tw=99 et:
// 
// Copyright (C) 2012 David Anderson
// 
// This file is part of SourcePawn.
// 
// SourcePawn is free software: you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation, either version 3 of the License, or (at your option)
// any later version.
// 
// SourcePawn is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.

#ifndef _include_sourcepawn_emit_emitter_h_
#define _include_sourcepawn_emit_emitter_h_

#include "libsmx/smx-builder.h"
#include "sema/expressions.h"
#include "sema/program.h"
#include "smx-assembly-buffer.h"
#include "smx-ssa.h"
#include <amtl/am-raii.h>

namespace sp {

class CompileContext;
struct ArrayInfo;

class SmxCompiler
{
public:
  SmxCompiler(CompileContext& cc, sema::Program* program);

  bool compile();
  bool emit(ISmxBuffer* buffer);

private:
  void generate(ast::FunctionStatement* fun);

  void generateStatement(ast::Statement* stmt);
  void generateBlock(ast::BlockStatement* block);
  void generateReturn(ast::ReturnStatement* stmt);
  void generateExprStatement(ast::ExpressionStatement* stmt);
  void generateWhile(ast::WhileStatement* stmt);
  void generateFor(ast::ForStatement* stmt);
  void generateIf(ast::IfStatement* stmt);
  void generateBreak(ast::BreakStatement* stmt);
  void generateContinue(ast::ContinueStatement* stmt);
  void generateSwitch(ast::SwitchStatement* stmt);

  // Allocate space and generate data for a local variable.
  void generateVarDecl(ast::VarDecl* decl);

  // Allocate space and generate data for a global variable.
  void generateData(ast::VarDecl* stmt);
  void generateLegacyStructData(ast::VarDecl* stmt);
  int32_t generateDataString(const char* str, size_t length);

  // Emit an expression into the given destination, returning false on failure.
  bool emit_into(sema::Expr* expr, ValueDest dest);

  // Low-level expression functions. These take in a destination and use it if
  // a specialized opcode is available - otherwise, they generally emit into
  // PRI. The actual value location is returned. Use emit_into to force a
  // final destination.
  ValueDest emit(sema::Expr* expr, ValueDest dest);
  ValueDest emitConstValue(sema::ConstValueExpr* expr, ValueDest dest);
  ValueDest emitBinary(sema::BinaryExpr* expr, ValueDest dest);
  ValueDest emitUnary(sema::UnaryExpr* expr, ValueDest dest);
  ValueDest emitCall(sema::CallExpr* expr, ValueDest dest);
  ValueDest emitImplicitCast(sema::ImplicitCastExpr* expr, ValueDest dest);
  ValueDest emitString(sema::StringExpr* expr, ValueDest dest);
  ValueDest emitIncDec(sema::IncDecExpr* expr, ValueDest dest);
  ValueDest emitLoad(sema::LoadExpr* expr, ValueDest dest);
  ValueDest emitStore(sema::StoreExpr* expr, ValueDest dest);
  ValueDest emitTernary(sema::TernaryExpr* expr, ValueDest dest);

  // l-value expressions always return the address of the l-value. For local
  // variables, this is their stack address. For pointer types (references,
  // dynamic arrays), it is the pointer. For fixed-length arrays, it is the
  // array address.
  //
  // The idea is that after calling one of these functions, storing to |*dest|
  // is equivalent to an assignment to the l-value.
  ValueDest emitVar(sema::VarExpr* expr, ValueDest dest);
  ValueDest emitIndex(sema::IndexExpr* expr, ValueDest dest);

private:
  void test(sema::Expr* expr, bool jumpOnTrue, Label* taken, Label* fallthrough);
  void test_logical(sema::BinaryExpr* expr, bool jumpOnTrue, Label* taken, Label* fallthrough);

  // Flatten a tree of BinaryExprs into a chain of the same operator.
  std::vector<sema::Expr*> flatten(sema::BinaryExpr* expr);

  // Load two expressions into PRI and ALT.
  bool load_both(sema::Expr* left, sema::Expr* right);

  // Store a constant value. Calls will_kill().
  void emit_const(ValueDest dest, cell_t value);

  // Helpers for l-values.
  ValueDest emit_var_load(sema::VarExpr* var, ValueDest dest);
  void emit_var_store(VariableSymbol* sym, ValueDest src);

  void initialize_array(VariableSymbol* sym, sema::Expr* expr, const ArrayInfo& info);
  void initialize_dynamic_array(ast::VarDecl* decl, sema::Expr* expr);

  struct ArrayBuilder {
    const ArrayInfo* info;
    int32_t base_delta;
    int32_t iv_cursor;
    int32_t data_cursor;
  };
  cell_t gen_array_iv(ArrayType* type, sema::Expr* expr, ArrayBuilder& b);
  cell_t gen_array_data(ArrayType* type, sema::Expr* expr, ArrayBuilder& b);

  // Called when ScopeInfo is destroyed.
  struct ScopeInfo;
  void leave_scope(ScopeInfo& scope_info);
  void jump_to_scope(ScopeInfo* scope_info);

private:
  // Signal that the given register is about to be clobbered.
  void will_kill(ValueDest dest);

  // Push the register onto the pseudo-stack. If the register is about to be
  // clobbered, then it will be popped by restore(). If nothing clobbers it,
  // then restore() will have no effect.
  //
  // The pseudo-stack is a true pseudo-stack, and not a spill space. That
  // means the compiler must take care not to cause any invalid save/restore
  // pairs, and we throw an error if such a situation arises. For example:
  //
  //   preserve(pri)
  //   preserve(alt)
  //   ... clobber alt ...
  //   ... clobber pri ...
  //   restore alt
  //   restore pri
  //
  // In this example, "alt" and "pri" are clobbered in reverse order, which
  // means they will be pushed in the opposite order they are popped.
  //
  // It may be that these situations are unavoidable... we'll probably have
  // to end up doing something clever (like flushing the pushes in-order,
  // or reserving spill space).
  uint64_t preserve(ValueDest dest);

  // Restore a register that was previously saved.
  void restore(uint64_t id);

  int32_t compute_storage_size(Type* type);

private:
  static int sort_functions(const void* a1, const void* a2);

  void add_code();
  void add_data();
  void add_natives();
  void add_publics();
  void add_pubvars();

private:
  CompileContext& cc_;
  sema::Program* program_;

  SmxAssemblyBuffer masm_;
  ByteBuffer data_;

  SmxBuilder builder_;
  RefPtr<SmxNameTable> names_;

  struct FunctionEntry {
    Atom* name;
    ast::FunctionStatement* fun;

    FunctionEntry()
      : name(nullptr),
        fun(nullptr)
    {}
    FunctionEntry(Atom* name, ast::FunctionStatement* fun)
      : name(name),
        fun(fun)
    {}
  };
  std::vector<FunctionEntry> publics_;
  std::vector<FunctionEntry> natives_;

  std::vector<SValue> operand_stack_;
  uint64_t pri_value_;
  uint64_t alt_value_;

  // How much space needs to be reserved in STK for all local variables.
  int32_t max_var_stk_;
  int32_t cur_var_stk_;
  DataLabel entry_stack_op_;

  // Heap usage is reset each statement. We track the max used.
  int32_t heap_usage_;
  int32_t max_heap_usage_;

  // Track where continue/break should go to, and how many scopes to destroy.
  struct LoopScope : public ke::StackLinked<LoopScope> {
    LoopScope(LoopScope** parent, ScopeInfo* scope)
     : StackLinked(parent),
       scope_info(scope),
       continue_to(nullptr),
       break_to(nullptr)
    {}
    // Current loop context. If we do nested functions, these have to be zapped
    // at function boundaries.
    ScopeInfo* scope_info;
    Label* continue_to;
    Label* break_to;
  };
  LoopScope* loop_;

  // Track how many heap-allocated variables are created in each scope.
  struct ScopeInfo : public ke::StackLinked<ScopeInfo> {
    explicit ScopeInfo(SmxCompiler* smx_cc, ScopeInfo** parent)
      : StackLinked(parent),
        smx_cc(*smx_cc),
        heap_vars(0)
    {}
    ~ScopeInfo() override {
      smx_cc.leave_scope(*this);
    }
    ScopeInfo* prev() const {
      return prev_;
    }
    SmxCompiler& smx_cc;
    size_t heap_vars;
  };
  ScopeInfo* scope_info_;

  uint32_t last_stmt_pc_;
};

} // namespace sp

#endif // _include_sourcepawn_emit_emitter_h_
