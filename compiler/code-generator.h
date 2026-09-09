// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2021-2026 AlliedModders LLC
// Copyright (c) ITB CompuPhase, 1997-2006
//
#pragma once

#include <list>
#include <string>
#include <optional>
#include <queue>
#include <unordered_map>
#include <unordered_set>
#include <variant>
#include <vector>

#include <utils/bitset.h>
#include "data-queue.h"
#include "errors.h"
#include "ir-node.h"
#include "libsmx/data-pool.h"
#include "libsmx/smx-builder.h"
#include "libsmx/smx-encoding.h"
#include "parse-node.h"
#include "rtti-builder.h"
#include "utils/byte-buffer.h"
#include "utils/string-pool.h"
#include "smx-assembly-buffer.h"
#include "stl/stl-unordered-map.h"

namespace sp {
namespace cc {

class CompileContext;
class ParseTree;

class CodeGenerator final
{
  public:
    CodeGenerator(CompileContext& cc, ParseTree* tree);

    bool Generate();

    SmxBuilder& smx() { return smx_; }
    uint32_t code_size() const { return (uint32_t)asm_.size(); }
    uint32_t data_size() const { return data_.size(); }

    int DynamicMemorySize() const;

  private:
    void FinishSmx();

    // Statements/decls.
    void EmitStmtList(StmtList* list);
    void EmitStmt(Stmt* stmt);
    void EmitChangeScopeNode(ChangeScopeNode* node);
    void EmitVarDecl(VarDeclBase* decl);
    void EmitPstruct(VarDeclBase* decl);
    void EmitGlobalVar(VarDeclBase* decl);
    void EmitLocalVar(VarDeclBase* decl);
    void EmitIfStmt(IfStmt* stmt);
    void EmitDeleteStmt(DeleteStmt* stmt);
    void EmitDoWhileStmt(DoWhileStmt* stmt);
    void EmitForStmt(ForStmt* stmt);
    void EmitSwitchStmt(SwitchStmt* stmt);
    void EmitFunctionDecl(FunctionDecl* info);
    void EmitEnumStructDecl(EnumStructDecl* info);
    void EmitMethodmapDecl(MethodmapDecl* info);
    void EmitReturnStmt(ReturnStmt* stmt);
    void EmitReturnArrayStmt(ReturnStmt* stmt);
    void EmitGlobalInitStmt(GlobalInitStmt* stmt);

    void EmitArrayCtor(ArrayType* type, ir::Value* ctor, unsigned int flags);
    void EmitEnumStructCtor(EnumStructDecl* es, ir::Value* ctor);
    void EmitEnumStructCopy(QualType type, ir::Value* rhs);
    void EmitArrayFillStructs(ArrayType* type, ir::Array* expr);
    void EmitArrayFillArrays(ArrayType* type, ArrayType* inner, ir::Array* expr);
    void EmitArrayFillHeapItems(ArrayType* type, ir::Array* expr);
    void EmitArrayFillIntptr(ArrayType* type, ir::Array* expr);
    uint32_t EmitArrayFillData(ArrayType* type, ir::Array* array);
    uint32_t EmitStringFillData(ArrayType* type, ir::String* array);

    // Expressions.
    enum EmitFlags {
        EMIT_DEFAULT = 0,
        EMIT_DISCARD_RESULT = (1 << 0),
        EMIT_ALLOW_LVALUE = (1 << 1),
        EMIT_REPEATABLE = (1 << 2),
    };

    void EmitExpr(ir::Value* expr, unsigned int flags = EMIT_DEFAULT);
    void EmitConstantExpr(ir::Constant* expr);
    void EmitTest(ir::Value* expr, bool jump_on_true, sp::Label* target);
    void EmitUnary(ir::Unary* expr);
    void EmitIncDec(ir::IncDec* expr, unsigned int flags);
    void EmitBinary(ir::Binary* expr, unsigned int flags);
    void EmitBinaryTail(int oper_tok, ir::Value* left, ir::Value* right);
    void EmitLogicalExpr(ir::Logical* expr);
    void EmitChainedCompareExpr(ir::ChainedCompare* expr);
    void EmitTernaryExpr(ir::Ternary* expr, unsigned int flags);
    void EmitStringExpr(ir::String* expr);
    void EmitIndexExpr(ir::Index* expr);
    void EmitSliceExpr(ir::Slice* expr);
    bool IsElidableSlice(ir::Value* expr, FunctionDecl* fun, QualType arg);
    void EmitElidedSliceExpr(ir::Slice* expr);
    void EmitCallExpr(ir::Call* expr, unsigned int flags);
    void EmitDefaultArgExpr(ir::DefaultArg* expr);
    void EmitNewArrayExpr(ir::NewArray* expr);
    void EmitSimpleCastExpr(ir::SimpleCast* expr);
    void EmitCastExpr(ir::Value* expr, ir::Value* from, unsigned int flags);
    void EmitRvalue(ir::Rvalue* expr);
    void EmitRvalueFromLvalue(ir::Lvalue* expr);
    void EmitAsRvalue(ir::Value* expr);
    void EmitCommaExpr(ir::Comma* expr, unsigned int flags);
    void EmitArrayExpr(ir::Array* expr, unsigned int flags);
    void EmitSizeofExpr(ir::Sizeof* expr);
    void EmitFunctionExpr(ir::Function* expr);
    void EmitNewClosure(FunctionDecl* fun);
    void EmitArraySize(ir::ArraySize* expr);

    // Logical test helpers.
    bool EmitBinaryTest(ir::Binary* expr, bool jump_on_true, sp::Label* target);
    bool EmitUnaryTest(ir::Unary* expr, bool jump_on_true, sp::Label* target);
    void EmitLogicalTest(ir::Logical* expr, bool jump_on_true, sp::Label* target);

    struct BoundLval {
        ir::Lvalue* lval = nullptr;
        bool address_on_stack = false;

        // Returns true if binding is idempotent and no operand needs to be
        // pushed onto the stack.
        bool canRematerialize() const {
            switch (lval->kind()) {
                case IrKind::Variable:
                case IrKind::Upvar:
                    return true;
                default:
                    return false;
            }
        }
    };

    void EmitCall(const CallTarget& target, cell nargs, bool is_spread = false);
    void InvokeGetter(ir::Value* node, PropertyDecl* method);
    void EmitRvalue(ir::Value* node, const BoundLval& binding);
    void EmitLoadVar(VarDeclBase* var);
    void EmitStoreVar(VarDeclBase* var);
    void EmitIndirectLoad(Type* type);
    void EmitIndirectStore(Type* type);
    void EmitStore(ir::Value* node, const BoundLval& binding);
    void EmitAddress(const BoundLval& binding);
    void EmitBinaryOp(BuiltinType type, int oper_tok);
    void EmitAddress(VarDeclBase* decl);

    void EmitInit(VarDeclBase* decl, ir::Value* ctor);

    void EmitLoadField(LayoutFieldDecl* field);
    void EmitLoadFieldOffset(LayoutFieldDecl* field);
    void EmitStoreField(LayoutFieldDecl* field);
    void EmitAddrField(LayoutFieldDecl* field);

    // Builtins.
    void EmitFloatBuiltin(ir::Call* expr);

    using DebugSymbol = std::pair<Decl*, uint32_t>;
    void AddDebugFile(const std::string& line);
    void AddDebugLine(const token_pos_t& pos);
    void AddDebugSymbol(Decl* sym, uint32_t pc);
    void AddDebugSymbols(tr::vector<DebugSymbol>* list);
    void AddFunctionToQueue(FunctionDecl* decl);
    void EnqueueDebugSymbol(Decl* decl, uint32_t pc);
    uint32_t AddNativeEntry(FunctionDecl* decl);
    smx_rtti_debug_method AddFunctionEntry(FunctionDecl* decl, uint32_t pcode_offset);

    void EmitLoopControl(int token);

    // Emit any precursor instructions needed to load or store from an l-value.
    //
    // SymbolExpr:
    //   iVARIABLE: nothing is pushed.
    //
    // IndexExpr, base[index]:
    //   iARRAYELEM: &base[index] is pushed, and loaded if the inner type is not
    //               a value type (inner arrays are not considered value types).
    //
    // FieldAccessExpr: base.field
    //   iACCESSOR: |base| is pushed.
    //
    // If |simple_address| is true, then iARRAYELEM is converted to an iADDRESS.
    // This is useful if the caller does not want to deal with complex stack
    // operations. Note that simple_address is ONLY intended to collapse two
    // stack values into one. It is not intended to compute an address
    // unconditionally.
    BoundLval BindLval(ir::Lvalue* expr, bool simple_address = false);
    static int StackSlotsForLval(const BoundLval& binding);

  private:
    enum MemuseType {
        MEMUSE_STATIC = 0,
        MEMUSE_DYNAMIC = 1
    };

    struct MemoryUse {
        MemoryUse(MemuseType type, int size)
         : type(type),
           size(size)
        {}
        MemuseType type;
        int size; /* size of array for static (0 for dynamic) */
    };

    struct MemoryScope {
        MemoryScope(MemoryScope&& other)
         : scope_id(other.scope_id),
           needs_restore(other.needs_restore)
        {}
        explicit MemoryScope(int scope_id)
         : scope_id(scope_id),
           needs_restore(false)
        {}
        MemoryScope(const MemoryScope& other) = delete;

        MemoryScope& operator =(const MemoryScope& other) = delete;
        MemoryScope& operator =(MemoryScope&& other) {
            scope_id = other.scope_id;
            needs_restore = other.needs_restore;
            return *this;
        }

        int scope_id;
        bool needs_restore;
    };

    void EnterMemoryScope(tr::vector<MemoryScope>& frame);
    int PopScope(tr::vector<MemoryScope>& scope_list);

    using CallGraph = tr::unordered_map<FunctionDecl*, tr::vector<FunctionDecl*>>;

    cell_t AcquireTempSlot(ir::Value* node, Type* type);
    cell_t AcquireTempSlot(ir::Value* node, BuiltinType type);

    uint16_t AcquireGlobalSlot(VarDeclBase* decl);

  private:
    typedef tr::vector<tr::vector<DebugSymbol>> SymbolStack;

    class AutoEnterScope {
      public:
        explicit AutoEnterScope(CodeGenerator* cg, SymbolStack* scopes);
        ~AutoEnterScope();

      private:
        CodeGenerator* cg_;
        SymbolStack* scopes_;
    };
    friend class AutoEnterScope;

  private:
    typedef SmxBlobSection<sp_file_data_t> SmxDataSection;
    typedef SmxBlobSection<sp_file_code_t> SmxCodeSection;

  private:
    CompileContext& cc_;
    ParseTree* tree_;
    FunctionDecl* fun_ = nullptr;

    SmxAssemblyBuffer asm_;
    DataQueue data_;

    // SMX layout.
    SmxBuilder smx_;
    RefPtr<SmxNameTable> names_;
    RefPtr<SmxDataSection> smx_data_;
    RefPtr<SmxCodeSection> code_;
    std::unique_ptr<RttiBuilder> rtti_;
    std::queue<FunctionDecl*> fun_queue_;

    smx_rtti_debug_method debug_info_;
    SymbolStack local_syms_;
    tr::vector<DebugSymbol> global_syms_;
    tr::vector<std::pair<SymbolScope*, tr::vector<DebugSymbol>>> static_syms_;
    tr::unordered_set<SymbolScope*> static_scopes_;
    std::list<std::pair<uint32_t, Type*>> free_temp_slots_;
    std::list<std::pair<uint32_t, Type*>> used_temp_slots_;

    // Data queue cache.
    std::unordered_map<ir::Value*, uint32_t> fill_data_cache_;

    // Loop handling.
    struct LoopContext {
        sp::Label break_to;
        sp::Label continue_to;
        int stack_scope_id;
    };
    LoopContext* loop_ = nullptr;

    cell_t max_array_size_ = 0;

    LocalSlotSignature locals_;

    AutoCountErrors errors_;

    std::unordered_map<sp::Atom*, void(CodeGenerator::*)(ir::Call*)> builtins_;
};

} // namespace cc
} // namespace sp
