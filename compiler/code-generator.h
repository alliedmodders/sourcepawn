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
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "stl/stl-unordered-map.h"
#include "data-queue.h"
#include "errors.h"
#include "ir.h"
#include "parse-node.h"
#include "smx-assembly-buffer.h"

namespace sp {
namespace cc {

class CompileContext;
class ParseTree;

class CodeGenerator final
{
  public:
    CodeGenerator(CompileContext& cc, std::shared_ptr<ir::Module> mod);

    bool Generate();

    void LinkPublicFunction(ir::Function* fun, uint32_t id);

    std::shared_ptr<ir::Module> mod() const { return mod_; }
    const tr::vector<tr::string>& debug_strings() const { return debug_strings_; }
    const tr::vector<ir::Function*>& native_list() const { return native_list_; }

    const uint8_t* code_ptr() const { return asm_.bytes(); }
    uint32_t code_size() const { return (uint32_t)asm_.size(); }
    const uint8_t* data_ptr() const { return data_.dat(); }
    uint32_t data_size() const { return data_.size(); }

    int DynamicMemorySize() const;

  private:
    // Statements/decls.
    void EmitFunction(ir::Function* fun);
    void EmitBlock(ir::InsnBlock* block);
    void EmitInsn(ir::Insn* node);
    void EmitValueInsn(ir::ValueInsn* insn);
    void EmitDoWhile(ir::DoWhile* loop);
    void EmitIf(ir::If* insn);
    void EmitSwitch(ir::Switch* insn);
    void EmitStmtList(StmtList* list);
    void EmitStmt(Stmt* stmt);
    void EmitChangeScopeNode(ChangeScopeNode* node);
    void EmitVarDecl(ir::Variable* var);
    void EmitPstruct(VarDeclBase* decl);
    void EmitGlobalVar(ir::Variable* var);
    void EmitLocalVar(ir::Variable* var);
    void EmitIfStmt(IfStmt* stmt);
    void EmitDelete(ir::Delete* stmt);
    void EmitDoWhileStmt(DoWhileStmt* stmt);
    void EmitForLoop(ir::ForLoop* loop);
    void EmitSwitchStmt(SwitchStmt* stmt);
    void EmitFunctionDecl(FunctionDecl* info);
    void EmitEnumStructDecl(EnumStructDecl* info);
    void EmitMethodmapDecl(MethodmapDecl* info);
    void EmitReturn(ir::Return* node);
    void EmitReturnArray(ir::Return* node);

    // Expressions.
    void EmitExpr(Expr* expr);
    void EmitValue(ir::Value* val);
    void EmitLoad(ir::Load* load);
    void EmitLoadVariable(ir::VariableRef* ref);
    void EmitStore(ir::Store* op);
    void EmitStoreWithTemp(ir::StoreWithTemp* op);
    void EmitConst(ir::Const* cv);
    void EmitCommaOp(ir::CommaOp* op);
    void EmitCallOp(ir::CallOp* call);
    void EmitAddressOf(ir::AddressOf* op);
    void EmitTest(Expr* expr, bool jump_on_true, sp::Label* target);
    void EmitTest(ir::Value* test, bool jump_on_true, sp::Label* target);
    void EmitUnary(ir::UnaryOp* op);
    void EmitBinary(ir::BinaryOp* op);
    void EmitTernaryOp(ir::TernaryOp* op);
    void EmitCharArrayLiteral(ir::CharArrayLiteral* val);
    void EmitFunctionRef(ir::FunctionRef* ref);
    void EmitBinary(BinaryExpr* expr);
    void EmitBinaryInner(int oper_tok, const UserOperation& in_user_op, Expr* left, Expr* right);
    void EmitLogicalExpr(LogicalExpr* expr);
    void EmitChainedCompareExpr(ChainedCompareExpr* expr);
    void EmitSymbolExpr(SymbolExpr* expr);
    void EmitIndexExpr(IndexExpr* expr);
    void EmitAddressOfIndexOp(ir::IndexOp* op);
    void EmitAddressOfFieldRef(ir::FieldRef* op);
    void EmitFieldAccessExpr(FieldAccessExpr* expr);
    void EmitCallExpr(CallExpr* expr);
    void EmitDefaultArgExpr(DefaultArgExpr* expr);
    void EmitCallUserOp(ir::CallUserOp* op);
    void EmitNewArrayExpr(NewArrayExpr* expr);
    void EmitIncDec(ir::IncDec* incdec);
    void EmitArrayInitializer(ir::ArrayInitializer* array);

    // Helper for pushing arguments.
    void EmitArg(ir::Value* val);

    // Calculate the address of an lvalue.
    //
    // A true return indicates the lvalue can only be computed once, for example
    // the base or index are not idempotent, and the address is stored in PRI. A
    // false value means no address was needed.
    bool EmitLoadStorePrologue(ir::Lvalue* lval);

    // Emit a store. The value must be in PRI. If EmitLoadStorePrologue
    // returned true, the address must be in ALT.
    //
    // If save_rval is true, it will be preserved against clobbers.
    void EmitStoreEpilogue(ir::Lvalue* lval, bool save_rval); 

    // Emit a load. If EmitLoadStorePrologue returned true, the address must be
    // in PRI.
    void EmitLoadEpilogue(ir::Lvalue* lval);

    // Logical test helpers.
    bool EmitUnaryExprTest(UnaryExpr* expr, bool jump_on_true, sp::Label* target);
    bool EmitUnaryExprTest(ir::UnaryOp* op, bool jump_on_true, sp::Label* target);
    void EmitLogicalExprTest(LogicalExpr* expr, bool jump_on_true, sp::Label* target);
    void EmitLogicalExprTest(ir::BinaryOp* op, bool jump_on_true, sp::Label* target);
    bool EmitChainedCompareExprTest(ChainedCompareExpr* expr, bool jump_on_true,
                                    sp::Label* target);

    void EmitDefaultArray(Expr* expr, ArgDecl* arg);
    void EmitUserOp(const UserOperation& user_op, value* lval);
    void EmitCall(ir::Function* fun, cell nargs);
    void EmitInc(const value* lval);
    void EmitDec(const value* lval);
    void InvokeGetter(MethodmapPropertyDecl* method);
    void InvokeSetter(MethodmapPropertyDecl* method, bool save);
    void EmitRvalue(value* lval);
    void EmitBreak();
    void EmitTempValueRef(ir::TempValueRef* ref);

    void EmitRvalue(const value& lval) {
        value tmp = lval;
        EmitRvalue(&tmp);
    }

    using DebugSymbol = std::pair<Decl*, uint32_t>;

    void AddDebugFile(const std::string& line);
    void AddDebugLine(int linenr);
    void AddDebugSymbol(Decl* sym, uint32_t pc);
    void AddDebugSymbols(tr::vector<DebugSymbol>* list);
    void EnqueueDebugSymbol(Decl* decl, uint32_t pc);

    // Helper that automatically handles heap deallocations.
    void EmitExprForStmt(Expr* expr);
    void EmitLoopControl(int token);

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
           usage(std::move(other.usage)),
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
            usage = std::move(other.usage);
            needs_restore = other.needs_restore;
            return *this;
        }

        int scope_id;
        std::vector<MemoryUse> usage;
        bool needs_restore;
    };

    // Heap functions
    void EnterHeapScope(FlowType flow_type);
    void LeaveHeapScope();
    void TrackTempHeapAlloc(Expr* source, int size);
    void TrackHeapAlloc(ParseNode* node, MemuseType type, int size);

    // Stack functions
    void pushstacklist();
    void popstacklist(bool codegen);
    cell_t markstack(ParseNode* node, MemuseType type, int size);
    void modheap_for_scope(const MemoryScope& scope);
    void modstk_for_scope(const MemoryScope& scope);

    // Generates code to free mem usage, but does not pop the list.  
    //  This is used for code like dobreak()/docont()/doreturn().
    // stop_id is the list at which to stop generating.
    void genstackfree(int stop_id);

    int stack_scope_id() { return stack_scopes_.back().scope_id; }
    int heap_scope_id();
    bool has_stack_or_heap_scopes() {
        return !stack_scopes_.empty() || !heap_scopes_.empty();
    }

    void EnterMemoryScope(tr::vector<MemoryScope>& frame);
    void AllocInScope(ParseNode* node, MemoryScope& scope, MemuseType type, int size);
    int PopScope(tr::vector<MemoryScope>& scope_list);

    using CallGraph = tr::unordered_map<ir::Function*, tr::vector<ir::Function*>>;

    bool ComputeStackUsage();
    bool ComputeStackUsage(CallGraph::iterator caller_iter);

    cell_t GetSlotAddr(uint32_t slot);

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
    CompileContext& cc_;
    ParseTree* tree_;
    std::shared_ptr<ir::Module> mod_;
    ir::Function* fun_ = nullptr;
    int max_script_memory_ = 0;

    tr::vector<tr::string> debug_strings_;
    tr::vector<ir::Function*> native_list_;
    SmxAssemblyBuffer asm_;
    DataQueue data_;

    ke::Maybe<uint32_t> last_break_op_;
    tr::vector<MemoryScope> stack_scopes_;
    tr::vector<MemoryScope> heap_scopes_;
    SymbolStack local_syms_;
    tr::vector<DebugSymbol> global_syms_;
    tr::vector<std::pair<SymbolScope*, tr::vector<DebugSymbol>>> static_syms_;
    tr::unordered_set<SymbolScope*> static_scopes_;

    // Loop handling.
    struct LoopContext {
        sp::Label break_to;
        sp::Label continue_to;
        int stack_scope_id;
        int heap_scope_id;
    };
    LoopContext* loop_ = nullptr;

    int current_stack_ = 0;
    int current_memory_ = 0;
    int max_func_memory_ = 0;
    int temp_slots_base_ = 0;
    CallGraph callgraph_;

    AutoCountErrors errors_;
};

} // namespace cc
} // namespace sp
