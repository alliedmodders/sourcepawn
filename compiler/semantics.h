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

#include <unordered_set>
#include <vector>

#include "compile-context.h"
#include "sc.h"
#include "parse-node.h"

class SemaContext
{
  public:
    SemaContext()
      : cc_(CompileContext::get()),
        func_(nullptr)
    {
        cc_prev_sc_ = cc_.sema();
        cc_.set_sema(this);
    }
    SemaContext(SemaContext& parent, symbol* func, FunctionInfo* func_node)
      : cc_(parent.cc_),
        scope_(parent.scope_),
        func_(func),
        func_node_(func_node)
    {
        cc_prev_sc_ = cc_.sema();
        cc_.set_sema(this);
    }

    ~SemaContext() {
        cc_.set_sema(cc_prev_sc_);
    }

    CompileContext& cc() { return cc_; }

    bool BindType(const token_pos_t& pos, TypenameInfo* ti);
    bool BindType(const token_pos_t& pos, typeinfo_t* ti);
    bool BindType(const token_pos_t& pos, sp::Atom* atom, bool is_label, int* tag);

    Stmt* void_return() const { return void_return_; }
    void set_void_return(Stmt* stmt) { void_return_ = stmt; }

    bool warned_mixed_returns() const { return warned_mixed_returns_; }
    void set_warned_mixed_returns() { warned_mixed_returns_ = true; }

    bool returns_value() const { return returns_value_; }
    void set_returns_value() { returns_value_ = true; }

    // Indicates that the current context only ever exits its control flow
    // through a return statement. For example,
    //
    //     for (;;) {
    //         if (a) return;
    //         if (b) break;
    //     }
    //
    // .. can exit through "break". If this line is omitted, "always_returns"
    // will be true. If the loop had a conditional, this would imply an implicit
    // break statement.
    //
    // This is different from Stmt::flow_type, which indicates whether the
    // statement unconditionally ends in a certain keyword.
    bool always_returns() const { return always_returns_; }
    void set_always_returns() { always_returns_ = true; }
    void set_always_returns(bool value) { always_returns_ = value; }

    bool& loop_has_break() { return loop_has_break_; }
    bool& loop_has_continue() { return loop_has_continue_; }
    bool& loop_has_return() { return loop_has_return_; }

    bool warned_unreachable() const { return warned_unreachable_; }
    void set_warned_unreachable() { warned_unreachable_ = true; }

    symbol* func() const { return func_; }
    FunctionInfo* func_node() const { return func_node_; }

    // Currently, this only refers to local/argument scopes, and not global
    // scope. They will be linked together when reparse goes away.
    SymbolScope* scope() const { return scope_; }
    void set_scope(SymbolScope* scope) { scope_ = scope; }

    std::unordered_set<SymbolScope*>& static_scopes() { return static_scopes_; }

  private:
    CompileContext& cc_;
    SymbolScope* scope_ = nullptr;
    symbol* func_ = nullptr;
    FunctionInfo* func_node_ = nullptr;
    Stmt* void_return_ = nullptr;
    bool warned_mixed_returns_ = false;
    bool returns_value_ = false;
    bool always_returns_ = false;
    bool loop_has_break_ = false;
    bool loop_has_continue_ = false;
    bool loop_has_return_ = false;
    bool warned_unreachable_ = false;
    SemaContext* cc_prev_sc_ = nullptr;
    std::unordered_set<SymbolScope*> static_scopes_;
};

class AutoEnterScope final
{
  public:
    // Create a new scope.
    AutoEnterScope(SemaContext& sc, ScopeKind kind);

    // Use existing scope.
    AutoEnterScope(SemaContext& sc, SymbolScope* scope);

    ~AutoEnterScope();

  private:
    SemaContext& sc_;
    SymbolScope* prev_;
};

class AutoCollectSemaFlow final
{
  public:
    AutoCollectSemaFlow(SemaContext& sc, ke::Maybe<bool>* out);
    ~AutoCollectSemaFlow();

  private:
    SemaContext& sc_;
    ke::Maybe<bool>* out_;
    bool old_value_;
};

void ReportFunctionReturnError(symbol* sym);
bool TestSymbols(SymbolScope* root, int testconst);
void check_void_decl(const typeinfo_t* type, int variable);
void check_void_decl(const declinfo_t* decl, int variable);
int check_operatortag(int opertok, int resulttag, const char* opername);
int argcompare(arginfo* a1, arginfo* a2);
void fill_arg_defvalue(VarDecl* decl, arginfo* arg);
bool IsLegacyEnumTag(SymbolScope* scope, int tag);
