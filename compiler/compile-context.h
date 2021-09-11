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

#include <memory>
#include <string>
#include <unordered_set>
#include <vector>

#include "lexer.h"

class ReportManager;
class SymbolScope;
struct CompileOptions;
struct symbol;

// The thread-safe successor to scvars.
class CompileContext final
{
  public:
    CompileContext();
    ~CompileContext();

    static CompileContext* sInstance;

    static inline CompileContext& get() { return *sInstance; }

    void CreateGlobalScope();
    void InitLexer();

    SymbolScope* globals() const { return globals_; }
    std::unordered_set<symbol*>& functions() { return functions_; }
    const std::shared_ptr<Lexer>& lexer() const { return lexer_; }
    ReportManager* reports() const { return reports_.get(); }
    CompileOptions* options() const { return options_.get(); }
    std::vector<std::string>& input_files() { return input_files_; }

    const std::string& default_include() const { return default_include_; }
    void set_default_include(const std::string& file) { default_include_ = file; }

    bool shutting_down() const { return shutting_down_; }
    void set_shutting_down() { shutting_down_ = true; }

    bool one_error_per_stmt() const { return one_error_per_stmt_; }
    void set_one_error_per_stmt(bool value) { one_error_per_stmt_ = value; }

    // No copy construction.
    CompileContext(const CompileContext&) = delete;
    CompileContext(CompileContext&&) = delete;
    void operator =(const CompileContext&) = delete;
    void operator =(CompileContext&&) = delete;

  private:
    SymbolScope* globals_;
    std::string default_include_;
    std::unordered_set<symbol*> functions_;
    std::unique_ptr<CompileOptions> options_;
    std::vector<std::string> input_files_;

    // The lexer is in CompileContext rather than Parser until we can eliminate
    // PreprocExpr().
    std::shared_ptr<Lexer> lexer_;

    // Error state.
    bool shutting_down_ = false;
    bool one_error_per_stmt_ = false;
    std::unique_ptr<ReportManager> reports_;
};
