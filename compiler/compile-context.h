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

#include "array-data.h"
#include "pool-allocator.h"
#include "shared/string-pool.h"
#include "source-file.h"
#include "source-manager.h"
#include "stl/stl-forward-list.h"
#include "stl/stl-unordered-map.h"
#include "stl/stl-unordered-set.h"
#include "stl/stl-vector.h"

namespace sp {
namespace cc {

class Decl;
class FunctionDecl;
class Lexer;
class ReportManager;
class SemaContext;
class SymbolScope;
class TypeManager;
struct CompileOptions;

// The thread-safe successor to scvars.
class CompileContext final
{
  public:
    CompileContext();
    ~CompileContext();

    static CompileContext* sInstance;

    static inline CompileContext& get() { return *sInstance; }
    static inline CompileContext* maybe_get() { return sInstance; }

    void CreateGlobalScope();
    void InitLexer();

    void TrackMalloc(size_t bytes);
    void TrackFree(size_t bytes);

    SymbolScope* globals() const { return globals_; }
    tr::unordered_set<FunctionDecl*>& functions() { return functions_; }
    tr::unordered_set<FunctionDecl*>& publics() { return publics_; }
    const std::shared_ptr<Lexer>& lexer() const { return lexer_; }
    ReportManager* reports() const { return reports_.get(); }
    CompileOptions* options() const { return options_.get(); }
    SourceManager* sources() const { return sources_.get(); }
    TypeManager* types() const { return types_.get(); }
    StringPool* atoms() { return &atoms_; }

    Atom* atom(const std::string& str) {
        return atoms_.add(str);
    }
    Atom* atom(const char* str, size_t length) {
        return atoms_.add(str, length);
    }
    Atom* atom(const char* str) {
        return atoms_.add(str);
    }

    const std::string& default_include() const { return default_include_; }
    void set_default_include(const std::string& file) { default_include_ = file; }

    bool shutting_down() const { return shutting_down_; }
    void set_shutting_down() { shutting_down_ = true; }

    bool one_error_per_stmt() const { return one_error_per_stmt_; }
    void set_one_error_per_stmt(bool value) { one_error_per_stmt_ = value; }

    bool verify_output() const { return verify_output_; }
    void set_verify_output(bool verify_output) { verify_output_ = verify_output; }

    // Kludge until we can get rid of markusage().
    void set_sema(SemaContext* sc) { sc_ = sc; }
    SemaContext* sema() const { return sc_; }

    const std::string& errfname() const { return errfname_; }
    void set_errfname(const std::string& value) { errfname_ = value; }

    std::string& outfname() { return outfname_; }
    void set_outfname(const std::string& value) { outfname_ = value; }

    std::shared_ptr<SourceFile> inpf_org() const { return inpf_org_; }
    void set_inpf_org(std::shared_ptr<SourceFile> sf) { inpf_org_ = sf; }

    bool must_abort() const { return must_abort_; }
    void set_must_abort() { must_abort_ = true; }

    size_t malloc_bytes() const { return malloc_bytes_; }
    size_t malloc_bytes_peak() const { return malloc_bytes_peak_; }

    bool& in_preprocessor() { return in_preprocessor_; }
    bool& detected_illegal_preproc_symbols() { return detected_illegal_preproc_symbols_; }

    cc::PoolAllocator& allocator() { return allocator_; }

    // No copy construction.
    CompileContext(const CompileContext&) = delete;
    CompileContext(CompileContext&&) = delete;
    void operator =(const CompileContext&) = delete;
    void operator =(CompileContext&&) = delete;

    DefaultArrayData* NewDefaultArrayData();
    tr::vector<tr::string>* NewDebugStringList();
    tr::unordered_map<Atom*, Decl*>* NewSymbolMap();

  private:
    cc::PoolAllocator allocator_;
    SymbolScope* globals_;
    std::string default_include_;
    tr::unordered_set<FunctionDecl*> functions_;
    tr::unordered_set<FunctionDecl*> publics_;
    std::unique_ptr<CompileOptions> options_;
    std::string outfname_;
    std::string errfname_;
    std::unique_ptr<SourceManager> sources_;
    std::shared_ptr<SourceFile> inpf_org_;
    std::unique_ptr<TypeManager> types_;
    StringPool atoms_;

    // The lexer is in CompileContext rather than Parser until we can eliminate
    // PreprocExpr().
    std::shared_ptr<Lexer> lexer_;

    // Error state.
    bool shutting_down_ = false;
    bool one_error_per_stmt_ = false;
    std::unique_ptr<ReportManager> reports_;

    // Skip the verify step.
    bool verify_output_ = true;

    // Indicates that compilation must abort immediately.
    bool must_abort_ = false;

    SemaContext* sc_ = nullptr;

    // AST attachments.
    tr::forward_list<DefaultArrayData> default_array_data_objects_;
    tr::forward_list<tr::vector<tr::string>> debug_strings_;
    tr::forward_list<tr::unordered_map<Atom*, Decl*>> symbol_maps_;

    size_t malloc_bytes_ = 0;
    size_t malloc_bytes_peak_ = 0;

    bool in_preprocessor_ = false;
    bool detected_illegal_preproc_symbols_ = false;
};

} // namespace cc
} // namespace sp
