/* vim: set sts=4 ts=8 sw=4 tw=99 et: */
//  Pawn compiler
//
//  Copyright (c) ITB CompuPhase, 1997-2006
//  Copyright (c) 2013 AlliedModders LLC
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
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <filesystem>
#include <sstream>
#include <string>
#include <utility>

#include <amtl/am-platform.h>
#include <amtl/am-raii.h>
#include <amtl/am-string.h>
#include <amtl/am-unused.h>
#include <amtl/experimental/am-argparser.h>

#include "array-helpers.h"
#include "assembler.h"
#include "code-generator.h"
#include "compile-options.h"
#include "lexer.h"
#include "lexer-inl.h"
#include "parser.h"
#include "semantics.h"
#include "source-manager.h"
#include "symbols.h"
#include "types.h"

#if defined __linux__ || defined __FreeBSD__ || defined __OpenBSD__ || defined DARWIN
#    include <unistd.h>
#endif

#if defined _WIN32
#    include <io.h> // for access()
#    include <windows.h>
#endif

#include <time.h>

// Hack to build under SourceMod. This should be cleaned up at some point.
#undef SM_USE_VERSIONLIB
#include <sourcemod_version.h>

#ifdef __EMSCRIPTEN__
#    include <emscripten.h>
#endif

#include "builtin-generator.h"
#include "errors.h"
#include "expressions.h"
#include "lexer.h"
#include "sc.h"
#include "sci18n.h"
#include "sctracker.h"
#define VERSION_INT 0x0302

using namespace ke;

namespace sp {
namespace cc {

namespace fs = std::filesystem;

int pc_tag_string = 0;
int pc_tag_bool = 0;

static void setconfig(const char* root);
static void inst_datetime_defines(BuiltinGenerator& gen);
static void inst_binary_name(BuiltinGenerator& gen, const std::string& binfile);

enum {
    TEST_PLAIN,  /* no parentheses */
    TEST_PARENS, /* '(' <expr> ')' */
    TEST_OPT,    /* '(' <expr> ')' or <expr> */
};

args::ToggleOption opt_show_stats(nullptr, "--show-stats", Some(false),
                                  "Show compiler statistics on exit.");

#ifdef __EMSCRIPTEN__
EM_JS(void, setup_emscripten_fs, (), {
    if (ENVIRONMENT_IS_NODE) {
        FS.mkdir('/fakeroot');
        FS.mount(NODEFS, {root: '/'}, '/fakeroot');
        FS.chdir('/fakeroot/' + process.cwd());
    }
});
#endif

int RunCompiler(int argc, char** argv, CompileContext& cc) {
    ParseTree* tree = nullptr;
    bool ok = false;
    std::string ext;
    BuiltinGenerator gen(cc);

    cc.CreateGlobalScope();
    cc.InitLexer();

    auto options = cc.options();

#ifdef __EMSCRIPTEN__
    setup_emscripten_fs();
#endif

    if (!cc.errfname().empty())
        remove(cc.errfname().c_str()); /* delete file on startup */
    else if (options->verbosity > 0)
        setcaption();
    setconfig(argv[0]); /* the path to the include files */

    if (options->source_files.size() > 1) {
        report(452);
        goto cleanup;
    }

    {
        auto sf = cc.sources()->Open({}, options->source_files[0]);
        if (!sf) {
            report(417) << options->source_files[0];
            goto cleanup;
        }
        sf->set_is_main_file();
        cc.set_inpf_org(sf);
    }

    // Add command-line defines.
    for (const auto& pair : options->predefines)
        gen.AddDefine(pair.first, pair.second);
    cc.lexer()->AddFile(gen.Generate("<command-line>"));

    // Add builtin defines.
    inst_binary_name(gen, cc.outfname());
    inst_datetime_defines(gen);

    gen.AddBuiltinConstants();
    gen.AddDefaultInclude();

    cc.lexer()->AddFile(gen.Generate("<built-in>"));

    cc.lexer()->AddFile(cc.inpf_org());
    cc.lexer()->Init();

    {
        Semantics sema(cc);
        Parser parser(cc, &sema);

        AutoCountErrors errors;
        tree = parser.Parse();      /* process all input */
        if (!tree || !errors.ok())
            goto cleanup;

        errors.Reset();

        {
            SemaContext sc(&sema);
            sema.set_context(&sc);

            if (!tree->stmts()->EnterNames(sc) || !errors.ok())
                goto cleanup;

            errors.Reset();
            if (!tree->stmts()->Bind(sc) || !errors.ok())
                goto cleanup;

            sema.set_context(nullptr);

            errors.Reset();
            if (!sema.Analyze(tree) || !errors.ok())
                goto cleanup;

            tree->stmts()->ProcessUses(sc);
            ok = true;
        }
    }

cleanup:
    if (!ok && cc.reports()->NumErrorMessages() == 0)
        report(423);

    unsigned int errnum = cc.reports()->NumErrorMessages();
    unsigned int warnnum = cc.reports()->NumWarnMessages();
    bool compile_ok = (errnum == 0);

    cc.set_shutting_down();
    cc.reports()->DumpErrorReport(true);

    CodeGenerator cg(cc, tree);
    if (tree && compile_ok)
        compile_ok = cg.Generate();

    // Write the binary file.
    if (!options->syntax_only && compile_ok) {
        compile_ok &= assemble(cc, cg, cc.outfname().c_str(), options->compression);
    }

    errnum += cc.reports()->NumErrorMessages();
    cc.reports()->DumpErrorReport(true);

    if (compile_ok && options->show_includes) {
        for (const auto& file : cc.sources()->opened_files()) {
            if (file->is_main_file() || file->is_builtin())
                continue;
            fprintf(stdout, "Note: including file: %s\n", file->name());
        }
    }

    cc.set_inpf_org(nullptr);

    if (compile_ok && cc.errfname().empty()) {
        if (options->verbosity >= 1 && compile_ok) {
            printf("Code size:         %" PRIu32 " bytes\n", cg.code_size());
            printf("Data size:         %" PRIu32 " bytes\n", cg.data_size());
            printf("Stack/heap size:   %8ld bytes\n", (long)cg.DynamicMemorySize());
            printf("Total requirements:%8ld bytes\n", (long)cg.code_size() +
                                                             (long)cg.data_size() +
                                                             (long)cg.DynamicMemorySize());
        }
        if (opt_show_stats.value()) {
            size_t allocated, reserved, bookkeeping;
            cc.allocator().memoryUsage(&allocated, &reserved, &bookkeeping);

            printf("\n");
            printf(" -- Compiler memory usage --\n");
            printf("Malloc bytes:      %8" KE_FMT_SIZET " bytes\n", cc.malloc_bytes());
            printf("Malloc bytes peak: %8" KE_FMT_SIZET " bytes\n", cc.malloc_bytes_peak());
            printf("Pool allocation:   %8" KE_FMT_SIZET " bytes\n", allocated);
            printf("Pool unused:       %8" KE_FMT_SIZET " bytes\n", reserved - allocated);
            printf("Pool bookkeeping:  %8" KE_FMT_SIZET " bytes\n", bookkeeping);
            printf("Pool wasted:       %8" KE_FMT_SIZET " bytes\n", cc.allocator().wasted());
        }
    }

    int retcode;
    if (!compile_ok) {
        if (cc.errfname().empty())
            printf("\n%d Error%s.\n", errnum, (errnum > 1) ? "s" : "");
        retcode = 1;
    } else if (warnnum != 0) {
        if (cc.errfname().empty())
            printf("\n%d Warning%s.\n", warnnum, (warnnum > 1) ? "s" : "");
        retcode = 0; /* use "0", so that MAKE and similar tools continue */
    } else {
        retcode = 0;
        if (retcode == 0 && options->verbosity >= 2)
            printf("\nDone.\n");
    }
    return retcode;
}

static void inst_binary_name(BuiltinGenerator& gen, const std::string& binfile) {
    fs::path binfile_path(binfile);
    fs::path binfile_name = binfile_path.filename();

    gen.AddDefine("__BINARY_PATH__", StringizePath(binfile_path));
    gen.AddDefine("__BINARY_NAME__", StringizePath(binfile_name.string()));
}

static void inst_datetime_defines(BuiltinGenerator& gen) {
    char date[64];
    char ltime[64];
    time_t td;
    struct tm* curtime;

    time(&td);
    curtime = localtime(&td);

#if defined __EMSCRIPTEN__
    snprintf(date, sizeof(date), "\"%02d/%02d/%04d\"", curtime->tm_mon + 1, curtime->tm_mday,
             curtime->tm_year + 1900);
    snprintf(ltime, sizeof(ltime), "\"%02d:%02d:%02d\"", curtime->tm_hour, curtime->tm_min,
             curtime->tm_sec);
#else
    strftime(date, 31, "\"%m/%d/%Y\"", curtime);
    strftime(ltime, 31, "\"%H:%M:%S\"", curtime);
#endif

    gen.AddDefine("__DATE__", date);
    gen.AddDefine("__TIME__", ltime);
}

static fs::path GetProcessPath([[maybe_unused]] const char* root) {
#if defined KE_WINDOWS
    char path[MAX_PATH];
    GetModuleFileNameA(NULL, path, sizeof(path));
    return fs::path(path);
#elif defined __EMSCRIPTEN__
    char path[PATH_MAX];
    if (EM_ASM_INT(
            {
                if (ENVIRONMENT_IS_NODE) {
                    stringToUTF8('/fakeroot' + __filename, $0, $1);
                    return 1;
                }
                return 0;
            },
            path, sizeof(path)) == 0 &&
        root != NULL) {
        SafeStrcpy(path, sizeof(path), root);
    }
    return fs::path(path);
#else
    return fs::path(root);
#endif
}

#if defined __EMSCRIPTEN__
// Needed due to EM_ASM usage
__attribute__((noinline))
#endif
static void setconfig(const char* root) {
    fs::path proc_path = GetProcessPath(root);

    fs::path proc_dir = proc_path.parent_path();
    if (proc_dir.empty())
        return;

    auto& cc = CompileContext::get();

    fs::path include_dir = proc_dir / "include";
    if (!fs::is_directory(include_dir)) {
        // There is no "include" directory below the directory where the compiler
        // is found. This typically means that the compiler is in a "bin" sub-directory
        // and the "include" is below the *parent*. So find the parent...
        proc_dir = proc_dir.parent_path();
        if (proc_dir.empty())
            return;
        include_dir = proc_dir / "include";
    }

    if (fs::is_directory(include_dir))
        cc.options()->include_paths.emplace_back(include_dir.string());
}

void setcaption() {
    printf("SourcePawn Compiler %s\n", SM_VERSION_STRING);
    printf("Copyright (c) 1997-2006 ITB CompuPhase\n");
    printf("Copyright (c) 2004-2024 AlliedModders LLC\n\n");
}

} // namespace cc
} // namespace sp
