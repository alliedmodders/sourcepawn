/* vim: set sts=4 ts=8 sw=4 tw=99 et: */
/*  Pawn compiler
 *
 *  Function and variable definition and declaration, statement parser.
 *
 *  Copyright (c) ITB CompuPhase, 1997-2006
 *
 *  This software is provided "as-is", without any express or implied warranty.
 *  In no event will the authors be held liable for any damages arising from
 *  the use of this software.
 *
 *  Permission is granted to anyone to use this software for any purpose,
 *  including commercial applications, and to alter it and redistribute it
 *  freely, subject to the following restrictions:
 *
 *  1.  The origin of this software must not be misrepresented; you must not
 *      claim that you wrote the original software. If you use this software in
 *      a product, an acknowledgment in the product documentation would be
 *      appreciated but is not required.
 *  2.  Altered source versions must be plainly marked as such, and must not be
 *      misrepresented as being the original software.
 *  3.  This notice may not be removed or altered from any source distribution.
 *
 *  Version: $Id$
 */
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
#include "lexer-inl.h"
#include "parser.h"
#include "semantics.h"
#include "source-manager.h"
#include "symbols.h"
#include "types.h"

#if defined _WIN32
#    include <conio.h>
#    include <io.h>
#    define strcasecmp stricmp
#endif

#if defined __linux__ || defined __FreeBSD__ || defined __OpenBSD__ || defined DARWIN
#    include <unistd.h>
#endif

#if defined _MSC_VER && defined _WIN32
#    include <direct.h> /* for _chdrive() */
#    define dos_setdrive(i) _chdrive(i)
#endif
#if defined __WIN32__ || defined _WIN32 || defined _Windows
#    include <windows.h>
#endif

#include <time.h>
#if defined(SOURCEMOD_BUILD)
#    include <sourcemod_version.h>
#    define SOURCEPAWN_VERSION SOURCEMOD_VERSION
#endif

#ifdef __EMSCRIPTEN__
#    include <emscripten.h>
#endif

#include "errors.h"
#include "expressions.h"
#include "lexer.h"
#include "sc.h"
#include "sci18n.h"
#include "sctracker.h"
#include "scvars.h"
#define VERSION_INT 0x0302

using namespace ke;

int pc_tag_string = 0;
int pc_tag_bool = 0;

sp::StringPool gAtoms;

static void initglobals(void);
static std::string get_extension(const std::string& filename);
static void set_extension(std::string* filename, const char* extension, bool force);
static void setconfig(char* root);
static void setcaption(void);
static void setconstants(void);
static void inst_datetime_defines(CompileContext& cc);
static void inst_binary_name(CompileContext& cc, std::string binfile);
static void parseoptions(CompileContext& cc, int argc, char** argv);

enum {
    TEST_PLAIN,  /* no parentheses */
    TEST_PARENS, /* '(' <expr> ')' */
    TEST_OPT,    /* '(' <expr> ')' or <expr> */
};
static int verbosity = 1;             /* verbosity level, 0=quiet, 1=normal, 2=verbose */
#if defined __WIN32__ || defined _WIN32 || defined _Windows
static HWND hwndFinish = 0;
#endif
static int sc_syntax_only = FALSE;

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

args::StringOption opt_active_dir("-D", "--active-dir", {}, "Active directory path");
args::StringOption opt_error_file("-e", "--error-file", {}, "Error file path");
#if defined __WIN32__ || defined _WIN32 || defined _Windows
args::StringOption opt_hwnd("-H", "--hwnd", {},
                            "Window handle to send a notification message on finish");
#endif
args::ToggleOption opt_warnings_as_errors("-E", "--warnings-as-errors", Some(false),
                                          "Treat warnings as errors");
args::ToggleOption opt_showincludes("-h", "--show-includes", Some(false),
                                    "Show included file paths");
args::IntOption opt_compression("-z", "--compress-level", Some(9),
                                "Compression level, default 9 (0=none, 1=worst, 9=best)");
args::IntOption opt_tabsize("-t", "--tabsize", Some(8),
                            "TAB indent size (in character positions, default=8)");
args::StringOption opt_verbosity("-v", "--verbose", {},
                                 "Verbosity level; 0=quiet, 1=normal, 2=verbose");
args::StringOption opt_prefixfile("-p", "--prefix", {}, "Set name of \"prefix\" file");
args::StringOption opt_outputfile("-o", "--output", {},
                                  "Set base name of (P-code) output file");
args::IntOption opt_optlevel("-O", "--opt-level", Some(2), "Deprecated; has no effect");
args::RepeatOption<std::string> opt_includes("-i", "--include", "Path for include files");
args::RepeatOption<std::string> opt_warnings("-w", "--warning",
                                         "Disable a specific warning by its number.");
args::ToggleOption opt_semicolons("-;", "--require-semicolons", Some(false),
                                  "Require a semicolon to end each statement.");
args::ToggleOption opt_syntax_only(nullptr, "--syntax-only", Some(false),
                              "Perform a dry-run (No file output) on the input");
args::ToggleOption opt_stderr(nullptr, "--use-stderr", Some(false),
                              "Use stderr instead of stdout for error messages.");
args::ToggleOption opt_no_verify(nullptr, "--no-verify", Some(false),
                                 "Disable opcode verification (for debugging).");

/*  "main" of the compiler
 */
int
pc_compile(int argc, char* argv[]) {
    int retcode;
    ParseTree* tree = nullptr;
    std::string ext;

    CompileContext cc;
    cc.CreateGlobalScope();
    cc.InitLexer();

    auto options = cc.options();

#ifdef __EMSCRIPTEN__
    setup_emscripten_fs();
#endif

    /* set global variables to their initial value */
    initglobals();

    parseoptions(cc, argc, argv);

    cc.set_binfname(cc.outfname());
    ext = get_extension(cc.binfname());
    if (strcasecmp(ext.c_str(), ".asm") == 0)
        set_extension(&cc.binfname(), ".smx", true);
    else
        set_extension(&cc.binfname(), ".smx", false);
    /* set output names that depend on the input name */
    set_extension(&cc.outfname(), ".asm", true);

    if (!cc.errfname().empty())
        remove(cc.errfname().c_str()); /* delete file on startup */
    else if (verbosity > 0)
        setcaption();
    setconfig(argv[0]); /* the path to the include files */

    assert(options->source_files.size() == 1);
    {
        auto sf = cc.sources()->Open(options->source_files[0], {});
        if (!sf) {
            report(417) << options->source_files[0];
            goto cleanup;
        }
        sf->set_is_main_file();
        cc.set_inpf_org(sf);
    }

    cc.lexer()->Init(cc.inpf_org());

    setconstants(); /* set predefined constants and tagnames */

    inst_datetime_defines(cc);
    inst_binary_name(cc, cc.binfname().c_str());

    {
        Parser parser(cc);

        AutoCountErrors errors;
        tree = parser.Parse();      /* process all input */
        if (!tree || !errors.ok())
            goto cleanup;

        errors.Reset();

        Semantics sema(cc, tree);
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
            if (!sema.Analyze() || !errors.ok())
                goto cleanup;

            tree->stmts()->ProcessUses(sc);
        }
    }

cleanup:
    unsigned int errnum = cc.reports()->NumErrorMessages();
    unsigned int warnnum = cc.reports()->NumWarnMessages();
    bool compile_ok = (errnum == 0);

    cc.set_shutting_down();
    cc.reports()->DumpErrorReport(true);

    CodeGenerator cg(cc, tree);
    if (tree && compile_ok)
        cg.Generate();

    // Write the binary file.
    if (!sc_syntax_only && compile_ok) {
        compile_ok &= assemble(cc, cg, cc.binfname().c_str(), opt_compression.value());
    }

    errnum += cc.reports()->NumErrorMessages();
    cc.reports()->DumpErrorReport(true);

    if (compile_ok && opt_showincludes.value()) {
        for (const auto& file : cc.sources()->opened_files()) {
            if (file->is_main_file())
                continue;
            fprintf(stdout, "Note: including file: %s\n", file->name());
        }
    }

    cc.set_inpf_org(nullptr);

    if (compile_ok && cc.errfname().empty()) {
        if (verbosity >= 1 && compile_ok) {
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

    gTypes.clear();
    funcenums_free();
    methodmaps_free();
    pstructs_free();
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
        if (retcode == 0 && verbosity >= 2)
            printf("\nDone.\n");
    }
#if defined __WIN32__ || defined _WIN32 || defined _Windows
    if (IsWindow(hwndFinish))
        PostMessageA(hwndFinish, RegisterWindowMessageA("PawnNotify"), retcode, 0L);
#endif
    return retcode;
}

static void
inst_binary_name(CompileContext& cc, std::string binfile)
{
    auto sepIndex = binfile.find_last_of(DIRSEP_CHAR);

    std::string binfileName = sepIndex == std::string::npos ? binfile : binfile.substr(sepIndex + 1);

    if (DIRSEP_CHAR == '\\') {
        auto pos = binfile.find('\\');
        while (pos != std::string::npos) {
            binfile.insert(pos + 1, 1, '\\');
            pos = binfile.find('\\', pos + 2);
        }
    }

    binfile.insert(binfile.begin(), '"');
    binfileName.insert(binfileName.begin(), '"');

    binfile.push_back('"');
    binfileName.push_back('"');

    cc.lexer()->AddMacro("__BINARY_PATH__", 15, binfile.c_str());
    cc.lexer()->AddMacro("__BINARY_NAME__", 15, binfileName.c_str());
}

static void
inst_datetime_defines(CompileContext& cc)
{
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

    cc.lexer()->AddMacro("__DATE__", 8, date);
    cc.lexer()->AddMacro("__TIME__", 8, ltime);
}

static void
initglobals(void)
{
    verbosity = 1;                     /* verbosity level, no copyright banner */
    sc_rationaltag = 0;         /* assume no support for rational numbers */
}

static std::string get_extension(const std::string& filename) {
    auto pos = filename.rfind('.');
    if (pos == std::string::npos)
        return {};

    /* ignore extension on a directory or at the start of the filename */
    if (pos == 0)
        return {};
    if (filename[pos - 1] == DIRSEP_CHAR)
        return {};
    if (filename.find(DIRSEP_CHAR, pos) != std::string::npos)
        return {};

    return filename.substr(pos);
}

/* set_extension
 * Set the default extension, or force an extension. To erase the
 * extension of a filename, set "extension" to an empty string.
 */
static void set_extension(std::string* filename, const char* extension, bool force) {
    assert(extension != NULL && (*extension == '\0' || *extension == '.'));
    assert(filename != NULL);

    auto old_ext = get_extension(*filename);
    if (force && !old_ext.empty())
        *filename = filename->substr(0, filename->size() - old_ext.size());
    if (force || old_ext.empty())
        *filename += extension;
}

static void
Usage(CompileContext& cc, args::Parser& parser, int argc, char** argv)
{
    if (cc.errfname().empty()) {
        setcaption();
        parser.usage(stdout, argc, argv);
    }
    exit(1);
}

static void parseoptions(CompileContext& cc, int argc, char** argv) {
    args::Parser parser;
    parser.enable_inline_values();
    parser.collect_extra_args();
    if (DIRSEP_CHAR != '/') {
        parser.allow_slashes();
    }

    parser.add_usage_line("sym=val", "Define constant \"sym\" with value \"val\".");
    parser.add_usage_line("sym=", "Define constant \"sym\" with value 0.");

    auto usage = "[options] <filename> [filename...]";
    parser.set_usage_line(usage);

    if (!parser.parse(argc, argv)) {
        Usage(cc, parser, argc, argv);
    }

    sc_syntax_only = opt_syntax_only.value();

    cc.options()->need_semicolon = opt_semicolons.value();
    cc.options()->tabsize = opt_tabsize.value();
    cc.options()->warnings_are_errors = opt_warnings_as_errors.value();
    cc.options()->use_stderr = opt_stderr.value();

    if (opt_no_verify.value())
        cc.set_verify_output(false);

    if (opt_prefixfile.hasValue())
        cc.set_default_include(opt_prefixfile.value());

    if (opt_outputfile.hasValue())
        cc.set_outfname(opt_outputfile.value());

    if (opt_verbosity.hasValue()) {
        if (isdigit(*opt_verbosity.value().c_str()))
            verbosity = atoi(opt_verbosity.value().c_str());
        else
            verbosity = 2;
    }

    if (opt_active_dir.hasValue()) {
        const char* ptr = opt_active_dir.value().c_str();
#if defined dos_setdrive
        if (ptr[1] == ':')
            dos_setdrive(toupper(*ptr) - 'A' + 1); /* set active drive */
#endif
            if (chdir(ptr)) {
                fprintf(stderr, "chdir failed: %s\n", strerror(errno));
                exit(1);
            }
    }

    if (opt_error_file.hasValue())
        cc.set_errfname(opt_error_file.value());

#if defined __WIN32__ || defined _WIN32 || defined _Windows
    if (opt_hwnd.hasValue()) {
        hwndFinish = (HWND)atoi(opt_hwnd.value().c_str());
        if (!IsWindow(hwndFinish))
            hwndFinish = (HWND)0;
    }
#endif

    for (const auto& inc_path : opt_includes.values()) {
        std::string str = inc_path;

        if (str.empty())
            continue;
        if (str.back() != DIRSEP_CHAR)
            str.push_back(DIRSEP_CHAR);

        cc.options()->include_paths.emplace_back(str);
    }

    for (const auto& warning : opt_warnings.values()) {
        char* ptr;
        int i = (int)strtol(warning.c_str(), (char**)&ptr, 10);
        if (*ptr == '-')
            cc.reports()->EnableWarning(i, 0);
        else if (*ptr == '+')
            cc.reports()->EnableWarning(i, 1);
        else if (*ptr == '\0')
            cc.reports()->EnableWarning(i, 2);
    }

    for (const auto& option : parser.extra_args()) {
        char str[PATH_MAX];
        const char* ptr = nullptr;
        const char* arg = option.c_str();
        if (arg[0] == '@') {
            fprintf(stderr, "Response files (@ prefix) are no longer supported.");
            exit(1);
        } else if ((ptr = strchr(arg, '=')) != NULL) {
            int i = (int)(ptr - arg);
            SafeStrcpyN(str, PATH_MAX, arg, i);
            i = atoi(ptr + 1);
            DefineConstant(CompileContext::get(), gAtoms.add(str), i, sGLOBAL);
        } else {
            std::string path = arg;
            set_extension(&path, ".sp", false);
            ke::SafeStrcpy(str, sizeof(str), path.c_str());

            cc.options()->source_files.emplace_back(str);
            /* The output name is the first input name with a different extension,
             * but it is stored in a different directory
             */
            if (cc.outfname().empty()) {
                if ((ptr = strrchr(str, DIRSEP_CHAR)) != NULL)
                    ptr++; /* strip path */
                else
                    ptr = str;
                assert(strlen(ptr) < PATH_MAX);
                cc.set_outfname(ptr);
            }
            set_extension(&cc.outfname(), ".asm", true);
        }
    }

    if (cc.options()->source_files.empty())
        Usage(cc, parser, argc, argv);
}

#if defined __EMSCRIPTEN__
// Needed due to EM_ASM usage
__attribute__((noinline))
#endif
static void
setconfig(char* root)
{
    char path[PATH_MAX];
    char *ptr, *base;
    int len;

    /* add the default "include" directory */
#if defined KE_WINDOWS
    GetModuleFileNameA(NULL, path, PATH_MAX);
#elif defined __EMSCRIPTEN__
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
#else
    if (root != NULL)
        SafeStrcpy(path, sizeof(path), root); /* path + filename (hopefully) */
#endif

    /* terminate just behind last \ or : */
    if ((ptr = strrchr(path, DIRSEP_CHAR)) != NULL || (ptr = strchr(path, ':')) != NULL) {
        /* If there is no "\" or ":", the string probably does not contain the
         * path; so we just don't add it to the list in that case
         */
        *(ptr + 1) = '\0';
        base = ptr;
        strcat(path, "include");
        len = strlen(path);
        path[len] = DIRSEP_CHAR;
        path[len + 1] = '\0';
        /* see if it exists */
        if (access(path, 0) != 0 && *base == DIRSEP_CHAR) {
            /* There is no "include" directory below the directory where the compiler
             * is found. This typically means that the compiler is in a "bin" sub-directory
             * and the "include" is below the *parent*. So find the parent...
             */
            *base = '\0';
            if ((ptr = strrchr(path, DIRSEP_CHAR)) != NULL) {
                *(ptr + 1) = '\0';
                strcat(path, "include");
                len = strlen(path);
                path[len] = DIRSEP_CHAR;
                path[len + 1] = '\0';
            } else {
                *base = DIRSEP_CHAR;
            }
        }

        auto& cc = CompileContext::get();
        cc.options()->include_paths.emplace_back(path);
    }
}

static void
setcaption(void)
{
    printf("SourcePawn Compiler %s\n", SOURCEPAWN_VERSION);
    printf("Copyright (c) 1997-2006 ITB CompuPhase\n");
    printf("Copyright (c) 2004-2021 AlliedModders LLC\n\n");
}

static void
setconstants(void)
{
    gTypes.init();
    assert(sc_rationaltag);

    auto& cc = CompileContext::get();
    DefineConstant(cc, gAtoms.add("EOS"), 0, 0);
    DefineConstant(cc, gAtoms.add("INVALID_FUNCTION"), -1, gTypes.tag_nullfunc());
    DefineConstant(cc, gAtoms.add("cellmax"), INT_MAX, 0);
    DefineConstant(cc, gAtoms.add("cellmin"), INT_MIN, 0);

    DefineConstant(cc, gAtoms.add("__Pawn"), VERSION_INT, 0);

    DefineConstant(cc, gAtoms.add("debug"), 2, 0);
}
