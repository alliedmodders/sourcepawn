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
#include "code-generator.h"
#include "lexer-inl.h"
#include "output-buffer.h"
#include "parser.h"
#include "semantics.h"
#include "symbols.h"
#include "types.h"

#if defined _WIN32
#    include <conio.h>
#    include <io.h>
#    define strcasecmp stricmp
#endif

#if defined __linux__ || defined __FreeBSD__ || defined __OpenBSD__ || defined DARWIN
#    include <unistd.h>
#    include "binreloc.h" /* from BinReloc, see www.autopackage.org */
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

#include "assembler.h"
#include "emitter.h"
#include "errors.h"
#include "expressions.h"
#include "lexer.h"
#include "sc.h"
#include "sci18n.h"
#include "sclist.h"
#include "sctracker.h"
#include "scvars.h"
#define VERSION_INT 0x0302

using namespace ke;

int pc_anytag = 0;
int pc_functag = 0;
int pc_tag_string = 0;
int pc_tag_void = 0;
int pc_tag_object = 0;
int pc_tag_bool = 0;
int pc_tag_null_t = 0;
int pc_tag_nullfunc_t = 0;

sp::StringPool gAtoms;

static void resetglobals(void);
static void initglobals(void);
static char* get_extension(char* filename);
static void setopt(int argc, char** argv, char* oname, char* ename, char* pname);
static void setconfig(char* root);
static void setcaption(void);
static void setconstants(void);
static void inst_datetime_defines(void);
static void inst_binary_name(std::string binfile);

enum {
    TEST_PLAIN,  /* no parentheses */
    TEST_PARENS, /* '(' <expr> ')' */
    TEST_OPT,    /* '(' <expr> ')' or <expr> */
};
static int norun = 0;                 /* the compiler never ran */
static int verbosity = 1;             /* verbosity level, 0=quiet, 1=normal, 2=verbose */
static int sc_parsenum = 0;           /* number of the extra parses */
#if defined __WIN32__ || defined _WIN32 || defined _Windows
static HWND hwndFinish = 0;
#endif
static int sc_syntax_only = FALSE;

char g_tmpfile[PATH_MAX] = {0};

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

/*  "main" of the compiler
 */
int
pc_compile(int argc, char* argv[]) {
    int entry, jmpcode;
    int retcode;
    char incfname[PATH_MAX];
    int64_t inpfmark;
    int lcl_needsemicolon, lcl_tabsize, lcl_require_newdecls;
    char* ptr;

    CompileContext cc;
    cc.set_globals(new SymbolScope(nullptr, sGLOBAL));

#ifdef __EMSCRIPTEN__
    setup_emscripten_fs();
#endif

    /* set global variables to their initial value */
    initglobals();
    errorset(sRESET, 0);
    errorset(sEXPRRELEASE, 0);
    lexinit();

    /* make sure that we clean up on a fatal error; do this before the first
     * call to error(). */
    if ((jmpcode = setjmp(errbuf)) != 0)
        goto cleanup;

    setopt(argc, argv, outfname, errfname, incfname);
    strcpy(binfname, outfname);
    ptr = get_extension(binfname);
    if (ptr != NULL && strcasecmp(ptr, ".asm") == 0)
        set_extension(binfname, ".smx", TRUE);
    else
        set_extension(binfname, ".smx", FALSE);
    /* set output names that depend on the input name */
    if (sc_listing)
        set_extension(outfname, ".lst", TRUE);
    else
        set_extension(outfname, ".asm", TRUE);
    if (strlen(errfname) != 0)
        remove(errfname); /* delete file on startup */
    else if (verbosity > 0)
        setcaption();
    setconfig(argv[0]); /* the path to the include files */
    sc_ctrlchar_org = sc_ctrlchar;
    lcl_needsemicolon = sc_needsemicolon;
    lcl_require_newdecls = sc_require_newdecls;
    lcl_tabsize = sc_tabsize;

    /* optionally create a temporary input file that is a collection of all
     * input files
     */
    assert(get_sourcefile(0) != NULL); /* there must be at least one source file */
    if (get_sourcefile(1) != NULL) {
        report(314);
    }
    inpf_org = std::make_shared<SourceFile>();
    if (!inpf_org->Open(get_sourcefile(0)))
        report(FATAL_ERROR_READ) << get_sourcefile(0);
    skip_utf8_bom(inpf_org.get());
    freading = TRUE;

    setconstants(); /* set predefined constants and tagnames */
    sc_status = statFIRST;
    /* write starting options (from the command line or the configuration file) */
    if (sc_listing) {
        gAsmBuffer << "#pragma ctrlchar 0x" << ke::StringPrintf("%02x", sc_ctrlchar) << "\n";
        gAsmBuffer << "#pragma semicolon " << (sc_needsemicolon ? "true" : "false") << "\n";
        gAsmBuffer << "#pragma tabsize " << sc_tabsize << "\n";
        setfiledirect(inpf->name());
    }
    /* do the first pass through the file (or possibly two or more "first passes") */
    sc_parsenum = 0;
    inpfmark = inpf_org->Pos();
    do {
        /* reset "defined" flag of all functions and global variables */
        reduce_referrers(cc);
        delete_symbols(cc, false);
        delete_substtable();
        inst_datetime_defines();
        inst_binary_name(binfname);
        resetglobals();
        gTypes.clearExtendedTypes();
        pstructs_free();
        funcenums_free();
        methodmaps_free();
        sc_ctrlchar = sc_ctrlchar_org;
        sc_needsemicolon = lcl_needsemicolon;
        sc_require_newdecls = lcl_require_newdecls;
        sc_tabsize = lcl_tabsize;
        errorset(sRESET, 0);
        clear_errors();
        /* reset the source file */
        inpf = inpf_org;
        freading = TRUE;
        inpf->Reset(inpfmark);
        sc_reparse = FALSE;          /* assume no extra passes */
        sc_status = statFIRST;       /* resetglobals() resets it to IDLE */
        insert_inputfile(inpf->name()); /* save for the error system */

        /* look for default prefix (include) file in include paths,
         * but only error if it was manually set on the command line
         */
        if (strlen(incfname) > 0) {
            int defOK = plungefile(incfname, FALSE, TRUE);
            if (!defOK && strcmp(incfname, sDEF_PREFIX) != 0) {
                error(FATAL_ERROR_READ, incfname);
            }
        }
        preprocess(true); /* fetch first line */

        Parser parser;
        parser.parse();      /* process all input */

        sc_parsenum++;
    } while (sc_reparse || sc_parsenum == 1);

    /* second (or third) pass */
    sc_status = statWRITE; /* set, to enable warnings */

    if (sc_listing)
        goto cleanup;

    /* ??? for re-parsing the listing file instead of the original source
     * file (and doing preprocessing twice):
     * - close input file, close listing file
     * - re-open listing file for reading (inpf)
     * - open assembler file (outf)
     */

    /* reset "defined" flag of all functions and global variables */
    reduce_referrers(cc);
    deduce_liveness(cc);
    delete_symbols(cc, false);
    gTypes.clearExtendedTypes();
    funcenums_free();
    methodmaps_free();
    pstructs_free();
    delete_substtable();
    inst_datetime_defines();
    inst_binary_name(binfname);
    resetglobals();
    sc_ctrlchar = sc_ctrlchar_org;
    sc_needsemicolon = lcl_needsemicolon;
    sc_require_newdecls = lcl_require_newdecls;
    sc_tabsize = lcl_tabsize;
    errorset(sRESET, 0);
    clear_errors();
    /* reset the source file */
    inpf = inpf_org;
    freading = TRUE;
    inpf->Reset(inpfmark);       /* reset file position */
    lexinit();                   /* clear internal flags of lex() */
    sc_status = statWRITE;       /* allow to write --this variable was reset by resetglobals() */
    writeleader();
    insert_dbgfile(inpf->name());   /* attach to debug information */
    insert_inputfile(inpf->name()); /* save for the error system */
    if (strlen(incfname) > 0) {
        plungefile(incfname, FALSE, TRUE); /* parse "default.inc" (again) */
    }
    preprocess(true); /* fetch first line */

    {
        Parser parser;
        parser.parse();      /* process all input */
    }

    /* inpf is already closed when readline() attempts to pop of a file */
    writetrailer(); /* write remaining stuff */

    entry = TestSymbols(cc.globals(), FALSE); /* test for unused or undefined
                                               * functions and variables */
    if (!entry)
        error(13); /* no entry point (no public functions) */

cleanup:
    sc_shutting_down = true;
    dump_error_report(true);

    inpf = nullptr;
    inpf_org = nullptr;

    // Write the binary file.
    if (!(sc_asmfile || sc_listing || sc_syntax_only) && errnum == 0 && jmpcode == 0)
        assemble(cc, binfname);

    if ((sc_asmfile || sc_listing) && !gAsmBuffer.empty()) {
        std::unique_ptr<FILE, decltype(&fclose)> fp(fopen(outfname, "wb"), fclose);
        auto data = gAsmBuffer.str();
        if (fwrite(data.c_str(), data.size(), 1, fp.get()) != 1)
            error(FATAL_ERROR_WRITE, outfname);
    }

    if (errnum == 0 && strlen(errfname) == 0) {
        if (pc_stksize_override)
            pc_stksize = pc_stksize_override;

        if ((!norun && (sc_debug & sSYMBOLIC) != 0) || verbosity >= 2) {
            printf("Code size:         %8ld bytes\n", (long)code_idx);
            printf("Data size:         %8ld bytes\n", (long)glb_declared * sizeof(cell));
            printf("Stack/heap size:   %8ld bytes\n", (long)pc_stksize * sizeof(cell));
            printf("Total requirements:%8ld bytes\n", (long)code_idx +
                                                             (long)glb_declared * sizeof(cell) +
                                                             (long)pc_stksize * sizeof(cell));
        }
        if (opt_show_stats.value()) {
            size_t allocated, reserved, bookkeeping;
            gPoolAllocator.memoryUsage(&allocated, &reserved, &bookkeeping);

            printf("Pool allocation:   %8" KE_FMT_SIZET " bytes\n", allocated);
            printf("Pool unused:       %8" KE_FMT_SIZET " bytes\n", reserved - allocated);
            printf("Pool bookkeeping:  %8" KE_FMT_SIZET " bytes\n", bookkeeping);
        }
    }

    if (g_tmpfile[0] != '\0') {
        remove(g_tmpfile);
    }

    gCurrentFileStack.clear();
    gCurrentLineStack.clear();
    gInputFileStack.clear();

    delete_symbols(cc, true);
    delete_aliastable();
    delete_pathtable();
    delete_sourcefiletable();
    delete_inputfiletable();
    delete_dbgstringtable();
    gTypes.clear();
    funcenums_free();
    methodmaps_free();
    pstructs_free();
    delete_substtable();
    if (errnum != 0) {
        if (strlen(errfname) == 0)
            printf("\n%d Error%s.\n", errnum, (errnum > 1) ? "s" : "");
        retcode = 1;
    } else if (warnnum != 0) {
        if (strlen(errfname) == 0)
            printf("\n%d Warning%s.\n", warnnum, (warnnum > 1) ? "s" : "");
        retcode = 0; /* use "0", so that MAKE and similar tools continue */
    } else {
        retcode = jmpcode;
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
inst_binary_name(std::string binfile)
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

    insert_subst("__BINARY_PATH__", 15, binfile.c_str());
    insert_subst("__BINARY_NAME__", 15, binfileName.c_str());
}

static void
inst_datetime_defines(void)
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

    insert_subst("__DATE__", 8, date);
    insert_subst("__TIME__", 8, ltime);
}

static void
resetglobals(void)
{
    /* reset the subset of global variables that is modified by the first pass */
    curfunc = NULL;        /* pointer to current function */
    sc_labnum = 0;         /* top value of (internal) labels */
    glb_declared = 0;      /* number of global cells declared */
    code_idx = 0;          /* number of bytes with generated code */
    curseg = 0;            /* 1 if currently parsing CODE, 2 if parsing DATA */
    freading = FALSE;      /* no input file ready yet */
    fline = 0;             /* the line number in the current file */
    fnumber = 0;           /* the file number in the file table (debugging) */
    sideeffect = 0;        /* true if an expression causes a side-effect */
    stmtindent = 0;        /* current indent of the statement */
    indent_nowarn = FALSE; /* do not skip warning "217 loose indentation" */
    sc_status = statIDLE;
    pc_deprecate = "";

    sc_intest = false;
    sc_allowtags = true;
    fcurrent = 0;
}

static void
initglobals(void)
{
    resetglobals();

    sc_asmfile = FALSE;                /* do not create .ASM file */
    sc_listing = FALSE;                /* do not create .LST file */
    sc_ctrlchar = CTRL_CHAR;           /* the escape character */
    errnum = 0;                        /* number of errors */
    warnnum = 0;                       /* number of warnings */
    verbosity = 1;                     /* verbosity level, no copyright banner */
    sc_debug = sCHKBOUNDS | sSYMBOLIC; /* sourcemod: full debug stuff */
    sc_needsemicolon = FALSE;          /* semicolon required to terminate expressions? */
    sc_require_newdecls = FALSE;
    pc_stksize = sDEF_AMXSTACK; /* default stack size */
    sc_tabsize = 8;             /* assume a TAB is 8 spaces */
    sc_rationaltag = 0;         /* assume no support for rational numbers */

    outfname[0] = '\0';      /* output file name */
    errfname[0] = '\0';      /* error file name */

    pline[0] = '\0'; /* the line read from the input file */
    lptr = NULL;     /* points to the current position in "pline" */
}

static char*
get_extension(char* filename)
{
    char* ptr;

    assert(filename != NULL);
    ptr = strrchr(filename, '.');
    if (ptr != NULL) {
        /* ignore extension on a directory or at the start of the filename */
        if (strchr(ptr, DIRSEP_CHAR) != NULL || ptr == filename || *(ptr - 1) == DIRSEP_CHAR)
            ptr = NULL;
    }
    return ptr;
}

/* set_extension
 * Set the default extension, or force an extension. To erase the
 * extension of a filename, set "extension" to an empty string.
 */
void
set_extension(char* filename, const char* extension, int force)
{
    char* ptr;

    assert(extension != NULL && (*extension == '\0' || *extension == '.'));
    assert(filename != NULL);
    ptr = get_extension(filename);
    if (force && ptr != NULL)
        *ptr = '\0'; /* set zero terminator at the position of the period */
    if (force || ptr == NULL)
        strcat(filename, extension);
}

args::ToggleOption opt_assembly("-a", "--assembly-only", Some(false), "Output assembler code");
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
args::ToggleOption opt_listing("-l", "--listing", Some(false),
                               "Create list file (preprocess only)");
args::IntOption opt_compression("-z", "--compress-level", Some(9),
                                "Compression level, default 9 (0=none, 1=worst, 9=best)");
args::IntOption opt_tabsize("-t", "--tabsize", Some(8),
                            "TAB indent size (in character positions, default=8)");
args::StringOption opt_verbosity("-v", "--verbose", {},
                                 "Verbosity level; 0=quiet, 1=normal, 2=verbose");
args::StringOption opt_prefixfile("-p", "--prefix", {}, "Set name of \"prefix\" file");
args::StringOption opt_outputfile("-o", "--output", {},
                                  "Set base name of (P-code) output file");
args::IntOption opt_optlevel("-O", "--opt-level", Some(2),
                             "Optimization level (0=none, 2=full)");
args::RepeatOption<std::string> opt_includes("-i", "--include", "Path for include files");
args::RepeatOption<std::string> opt_warnings("-w", "--warning",
                                         "Disable a specific warning by its number.");
args::ToggleOption opt_semicolons("-;", "--require-semicolons", Some(false),
                                  "Require a semicolon to end each statement.");
args::ToggleOption opt_syntax_only(nullptr, "--syntax-only", Some(false),
                              "Perform a dry-run (No file output) on the input");
args::ToggleOption opt_stderr(nullptr, "--use-stderr", Some(false),
                              "Use stderr instead of stdout for error messages.");

static void
Usage(args::Parser& parser, int argc, char** argv)
{
    if (strlen(errfname) == 0) {
        setcaption();
        parser.usage(stdout, argc, argv);
    }
    exit(1);
}

static void
parseoptions(int argc, char** argv, char* oname, char* ename, char* pname)
{
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
        Usage(parser, argc, argv);
    }

    sc_warnings_are_errors = opt_warnings_as_errors.value();
    sc_showincludes = opt_showincludes.value();
    sc_listing = opt_listing.value();
    sc_compression_level = opt_compression.value();
    sc_tabsize = opt_tabsize.value();
    sc_needsemicolon = opt_semicolons.value();
    sc_syntax_only = opt_syntax_only.value();
    sc_use_stderr = opt_stderr.value();

    int pc_optimize = opt_optlevel.value();
    if (pc_optimize < sOPTIMIZE_NONE || pc_optimize >= sOPTIMIZE_NUMBER)
        Usage(parser, argc, argv);

    if (opt_prefixfile.hasValue())
        SafeStrcpy(pname, PATH_MAX, opt_prefixfile.value().c_str());
    if (opt_outputfile.hasValue())
        SafeStrcpy(oname, PATH_MAX, opt_outputfile.value().c_str());

    if (opt_verbosity.hasValue()) {
        if (isdigit(*opt_verbosity.value().c_str()))
            verbosity = atoi(opt_verbosity.value().c_str());
        else
            verbosity = 2;
    }

    sc_asmfile = opt_assembly.value();
    if (sc_asmfile && verbosity > 1)
        verbosity = 1;

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
        SafeStrcpy(ename, PATH_MAX, opt_error_file.value().c_str());

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

        insert_path(str.c_str());
    }

    for (const auto& warning : opt_warnings.values()) {
        char* ptr;
        int i = (int)strtol(warning.c_str(), (char**)&ptr, 10);
        if (*ptr == '-')
            pc_enablewarning(i, 0);
        else if (*ptr == '+')
            pc_enablewarning(i, 1);
        else if (*ptr == '\0')
            pc_enablewarning(i, 2);
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
            add_constant(CompileContext::get(), nullptr, gAtoms.add(str), i, sGLOBAL, 0, -1);
        } else {
            SafeStrcpy(str, sizeof(str) - 5, arg); /* -5 because default extension is ".sp" */
            set_extension(str, ".sp", FALSE);
            insert_sourcefile(str);
            /* The output name is the first input name with a different extension,
             * but it is stored in a different directory
             */
            if (strlen(oname) == 0) {
                if ((ptr = strrchr(str, DIRSEP_CHAR)) != NULL)
                    ptr++; /* strip path */
                else
                    ptr = str;
                assert(strlen(ptr) < PATH_MAX);
                strcpy(oname, ptr);
            }
            set_extension(oname, ".asm", TRUE);
        }
    }

    if (get_sourcefile(0) == NULL)
        Usage(parser, argc, argv);
}

static void
setopt(int argc, char** argv, char* oname, char* ename, char* pname)
{
    delete_sourcefiletable(); /* make sure it is empty */
    *oname = '\0';
    *ename = '\0';
    *pname = '\0';
    strcpy(pname, sDEF_PREFIX);

    parseoptions(argc, argv, oname, ename, pname);
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
#elif defined ENABLE_BINRELOC
    /* see www.autopackage.org for the BinReloc module */
    br_init_lib(NULL);
    ptr = br_find_exe("spcomp");
    SafeStrcpy(path, sizeof(path), ptr);
    free(ptr);
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
        insert_path(path);
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
    int debug;

    assert(sc_status == statIDLE);

    gTypes.init();
    assert(sc_rationaltag);

    auto& cc = CompileContext::get();
    add_constant(cc, nullptr, gAtoms.add("true"), 1, sGLOBAL, 1, -1); /* boolean flags */
    add_constant(cc, nullptr, gAtoms.add("false"), 0, sGLOBAL, 1, -1);
    add_constant(cc, nullptr, gAtoms.add("EOS"), 0, sGLOBAL, 0, -1); /* End Of String, or '\0' */
    add_constant(cc, nullptr, gAtoms.add("INVALID_FUNCTION"), -1, sGLOBAL, pc_tag_nullfunc_t, -1);
    add_constant(cc, nullptr, gAtoms.add("cellmax"), INT_MAX, sGLOBAL, 0, -1);
    add_constant(cc, nullptr, gAtoms.add("cellmin"), INT_MIN, sGLOBAL, 0, -1);

    add_constant(cc, nullptr, gAtoms.add("__Pawn"), VERSION_INT, sGLOBAL, 0, -1);

    debug = 0;
    if ((sc_debug & (sCHKBOUNDS | sSYMBOLIC)) == (sCHKBOUNDS | sSYMBOLIC))
        debug = 2;
    else if ((sc_debug & sCHKBOUNDS) == sCHKBOUNDS)
        debug = 1;
    add_constant(cc, nullptr, gAtoms.add("debug"), debug, sGLOBAL, 0, -1);
}

void
fill_arg_defvalue(VarDecl* decl, arginfo* arg)
{
    arg->def = new DefaultArg();
    arg->def->tag = decl->type().tag;

    if (SymbolExpr* expr = decl->init_rhs()->AsSymbolExpr()) {
        symbol* sym = expr->sym();
        assert(sym->vclass == sGLOBAL);

        arg->def->val = ke::Some(sym->addr());
        arg->type.tag = sym->tag;
        if (sc_status == statWRITE && (sym->usage & uREAD) == 0)
            markusage(sym, uREAD);
        return;
    }

    ArrayData data;
    BuildArrayInitializer(decl, &data, 0);

    arg->def->array = new ArrayData;
    *arg->def->array = std::move(data);
}
