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
#include "libpawnc.h"
#include "sc.h"
#include "sci18n.h"
#include "sclist.h"
#include "sctracker.h"
#include "scvars.h"
#include "sp_symhash.h"
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
static void reduce_referrers(symbol* root);
static void deduce_liveness(symbol* root);
static void inst_datetime_defines(void);
static void inst_binary_name(std::string binfile);
static int operatorname(sp::Atom** name);
static void rewrite_type_for_enum_struct(typeinfo_t* info);

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
    void* inpfmark;
    int lcl_needsemicolon, lcl_tabsize, lcl_require_newdecls;
    char* ptr;

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

    sp_Globals = NewHashTable();
    if (!sp_Globals)
        error(FATAL_ERROR_OOM);

    /* allocate memory for fixed tables */
    inpfname = (char*)malloc(PATH_MAX);
    if (inpfname == NULL)
        error(FATAL_ERROR_OOM); /* insufficient memory */

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
        /* there are at least two or more source files */
        char *tname, *sname;
        void *ftmp, *fsrc;
        int fidx;
#if defined __WIN32__ || defined _WIN32
        tname = _tempnam(NULL, "pawn");
#elif defined(MACOS) && !defined(__MACH__)
        /* tempnam is not supported for the Macintosh CFM build. */
        error(FATAL_ERROR_INVALID_INSN, get_sourcefile(1));
        tname = NULL;
        sname = NULL;
#else
        char* buffer = strdup(P_tmpdir "/pawn.XXXXXX");
        close(mkstemp(buffer));
        tname = buffer;
#endif
        ftmp = pc_createsrc(tname);
        for (fidx = 0; (sname = get_sourcefile(fidx)) != NULL; fidx++) {
            unsigned char tstring[128];
            fsrc = pc_opensrc(sname);
            if (fsrc == NULL) {
                pc_closesrc(ftmp);
                remove(tname);
                strcpy(inpfname, sname); /* avoid invalid filename */
                error(FATAL_ERROR_READ, sname);
            }
            pc_writesrc(ftmp, (unsigned char*)"#file \"");
            pc_writesrc(ftmp, (unsigned char*)sname);
            pc_writesrc(ftmp, (unsigned char*)"\"\n");
            skip_utf8_bom(fsrc);
            while (!pc_eofsrc(fsrc) && pc_readsrc(fsrc, tstring, sizeof tstring)) {
                pc_writesrc(ftmp, tstring);
            }
            pc_closesrc(fsrc);
        }
        pc_closesrc(ftmp);
        strcpy(inpfname, tname);
        strcpy(g_tmpfile, tname);
        free(tname);
    } else {
        strcpy(inpfname, get_sourcefile(0));
    }
    inpf_org = pc_opensrc(inpfname);
    if (inpf_org == NULL)
        error(FATAL_ERROR_READ, inpfname);
    skip_utf8_bom(inpf_org);
    freading = TRUE;
    setconstants(); /* set predefined constants and tagnames */
    sc_status = statFIRST;
    /* write starting options (from the command line or the configuration file) */
    if (sc_listing) {
        gAsmBuffer << "#pragma ctrlchar 0x" << ke::StringPrintf("%02x", sc_ctrlchar) << "\n";
        gAsmBuffer << "#pragma semicolon " << (sc_needsemicolon ? "true" : "false") << "\n";
        gAsmBuffer << "#pragma tabsize " << sc_tabsize << "\n";
        setfiledirect(inpfname);
    }
    /* do the first pass through the file (or possibly two or more "first passes") */
    sc_parsenum = 0;
    inpfmark = pc_getpossrc(inpf_org);
    do {
        /* reset "defined" flag of all functions and global variables */
        reduce_referrers(&glbtab);
        delete_symbols(&glbtab, FALSE);
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
        pc_resetsrc(inpf, inpfmark); /* reset file position */
        sc_reparse = FALSE;          /* assume no extra passes */
        sc_status = statFIRST;       /* resetglobals() resets it to IDLE */
        insert_inputfile(inpfname); /* save for the error system */

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
    reduce_referrers(&glbtab);
    deduce_liveness(&glbtab);
    delete_symbols(&glbtab, FALSE);
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
    pc_resetsrc(inpf, inpfmark); /* reset file position */
    lexinit();                   /* clear internal flags of lex() */
    sc_status = statWRITE;       /* allow to write --this variable was reset by resetglobals() */
    writeleader(&glbtab);
    insert_dbgfile(inpfname);   /* attach to debug information */
    insert_inputfile(inpfname); /* save for the error system */
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

    entry = TestSymbols(&glbtab, FALSE); /* test for unused or undefined
                                          * functions and variables */
    if (!entry)
        error(13); /* no entry point (no public functions) */

cleanup:
    sc_shutting_down = true;
    dump_error_report(true);

    /* main source file is not closed, do it now */
    if (inpf != NULL) {
        pc_closesrc(inpf);
        inpf = nullptr;
    }

    // Write the binary file.
    if (!(sc_asmfile || sc_listing || sc_syntax_only) && errnum == 0 && jmpcode == 0)
        assemble(binfname);

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
            pc_printf("Code size:         %8ld bytes\n", (long)code_idx);
            pc_printf("Data size:         %8ld bytes\n", (long)glb_declared * sizeof(cell));
            pc_printf("Stack/heap size:   %8ld bytes\n", (long)pc_stksize * sizeof(cell));
            pc_printf("Total requirements:%8ld bytes\n", (long)code_idx +
                                                             (long)glb_declared * sizeof(cell) +
                                                             (long)pc_stksize * sizeof(cell));
        }
        if (opt_show_stats.value()) {
            size_t allocated, reserved, bookkeeping;
            gPoolAllocator.memoryUsage(&allocated, &reserved, &bookkeeping);

            pc_printf("Pool allocation:   %8" KE_FMT_SIZET " bytes\n", allocated);
            pc_printf("Pool unused:       %8" KE_FMT_SIZET " bytes\n", reserved - allocated);
            pc_printf("Pool bookkeeping:  %8" KE_FMT_SIZET " bytes\n", bookkeeping);
        }
    }

    if (g_tmpfile[0] != '\0') {
        remove(g_tmpfile);
    }
    if (inpfname != NULL) {
        free(inpfname);
    }

    gCurrentFileStack.clear();
    gCurrentLineStack.clear();
    gInputFileStack.clear();
    gInputFilenameStack.clear();

    delete_symbols(&glbtab, TRUE);
    DestroyHashTable(sp_Globals);
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
            pc_printf("\n%d Error%s.\n", errnum, (errnum > 1) ? "s" : "");
        retcode = 1;
    } else if (warnnum != 0) {
        if (strlen(errfname) == 0)
            pc_printf("\n%d Warning%s.\n", warnnum, (warnnum > 1) ? "s" : "");
        retcode = 0; /* use "0", so that MAKE and similar tools continue */
    } else {
        retcode = jmpcode;
        if (retcode == 0 && verbosity >= 2)
            pc_printf("\nDone.\n");
    }
#if defined __WIN32__ || defined _WIN32 || defined _Windows
    if (IsWindow(hwndFinish))
        PostMessageA(hwndFinish, RegisterWindowMessageA("PawnNotify"), retcode, 0L);
#endif
    return retcode;
}

int
pc_addconstant(const char* name, cell value, int tag)
{
    errorset(sFORCESET, 0); /* make sure error engine is silenced */
    sc_status = statIDLE;
    add_constant(name, value, sGLOBAL, tag);
    return 1;
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
    inpf = NULL;             /* file read from */
    inpfname = NULL;         /* pointer to name of the file currently read from */
    glbtab.next = NULL;      /* clear global variables/constants table */

    pline[0] = '\0'; /* the line read from the input file */
    lptr = NULL;     /* points to the current position in "pline" */
    inpf_org = NULL; /* main source file */
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
            add_constant(str, i, sGLOBAL, 0);
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
    pc_printf("SourcePawn Compiler %s\n", SOURCEPAWN_VERSION);
    pc_printf("Copyright (c) 1997-2006 ITB CompuPhase\n");
    pc_printf("Copyright (c) 2004-2021 AlliedModders LLC\n\n");
}

static void
setconstants(void)
{
    int debug;

    assert(sc_status == statIDLE);

    gTypes.init();
    assert(sc_rationaltag);

    add_constant("true", 1, sGLOBAL, 1); /* boolean flags */
    add_constant("false", 0, sGLOBAL, 1);
    add_constant("EOS", 0, sGLOBAL, 0); /* End Of String, or '\0' */
    add_constant("INVALID_FUNCTION", -1, sGLOBAL, pc_tag_nullfunc_t);
    add_constant("cellmax", INT_MAX, sGLOBAL, 0);
    add_constant("cellmin", INT_MIN, sGLOBAL, 0);

    add_constant("__Pawn", VERSION_INT, sGLOBAL, 0);
    add_constant("__LINE__", 0, sGLOBAL, 0);

    debug = 0;
    if ((sc_debug & (sCHKBOUNDS | sSYMBOLIC)) == (sCHKBOUNDS | sSYMBOLIC))
        debug = 2;
    else if ((sc_debug & sCHKBOUNDS) == sCHKBOUNDS)
        debug = 1;
    add_constant("debug", debug, sGLOBAL, 0);
}

// Consumes a line, returns FALSE if EOF hit.
static int
consume_line()
{
    int val;
    const char* str;

    // First check for EOF.
    if (lex(&val, &str) == 0)
        return FALSE;
    lexpush();

    while (!matchtoken(tTERM)) {
        // Check for EOF.
        if (lex(&val, &str) == 0)
            return FALSE;
    }

    return TRUE;
}

int
parse_new_typename(const token_t* tok)
{
    token_t tmp;

    if (!tok) {
        lextok(&tmp);
        tok = &tmp;
    }

    switch (tok->id) {
        case tINT:
            return 0;
        case tCHAR:
            return pc_tag_string;
        case tVOID:
            return pc_tag_void;
        case tOBJECT:
            return pc_tag_object;
        case tLABEL:
            error(120);
            return pc_addtag(tok->str);
        case tSYMBOL: {
            if (strcmp(tok->str, "float") == 0)
                return sc_rationaltag;
            if (strcmp(tok->str, "bool") == 0)
                return pc_tag_bool;
            int tag = pc_findtag(tok->str);
            if (tag == sc_rationaltag) {
                error(98, "Float", "float");
            } else if (tag == pc_tag_string) {
                error(98, "String", "char");
            } else if (tag == 0) {
                error(98, "_", "int");
            } else if (tag == -1) {
                report(139) << tok->str;
                tag = 0;
            } else if (tag != pc_anytag) {
                Type* type = gTypes.find(tag);
                // Perform some basic filters so we can start narrowing down what can
                // be used as a type.
                if (type->isDeclaredButNotDefined())
                    report(139) << tok->str;
            }
            return tag;
        }
    }

    error(122);
    return -1;
}

bool
parse_new_typename(const token_t* tok, int* tagp)
{
    int tag = parse_new_typename(tok);
    if (tag >= 0)
        *tagp = tag;
    else
        *tagp = 0;
    return true;
}

static int
parse_new_typeexpr(typeinfo_t* type, const token_t* first, int flags)
{
    token_t tok;

    if (first)
        tok = *first;
    else
        lextok(&tok);

    if (tok.id == tCONST) {
        if (type->is_const)
            error(138);
        type->is_const = true;
        lextok(&tok);
    }

    if (!parse_new_typename(&tok, &type->tag))
        return FALSE;
    type->declared_tag = type->tag;

    // Note: we could have already filled in the prefix array bits, so we check
    // that ident != iARRAY before looking for an open bracket.
    if (type->ident != iARRAY && matchtoken('[')) {
        do {
            if (type->numdim == sDIMEN_MAX) {
                error(53);
                break;
            }
            type->dim[type->numdim++] = 0;
            if (!matchtoken(']')) {
                error(101);

                // Try to eat a close bracket anyway.
                cell ignored;
                if (exprconst(&ignored, nullptr, nullptr))
                    matchtoken(']');
            }
        } while (matchtoken('['));
        type->ident = iREFARRAY;
    }

    if (flags & DECLFLAG_ARGUMENT) {
        if (matchtoken('&')) {
            if (type->ident == iARRAY || type->ident == iREFARRAY)
                error(137);
            else if (gTypes.find(type->semantic_tag())->isEnumStruct())
                error(136);
            else
                type->ident = iREFERENCE;
        }
    }

    // We're not getting another chance to do enum struct desugaring, since our
    // caller is not looking for a declaration. Do it now.
    if (!flags)
        rewrite_type_for_enum_struct(type);

    return TRUE;
}

static void
parse_post_array_dims(declinfo_t* decl, int flags)
{
    typeinfo_t* type = &decl->type;

    // Illegal declaration (we'll have a name since ref requires decl).
    if (type->ident == iREFERENCE)
        report(67) << decl->name;

    Parser parser;
    parser.parse_post_dims(type);

    // We can't deduce iARRAY vs iREFARRAY until the analysis phase. Start with
    // iARRAY for now.
    decl->type.ident = iARRAY;
    decl->type.has_postdims = TRUE;
}

static int
parse_old_decl(declinfo_t* decl, int flags)
{
    token_t tok;
    typeinfo_t* type = &decl->type;

    if (matchtoken(tCONST)) {
        if (type->is_const)
            error(138);
        type->is_const = true;
    }

    int tag, numtags = 0;
    if (flags & DECLFLAG_ARGUMENT) {
        if (matchtoken('&'))
            type->ident = iREFERENCE;

        // grammar for multitags is:
        //   multi-tag ::= '{' (symbol (',' symbol)*)? '}' ':'
        if (matchtoken('{')) {
            while (true) {
                int parsed_tag = 0;

                if (!matchtoken('_')) {
                    // If we don't get the magic tag '_', then we should have a symbol.
                    if (expecttoken(tSYMBOL, &tok))
                        parsed_tag = pc_addtag(tok.str);
                }
                tag = parsed_tag;
                numtags++;

                if (matchtoken('}'))
                    break;
                needtoken(',');
            }
            needtoken(':');
        }
        if (numtags > 1)
            error(158);
    }

    if (numtags == 0) {
        if (matchtoken2(tLABEL, &tok))
            tag = pc_addtag(tok.str);
        else
            tag = 0;
    }

    // All finished with tag stuff.
    type->tag = tag;
    type->declared_tag = type->tag;

    Type* type_obj = gTypes.find(type->tag);
    if (type_obj->isEnumStruct())
        error(85, type_obj->name());

    if (sc_require_newdecls)
        error(147);

    // Look for varargs and end early.
    if (matchtoken(tELLIPS)) {
        type->ident = iVARARGS;
        return TRUE;
    }

    if (flags & DECLMASK_NAMED_DECL) {
        if ((flags & DECLFLAG_MAYBE_FUNCTION) && matchtoken(tOPERATOR)) {
            decl->opertok = operatorname(&decl->name);
            if (decl->opertok == 0)
                decl->name = gAtoms.add("__unknown__");
        } else {
            if (!lexpeek(tSYMBOL)) {
                extern const char* sc_tokens[];
                switch (lextok(&tok)) {
                    case tOBJECT:
                    case tCHAR:
                    case tVOID:
                    case tINT:
                        if (lexpeek(tSYMBOL)) {
                            error(143);
                        } else {
                            error(157, sc_tokens[tok.id - tFIRST]);
                            decl->name = gAtoms.add(sc_tokens[tok.id - tFIRST]);
                        }
                        break;
                    default:
                        lexpush();
                        break;
                }
            }
            if (expecttoken(tSYMBOL, &tok))
                decl->name = gAtoms.add(tok.str);
            else if (!decl->name)
                decl->name = gAtoms.add("__unknown__");
        }
    }

    if ((flags & DECLMASK_NAMED_DECL) && !decl->opertok) {
        if (matchtoken('['))
            parse_post_array_dims(decl, flags);
    }

    return TRUE;
}

int
reparse_old_decl(declinfo_t* decl, int flags)
{
    bool is_const = decl->type.is_const;

    memset(decl, 0, sizeof(*decl));
    decl->type.ident = iVARIABLE;
    decl->type.is_const = is_const;

    return parse_old_decl(decl, flags);
}

static void
rewrite_type_for_enum_struct(typeinfo_t* info)
{
    Type* type = gTypes.find(info->declared_tag);
    symbol* enum_type = type->asEnumStruct();
    if (!enum_type)
        return;

    if (info->numdim >= sDIMEN_MAX) {
        error(86, type->name());
        return;
    }

    info->tag = 0;
    info->dim[info->numdim] = enum_type->addr();
    assert(info->declared_tag = enum_type->tag);

    // Note that the size here is incorrect. It's fixed up in initials() by
    // parse_var_decl. Unfortunately type->size is difficult to remove because
    // it can't be recomputed from array sizes (yet), in the case of
    // initializers with inconsistent final arrays. We could set it to
    // anything here, but we follow what parse_post_array_dims() does.
    info->numdim++;

    if (info->ident != iARRAY && info->ident != iREFARRAY) {
        info->ident = iARRAY;
        info->has_postdims = true;
    }
}

int
parse_new_decl(declinfo_t* decl, const token_t* first, int flags)
{
    token_t tok;

    if (!parse_new_typeexpr(&decl->type, first, flags))
        return FALSE;

    decl->type.is_new = TRUE;

    if (flags & DECLMASK_NAMED_DECL) {
        if ((flags & DECLFLAG_ARGUMENT) && matchtoken(tELLIPS)) {
            decl->type.ident = iVARARGS;
            return TRUE;
        }

        if ((flags & DECLFLAG_MAYBE_FUNCTION) && matchtoken(tOPERATOR)) {
            decl->opertok = operatorname(&decl->name);
            if (decl->opertok == 0)
                decl->name = gAtoms.add("__unknown__");
        } else {
            if (expecttoken(tSYMBOL, &tok))
                decl->name = gAtoms.add(tok.str);
            else
                decl->name = gAtoms.add("__unknown__");
        }
    }

    if (flags & DECLMASK_NAMED_DECL) {
        if (matchtoken('[')) {
            if (decl->type.numdim == 0)
                parse_post_array_dims(decl, flags);
            else
                error(121);
        }
    }

    rewrite_type_for_enum_struct(&decl->type);
    return TRUE;
}

int
reparse_new_decl(declinfo_t* decl, int flags)
{
    token_t tok;
    if (expecttoken(tSYMBOL, &tok))
        decl->name = gAtoms.add(tok.str);

    if (decl->type.declared_tag && !decl->type.tag) {
        assert(decl->type.numdim > 0);
        decl->type.numdim--;
    }

    decl->type.dim_exprs = nullptr;

    if (decl->type.has_postdims) {
        // We have something like:
        //    int x[], y...
        //
        // Reset the fact that we saw an array.
        decl->type.numdim = 0;
        decl->type.ident = iVARIABLE;
        decl->type.has_postdims = false;
        if (matchtoken('[')) {
            // int x[], y[]
            //           ^-- parse this
            parse_post_array_dims(decl, flags);
        }
    } else {
        if (matchtoken('[')) {
            if (decl->type.numdim > 0) {
                // int[] x, y[]
                //           ^-- not allowed
                error(121);
            }

            // int x, y[]
            //         ^-- parse this
            parse_post_array_dims(decl, flags);
        } else if (decl->type.numdim) {
            // int[] x, y
            //          ^-- still an array, because the type is int[]
            //
            // Dim count should be 0 but we zap it anyway.
            memset(decl->type.dim, 0, sizeof(decl->type.dim[0]) * decl->type.numdim);
        }
    }

    rewrite_type_for_enum_struct(&decl->type);
    return TRUE;
}

static void
fix_mispredicted_postdims(declinfo_t* decl)
{
    assert(decl->type.has_postdims);
    assert(decl->type.ident == iARRAY);

    decl->type.has_postdims = false;

    // We got a declaration like:
    //      int[3] x;
    //
    // This is illegal, so report it now, and strip dim_exprs.
    if (decl->type.dim_exprs) {
        for (int i = 0; i < decl->type.numdim; i++) {
            if (decl->type.dim_exprs[i]) {
                error(decl->type.dim_exprs[i]->pos(), 101);
                break;
            }
        }
        decl->type.dim_exprs = nullptr;
    }

    // If has_postdims is false, we never want to report an iARRAY.
    decl->type.ident = iREFARRAY;
}

// Parse a declaration.
//
// Grammar for named declarations is:
//    "const"? symbol ('[' ']')* '&'? symbol
//  | "const"? label? '&'? symbol '[' ']'
//
int
parse_decl(declinfo_t* decl, int flags)
{
    token_ident_t ident;

    memset(decl, 0, sizeof(*decl));

    decl->type.ident = iVARIABLE;

    // Match early varargs as old decl.
    if (lexpeek(tELLIPS))
        return parse_old_decl(decl, flags);

    // Must attempt to match const first, since it's a common prefix.
    if (matchtoken(tCONST))
        decl->type.is_const = true;

    // Sometimes we know ahead of time whether the declaration will be old, for
    // example, if preceded by tNEW or tDECL.
    if (flags & DECLFLAG_OLD)
        return parse_old_decl(decl, flags);
    if (flags & DECLFLAG_NEW)
        return parse_new_decl(decl, NULL, flags);

    // If parsing an argument, there are two simple checks for whether this is a
    // new or old-style declaration.
    if ((flags & DECLFLAG_ARGUMENT) && (lexpeek('&') || lexpeek('{')))
        return parse_old_decl(decl, flags);

    // Another dead giveaway is there being a label or typeless operator.
    if (lexpeek(tLABEL) || lexpeek(tOPERATOR))
        return parse_old_decl(decl, flags);

    // Otherwise, we have to eat a symbol to tell.
    if (matchsymbol(&ident)) {
        if (lexpeek(tSYMBOL) || lexpeek(tOPERATOR) || lexpeek('&') || lexpeek(tELLIPS)) {
            // A new-style declaration only allows array dims or a symbol name, so
            // this is a new-style declaration.
            return parse_new_decl(decl, &ident.tok, flags);
        }

        if ((flags & DECLMASK_NAMED_DECL) && matchtoken('[')) {
            // Oh no - we have to parse array dims before we can tell what kind of
            // declarator this is. It could be either:
            //    "x[] y" (new-style), or
            //    "y[],"  (old-style)
            parse_post_array_dims(decl, flags);

            if (matchtoken(tSYMBOL) || matchtoken('&')) {
                // This must be a newdecl, "x[] y" or "x[] &y", the latter of which
                // is illegal, but we flow it through the right path anyway.
                lexpush();
                fix_mispredicted_postdims(decl);
                return parse_new_decl(decl, &ident.tok, flags);
            }

            if (sc_require_newdecls)
                error(147);

            // The most basic - "x[]" and that's it. Well, we know it has no tag and
            // we know its name. We might as well just complete the entire decl.
            decl->name = ident.name;
            decl->type.tag = 0;
            return TRUE;
        }

        // Give the symbol back to the lexer. This is an old decl.
        lexpush();
        return parse_old_decl(decl, flags);
    }

    // All else has failed. Probably got a type keyword. New-style.
    return parse_new_decl(decl, NULL, flags);
}

void
check_void_decl(const typeinfo_t* type, int variable)
{
    if (type->tag != pc_tag_void)
        return;

    if (variable) {
        error(144);
        return;
    }

    if (type->numdim > 0) {
        error(145);
        return;
    }
}

void
check_void_decl(const declinfo_t* decl, int variable)
{
    check_void_decl(&decl->type, variable);
}

static void
make_primitive(typeinfo_t* type, int tag)
{
    memset(type, 0, sizeof(*type));
    type->tag = tag;
    type->ident = iVARIABLE;
}

symbol*
parse_inline_function(methodmap_t* map, const typeinfo_t* type, sp::Atom* name, int is_native,
                      int is_ctor, bool is_static)
{
    declinfo_t decl;
    memset(&decl, 0, sizeof(decl));

    if (is_ctor) {
        make_primitive(&decl.type, map->tag);
    } else {
        decl.type = *type;
    }
    decl.type.is_new = TRUE;

    const int* thistag = NULL;
    if (!is_ctor && !is_static)
        thistag = &map->tag;

    // Build a new symbol. Construct a temporary name including the class.
    auto fullname = ke::StringPrintf("%s.%s", map->name->chars(), name->chars());
    decl.name = gAtoms.add(fullname);

    ke::SaveAndSet<int> require_newdecls(&sc_require_newdecls, TRUE);

    int tokid = is_native ? tMETHODMAP : 0;
    symbol* target = Parser::ParseInlineFunction(tokid, decl, thistag);
    if (!target || target->forward) {
        error(10);
        return nullptr;
    }
    return target;
}

int
parse_property_accessor(const typeinfo_t* type, methodmap_t* map, methodmap_method_t* method)
{
    token_ident_t ident;
    int is_native = FALSE;

    needtoken(tPUBLIC);
    if (!matchsymbol(&ident)) {
        if (!matchtoken(tNATIVE)) {
            error(125);
            return FALSE;
        }
        is_native = TRUE;
        if (!needsymbol(&ident))
            return FALSE;
    }

    int getter = (strcmp(ident.name->chars(), "get") == 0);
    int setter = (strcmp(ident.name->chars(), "set") == 0);

    if (!getter && !setter) {
        error(125);
        return FALSE;
    }

    typeinfo_t voidtype;
    std::string tmpname = method->name->chars();
    if (getter)
        tmpname += ".get";
    else
        tmpname += ".set";
    auto full_atom = gAtoms.add(tmpname);

    const typeinfo_t* ret_type;
    if (getter) {
        ret_type = type;
    } else {
        make_primitive(&voidtype, pc_tag_void);
        ret_type = &voidtype;
    }

    symbol* target = parse_inline_function(map, ret_type, full_atom, is_native, FALSE, false);

    if (!target)
        return FALSE;

    if (getter && method->getter) {
        error(126, "getter", method->name);
        return FALSE;
    }
    if (setter && method->setter) {
        error(126, "setter", method->name);
        return FALSE;
    }

    auto& arglist = target->function()->args;

    if (getter) {
        method->getter = target;

        // Cannot have extra arguments.
        if (arglist[0].ident && arglist[1].ident)
            error(127);
    } else {
        method->setter = target;

        // Must have one extra argument taking the return type.
        arginfo* arg = &arglist[1];
        if (arg->ident != iVARIABLE || arg->def || arg->tag != type->tag) {
            error(150, pc_tagname(type->tag));
            return FALSE;
        }
        if (arglist[2].ident) {
            error(150, pc_tagname(type->tag));
            return FALSE;
        }
    }

    if (target->native)
        require_newline(TerminatorPolicy::Semicolon);
    else
        require_newline(TerminatorPolicy::Newline);
    return TRUE;
}

static std::unique_ptr<methodmap_method_t>
parse_property(methodmap_t* map)
{
    typeinfo_t type;
    token_ident_t ident;

    memset(&type, 0, sizeof(type));
    if (!parse_new_typeexpr(&type, NULL, 0))
        return NULL;
    if (type.numdim > 0)
        error(82);

    if (!needsymbol(&ident))
        return NULL;

    auto method = std::make_unique<methodmap_method_t>(map);
    method->name = ident.name;
    method->target = NULL;
    method->getter = NULL;
    method->setter = NULL;

    if (!needtoken('{'))
        return nullptr;

    while (!matchtoken('}')) {
        if (!parse_property_accessor(&type, map, method.get()))
            lexclr(TRUE);
    }

    require_newline(TerminatorPolicy::Newline);
    return method;
}

static std::unique_ptr<methodmap_method_t>
parse_method(methodmap_t* map)
{
    int maybe_ctor = 0;
    int is_ctor = 0;
    int is_native = 0;
    bool is_static = false;
    const char* spectype = layout_spec_name(map->spec);

    if (matchtoken(tSTATIC))
        is_static = true;

    // This stores the name of the method (for destructors, we add a ~).
    token_ident_t ident = {};

    typeinfo_t type;
    memset(&type, 0, sizeof(type));

    // Destructors cannot be static.
    int got_symbol;

    is_native = matchtoken(tNATIVE);
    got_symbol = matchsymbol(&ident);

    if (matchtoken('~'))
        error(118);

    if (got_symbol && matchtoken('(')) {
        // ::= ident '('

        // Push the '(' token back for declargs().
        maybe_ctor = TRUE;
        lexpush();
    } else {
        // The first token of the type expression is either the symbol we
        // predictively parsed earlier, or it's been pushed back into the
        // lex buffer.
        const token_t* first = got_symbol ? &ident.tok : nullptr;

        // Parse for type expression, priming it with the token we predicted
        // would be an identifier.
        if (!parse_new_typeexpr(&type, first, 0))
            return nullptr;

        // Now, we should get an identifier.
        if (!needsymbol(&ident))
            return nullptr;

        // If the identifier is a constructor, error, since the user specified
        // a type.
        if (ident.name == map->name)
            error(99, "constructor");
    }

    // Do some preliminary verification of ctor names.
    if (maybe_ctor) {
        if (ident.name == map->name)
            is_ctor = TRUE;
        else
            report(114) << "constructor" << spectype << map->name;
    }

    if (is_ctor && is_static) {
        // Constructors may not be static.
        error(175);
    }

    // Natives will hit a similar error on their own, so we only check numdim
    // if we're not parsing a native.
    if (!is_native && type.numdim > 0)
        error(83);

    symbol* target = parse_inline_function(map, &type, ident.name, is_native, is_ctor, is_static);

    if (!target)
        return nullptr;

    auto method = std::make_unique<methodmap_method_t>(map);
    method->name = ident.name;
    method->target = target;
    method->is_static = is_static;

    // If the symbol is a constructor, we bypass the initial argument checks.
    if (is_ctor) {
        if (map->ctor)
            report(113) << map->name;

        map->ctor = method.get();
    }

    if (target->native)
        require_newline(TerminatorPolicy::Semicolon);
    else
        require_newline(TerminatorPolicy::Newline);
    return method;
}

/**
 * domethodmap - declare a method map for OO-ish syntax.
 *
 */
void
domethodmap(LayoutSpec spec)
{
    token_ident_t ident;
    methodmap_t* parent = NULL;
    const char* spectype = layout_spec_name(spec);

    // methodmap ::= "methodmap" symbol ("<" symbol)? "{" methodmap-body "}"
    sp::Atom* mapname;
    if (needsymbol(&ident)) {
        if (!isupper(*ident.name->chars()))
            error(109, spectype);
        mapname = ident.name;
    } else {
        static int unknown_methodmap_num = 0;
        auto tempname = ke::StringPrintf("methodmap_%d", ++unknown_methodmap_num);
        mapname = gAtoms.add(tempname);
    }

    LayoutSpec old_spec = deduce_layout_spec_by_name(mapname->chars());
    bool can_redef = can_redef_layout_spec(spec, old_spec);
    if (!can_redef)
        report(110) << mapname << layout_spec_name(old_spec);

    int old_nullable = matchtoken(tNULLABLE);

    if (matchtoken('<') && needsymbol(&ident)) {
        if ((parent = methodmap_find_by_name(ident.name)) == NULL) {
            report(102) << spectype << ident.name;
        } else if (parent->spec != spec) {
            error(129);
        }
    }

    methodmap_t* map = methodmap_add(parent, spec, mapname);

    if (old_nullable)
        map->keyword_nullable = old_nullable;

    declare_methodmap_symbol(map, can_redef);

    needtoken('{');
    while (!matchtoken('}')) {
        token_t tok;
        std::unique_ptr<methodmap_method_t> method;

        if (lextok(&tok) == tPUBLIC) {
            method = parse_method(map);
        } else if (tok.id == tSYMBOL && strcmp(tok.str, "property") == 0) {
            auto pos = current_pos();
            method = parse_property(map);
            if (method && !(method->getter || method->setter)) {
                error(pos, 57, method->name);
                continue;
            }
        } else {
            error(124);
        }

        if (method) {
            // Check that a method with this name doesn't already exist.
            for (const auto& other : map->methods) {
                if (other->name == method->name) {
                    error(103, method->name, spectype);
                    method = nullptr;
                    break;
                }
            }
        }

        if (!method) {
            if (!consume_line())
                return;
            continue;
        }

        map->methods.push_back(std::move(method));
    }

    require_newline(TerminatorPolicy::NewlineOrSemicolon);
}

/**
 * function-type ::= "(" function-type-inner ")"
 *                 | function-type-inner
 * function-type-inner ::= "function" type-expr "(" new-style-args ")"
 */
functag_t*
parse_function_type()
{
    int lparen = matchtoken('(');
    needtoken(tFUNCTION);

    functag_t* type = new functag_t;

    parse_new_typename(NULL, &type->ret_tag);

    needtoken('(');

    while (!matchtoken(')')) {
        declinfo_t decl = {};
        decl.type.ident = iVARIABLE;

        parse_new_decl(&decl, nullptr, DECLFLAG_ARGUMENT);

        if (decl.type.ident == iARRAY) {
            SemaContext sc;
            ResolveArraySize(sc, current_pos(), &decl.type, sARGUMENT);
        }

        // Eat optional symbol name.
        matchtoken(tSYMBOL);

        // Error once when we're past max args.
        if (type->args.size() == SP_MAX_EXEC_PARAMS) {
            error(45);
            continue;
        }

        funcarg_t arg;
        arg.tag = decl.type.tag;
        arg.dimcount = decl.type.numdim;
        memcpy(arg.dims, decl.type.dim, arg.dimcount * sizeof(decl.type.dim[0]));
        arg.fconst = decl.type.is_const;
        if (decl.type.ident == iARRAY)
            arg.ident = iREFARRAY;
        else
            arg.ident = decl.type.ident;
        if (arg.ident != iREFARRAY && arg.ident != iARRAY)
            assert(arg.dimcount == 0);
        type->args.push_back(arg);

        if (!matchtoken(',')) {
            needtoken(')');
            break;
        }
    }

    if (lparen)
        needtoken(')');

    require_newline(TerminatorPolicy::Semicolon);
    errorset(sRESET, 0);
    return type;
}

/*
 *  Finds a function in the global symbol table or creates a new entry.
 *  It does some basic processing and error checking.
 */
symbol*
fetchfunc(const char* name)
{
    symbol* sym;

    if ((sym = findglb(name)) != 0) { /* already in symbol table? */
        if (sym->ident != iFUNCTN) {
            error(21, name); /* yes, but not as a function */
            return NULL;     /* make sure the old symbol is not damaged */
        } else if (sym->native) {
            error(21, name); /* yes, and it is a native */
        }
        assert(sym->vclass == sGLOBAL);
    } else {
        /* don't set the "uDEFINE" flag; it may be a prototype */
        sym = addsym(name, code_idx, iFUNCTN, sGLOBAL, 0);
        assert(sym != NULL); /* fatal error 103 must be given on error */
    }
    if (pc_deprecate.size() > 0) {
        assert(sym != NULL);
        sym->deprecated = true;
        if (sc_status == statWRITE) {
            sym->documentation = std::move(pc_deprecate);
        } else {
            pc_deprecate = "";
        }
    }

    return sym;
}

static int
operatorname(sp::Atom** name)
{
    cell val;
    const char* str;

    /* check the operator */
    int opertok = lex(&val, &str);
    switch (opertok) {
        case '+':
        case '-':
        case '*':
        case '/':
        case '%':
        case '>':
        case '<':
        case '!':
        case '~':
        case '=':
        {
            char str[] = {(char)opertok, '\0'};
            *name = gAtoms.add(str);
            break;
        }
        case tINC:
            *name = gAtoms.add("++");
            break;
        case tDEC:
            *name = gAtoms.add("--");
            break;
        case tlEQ:
            *name = gAtoms.add("==");
            break;
        case tlNE:
            *name = gAtoms.add("!=");
            break;
        case tlLE:
            *name = gAtoms.add("<=");
            break;
        case tlGE:
            *name = gAtoms.add(">=");
            break;
        default:
            *name = gAtoms.add("");
            error(7); /* operator cannot be redefined (or bad operator name) */
            return 0;
    }

    return opertok;
}

int
check_operatortag(int opertok, int resulttag, const char* opername)
{
    assert(opername != NULL && strlen(opername) > 0);
    switch (opertok) {
        case '!':
        case '<':
        case '>':
        case tlEQ:
        case tlNE:
        case tlLE:
        case tlGE:
            if (resulttag != pc_tag_bool) {
                error(63, opername, "bool:"); /* operator X requires a "bool:" result tag */
                return FALSE;
            }
            break;
        case '~':
            if (resulttag != 0) {
                error(63, opername, "_:"); /* operator "~" requires a "_:" result tag */
                return FALSE;
            }
            break;
    }
    return TRUE;
}

static int
parse_funcname(const char* fname, int* tag1, int* tag2, char* opname, size_t opname_len)
{
    const char* ptr;
    int unary;

    /* tags are only positive, so if the function name starts with a '-',
     * the operator is an unary '-' or '--' operator.
     */
    if (*fname == '-') {
        *tag1 = 0;
        unary = TRUE;
        ptr = fname;
    } else {
        *tag1 = (int)strtol(fname, (char**)&ptr, 16);
        unary = ptr == fname; /* unary operator if it doesn't start with a tag name */
    }
    assert(!unary || *tag1 == 0);
    assert(*ptr != '\0');
    size_t chars_to_copy = 0;
    for (const char* iter = ptr; *iter && !isdigit(*iter); iter++)
        chars_to_copy++;
    ke::SafeStrcpyN(opname, opname_len, ptr, chars_to_copy);
    *tag2 = (int)strtol(&ptr[chars_to_copy], NULL, 16);
    return unary;
}

std::string
funcdisplayname(const char* funcname)
{
    int tags[2];
    char opname[10];
    int unary;

    if (isalpha(*funcname) || *funcname == '_' || *funcname == PUBLIC_CHAR || *funcname == '\0')
        return funcname;

    unary = parse_funcname(funcname, &tags[0], &tags[1], opname, sizeof(opname));
    Type* rhsType = gTypes.find(tags[1]);
    assert(rhsType != NULL);
    if (unary) {
        return ke::StringPrintf("operator%s(%s:)", opname, rhsType->name());
    }

    Type* lhsType = gTypes.find(tags[0]);
    assert(lhsType != NULL);
    /* special case: the assignment operator has the return value as the 2nd tag */
    if (opname[0] == '=' && opname[1] == '\0')
        return ke::StringPrintf("%s:operator%s(%s:)", lhsType->name(), opname, rhsType->name());
    return ke::StringPrintf("operator%s(%s:,%s:)", opname, lhsType->name(), rhsType->name());
}

int
argcompare(arginfo* a1, arginfo* a2)
{
    int result, level;

    result = 1;
    if (result)
        result = a1->ident == a2->ident; /* type/class */
    if (result)
        result = a1->is_const == a2->is_const; /* "const" flag */
    if (result)
        result = a1->tag == a2->tag;
    if (result)
        result = a1->numdim == a2->numdim; /* array dimensions & index tags */
    if (result)
        result = a1->enum_struct_tag == a2->enum_struct_tag;
    for (level = 0; result && level < a1->numdim; level++)
        result = a1->dim[level] == a2->dim[level];
    if (result)
        result = !!a1->def == !!a2->def; /* availability of default value */
    if (a1->def) {
        if (a1->ident == iREFARRAY) {
            if (result)
                result = !!a1->def->array == !!a2->def->array;
            if (result && a1->def->array)
                result = a1->def->array->total_size() == a2->def->array->total_size();
            /* ??? should also check contents of the default array (these troubles
             * go away in a 2-pass compiler that forbids double declarations, but
             * Pawn currently does not forbid them) */
        } else {
            if (result)
                result = a1->def->val.isValid() == a2->def->val.isValid();
            if (result && a1->def->val)
                result = a1->def->val.get() == a2->def->val.get();
        }
        if (result)
            result = a1->def->tag == a2->def->tag;
    }
    return result;
}

void
fill_arg_defvalue(VarDecl* decl, arginfo* arg)
{
    arg->def = std::make_unique<DefaultArg>();
    arg->def->tag = decl->type().tag;

    if (SymbolExpr* expr = decl->init_rhs()->AsSymbolExpr()) {
        symbol* sym = expr->sym();
        assert(sym->vclass == sGLOBAL);

        arg->def->val = ke::Some(sym->addr());
        arg->tag = sym->tag;
        if (sc_status == statWRITE && (sym->usage & uREAD) == 0)
            markusage(sym, uREAD);
        return;
    }

    ArrayData data;
    BuildArrayInitializer(decl, &data, 0);

    arg->def->array = new ArrayData;
    *arg->def->array = std::move(data);
}

static inline bool
is_symbol_unused(symbol* sym)
{
    if (sym->parent())
        return false;
    if (!sym->is_unreferenced())
        return false;
    if (sym->is_public)
        return false;
    if (sym->ident == iVARIABLE || sym->ident == iARRAY)
        return true;
    return sym->ident == iFUNCTN && !sym->native &&
           strcmp(sym->name(), uMAINFUNC) != 0;
}

static void
reduce_referrers(symbol* root)
{
    std::vector<symbol*> work;

    // Enqueue all unreferred symbols.
    for (symbol* sym = root->next; sym; sym = sym->next) {
        if (is_symbol_unused(sym)) {
            sym->queued = true;
            work.push_back(sym);
        }
    }

    while (!work.empty()) {
        symbol* dead = ke::PopBack(&work);
        dead->usage &= ~(uREAD | uWRITTEN);

        for (symbol* sym : dead->refers_to()) {
            sym->drop_reference_from(dead);
            if (is_symbol_unused(sym) && !sym->queued) {
                // During compilation, anything marked as stock will be omitted from
                // the final binary *without warning*. If a stock calls a non-stock
                // function, we want to avoid warnings on that function as well, so
                // we propagate the stock bit.
                if (dead->stock)
                    sym->stock = true;

                sym->queued = true;
                work.push_back(sym);
            }
        }
    }
}

// Determine the set of live functions. Note that this must run before delete_symbols,
// since that resets referrer lists.
static void
deduce_liveness(symbol* root)
{
    std::vector<symbol*> work;

    // The root set is all public functions.
    for (symbol* sym = root->next; sym; sym = sym->next) {
        if (sym->ident != iFUNCTN)
            continue;
        if (sym->native)
            continue;

        if (sym->is_public) {
            sym->queued = true;
            work.push_back(sym);
        } else {
            sym->queued = false;
        }
    }

    // Traverse referrers to find the transitive set of live functions.
    while (!work.empty()) {
        symbol* live = ke::PopBack(&work);

        for (const auto& other : live->refers_to()) {
            if (other->ident != iFUNCTN || other->queued)
                continue;
            other->queued = true;
            work.push_back(other);
        }
    }

    // Remove the liveness flags for anything we did not visit.
    for (symbol* sym = root->next; sym; sym = sym->next) {
        if (sym->ident != iFUNCTN || sym->queued)
            continue;
        if (sym->native)
            continue;
        sym->usage &= ~(uWRITTEN | uREAD);
    }
}

static constvalue*
insert_constval(constvalue* prev, constvalue* next, sp::Atom* name, cell val, int index)
{
    constvalue* cur;

    if ((cur = (constvalue*)malloc(sizeof(constvalue))) == NULL)
        error(FATAL_ERROR_OOM); /* insufficient memory (fatal error) */
    memset(cur, 0, sizeof(constvalue));
    cur->name = name;
    cur->value = val;
    cur->index = index;
    cur->next = next;
    prev->next = cur;
    return cur;
}

constvalue*
append_constval(constvalue* table, sp::Atom* name, cell val, int index)
{
    constvalue *cur, *prev;

    /* find the end of the constant table */
    for (prev = table, cur = table->next; cur != NULL; prev = cur, cur = cur->next)
        /* nothing */;
    return insert_constval(prev, nullptr, name, val, index);
}

void
delete_consttable(constvalue* table)
{
    constvalue *cur = table->next, *next;

    while (cur != NULL) {
        next = cur->next;
        free(cur);
        cur = next;
    }
    memset(table, 0, sizeof(constvalue));
}

/*  add_constant
 *
 *  Adds a symbol to the symbol table. Returns NULL on failure.
 */
symbol*
add_constant(const char* name, cell val, int vclass, int tag)
{
    symbol* sym;

    /* Test whether a global or local symbol with the same name exists. Since
     * constants are stored in the symbols table, this also finds previously
     * defind constants. */
    sym = findglb(name);
    if (!sym)
        sym = findloc(name);
    if (sym) {
        int redef = 0;
        if (sym->ident != iCONSTEXPR)
            redef = 1; /* redefinition a function/variable to a constant is not allowed */
        if (sym->enumfield) {
            /* enum field, special case if it has a different tag and the new symbol is also an enum field */
            symbol* tagsym;
            if (sym->tag == tag)
                redef = 1; /* enumeration field is redefined (same tag) */
            Type* type = gTypes.find(tag);
            if (type == NULL) {
                redef = 1; /* new constant does not have a tag */
            } else {
                tagsym = findconst(type->name());
                if (tagsym == NULL || !tagsym->enumroot)
                    redef = 1; /* new constant is not an enumeration field */
            }
            /* in this particular case (enumeration field that is part of a different
             * enum, and non-conflicting with plain constants) we want to be able to
             * redefine it
             */
            if (!redef)
                goto redef_enumfield;
        } else if (sym->tag != tag) {
            redef = 1; /* redefinition of a constant (non-enum) to a different tag is not allowed */
        }
        if (redef) {
            error(21, name); /* symbol already defined */
            return NULL;
        } else if (sym->addr() != val) {
            error(201, name);  /* redefinition of constant (different value) */
            sym->setAddr(val); /* set new value */
        }
        /* silently ignore redefinitions of constants with the same value & tag */
        return sym;
    }

    /* constant doesn't exist yet (or is allowed to be redefined) */
redef_enumfield:
    sym = addsym(name, val, iCONSTEXPR, vclass, tag);
    sym->defined = true;
    if (sc_status == statIDLE)
        sym->predefined = true;
    return sym;
}

/*  exprconst
 */
bool
exprconst(cell* val, int* tag, symbol** symptr)
{
    int ident;
    AutoCountErrors errors;

    bool failed;
    {
        AutoStage stage;
        errorset(sEXPRMARK, 0);
        ident = expression(val, tag, symptr, nullptr);
        failed = (sc_status == statWRITE && !errors.ok());
        stage.Rewind();
    }

    if (ident != iCONSTEXPR) {
        if (!failed) // Don't pile on errors.
            error(8); /* must be constant expression */
        if (val != NULL)
            *val = 0;
        if (tag != NULL)
            *tag = 0;
        if (symptr != NULL)
            *symptr = NULL;
    }
    errorset(sEXPRRELEASE, 0);
    return !failed && (ident == iCONSTEXPR);
}
