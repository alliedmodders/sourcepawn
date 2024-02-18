/* vim: set sts=4 ts=8 sw=4 tw=99 et: */
//  Copyright (c) AlliedModders LLC 2022
//  Copyright (c) ITB CompuPhase 1997-2006
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
#include <amtl/experimental/am-argparser.h>

#include <filesystem>

#include "compile-options.h"
#include "errors.h"
#include "sc.h"

#if defined _WIN32
# include <Windows.h>
# include <direct.h>
#else
# include <unistd.h>
#endif

using namespace ke;
using namespace sp;
using namespace sp::cc;

namespace fs = std::filesystem;

#if defined _WIN32
static HWND hwndFinish = 0;
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

/* set_extension
 * Set the default extension, or force an extension. To erase the
 * extension of a filename, set "extension" to an empty string.
 */
static void set_extension(std::string* filename, const char* extension, bool force) {
    assert(extension != NULL && (*extension == '\0' || *extension == '.'));
    assert(filename != NULL);

    fs::path path(*filename);
    if (force && !path.has_extension())
        *filename = path.stem().string();
    if (force || path.has_extension())
        *filename = fs::path(*filename).replace_extension(extension).string();
}

static void Usage(CompileContext& cc, args::Parser& parser, int argc, char** argv)
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
    if (fs::path::preferred_separator != '/') {
        parser.allow_slashes();
    }

    parser.add_usage_line("sym=val", "Define macro \"sym\" with value \"val\".");
    parser.add_usage_line("sym=", "Define macro \"sym\" with value 0.");

    auto usage = "[options] <filename> [filename...]";
    parser.set_usage_line(usage);

    if (!parser.parse(argc, argv)) {
        Usage(cc, parser, argc, argv);
    }

    cc.options()->syntax_only = opt_syntax_only.value();
    cc.options()->need_semicolon = opt_semicolons.value();
    cc.options()->tabsize = opt_tabsize.value();
    cc.options()->warnings_are_errors = opt_warnings_as_errors.value();
    cc.options()->use_stderr = opt_stderr.value();
    cc.options()->compression = opt_compression.value();
    cc.options()->show_includes = opt_showincludes.value();

    if (opt_no_verify.value())
        cc.set_verify_output(false);

    if (opt_prefixfile.hasValue())
        cc.set_default_include(opt_prefixfile.value());

    if (opt_outputfile.hasValue())
        cc.set_outfname(opt_outputfile.value());

    if (opt_verbosity.hasValue()) {
        if (isdigit(*opt_verbosity.value().c_str()))
            cc.options()->verbosity = atoi(opt_verbosity.value().c_str());
        else
            cc.options()->verbosity = 2;
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
        size_t pos;
        if (option[0] == '@') {
            fprintf(stderr, "Response files (@ prefix) are no longer supported.");
            exit(1);
        } else if ((pos = option.find('=')) != std::string::npos) {
            std::string key = option.substr(0, pos);
            std::string value = option.substr(pos + 1);
            cc.options()->predefines.emplace_back(std::move(key), std::move(value));
        } else {
            cc.options()->source_files.emplace_back(option);

            /* The output name is the first input name with a different extension,
             * but it is stored in a different directory
             */
            if (cc.outfname().empty()) {
                fs::path out_path(option);
                cc.set_outfname(out_path.filename().string());
                set_extension(&cc.outfname(), ".smx", true);
            }
        }
    }

    if (cc.options()->source_files.empty())
        Usage(cc, parser, argc, argv);
}


int main(int argc, char** argv) {
    CompileContext cc;

    parseoptions(cc, argc, argv);

    int rv = RunCompiler(argc, argv, cc);

#if defined __WIN32__ || defined _WIN32 || defined _Windows
    if (IsWindow(hwndFinish))
        PostMessageA(hwndFinish, RegisterWindowMessageA("PawnNotify"), rv, 0L);
#endif

    return rv;
}
