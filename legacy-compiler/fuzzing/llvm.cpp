// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
//  Copyright (c) AlliedModders 2022
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
#include <stddef.h>
#include <stdint.h>

#include <filesystem>

#include <amtl/experimental/am-argparser.h>

#include "compile-options.h"
#include "sc.h"

using namespace ke;
namespace fs = std::filesystem;

extern "C" int LLVMFuzzerRunDriver(int *argc, char ***argv,
                                   int (*UserCb)(const uint8_t *Data, size_t Size));

args::RepeatOption<std::string> opt_includes("-i", "--include", "Path for include files");

std::string prog_name;

int FuzzCompiler(const uint8_t* data, size_t size) {
    auto& cc = CompileContext::get();

    auto tmp = fs::temp_directory_path() / "XXXXXX";

    int argc = 1;
    char* argv = prog_name.data();
    return RunCompiler(argc, &argv, cc);
}

int main(int argc, char** argv) {
    args::Parser parser;
    parser.enable_inline_values();
    parser.allow_passthrough_args();

    auto usage = "[options] [-- llvm-fuzzer options]";
    parser.set_usage_line(usage);

    if (!parser.parse(argc, argv)) {
        setcaption();
        parser.usage(stdout, argc, argv);
        exit(1);
    }

    CompileContext cc;

    for (const auto& inc_path : opt_includes.values()) {
        std::string str = inc_path;

        if (str.empty())
            continue;
        if (str.back() != DIRSEP_CHAR)
            str.push_back(DIRSEP_CHAR);

        cc.options()->include_paths.emplace_back(str);
    }

    int sub_argc = 1;
    std::vector<char*> sub_argv = {argv[0]};
    for (auto& arg : parser.passthrough_args()) {
        sub_argc++;
        sub_argv.emplace_back(arg.data());
    }
    char** sub_argv_ptr = sub_argv.data();

    prog_name = argv[0];

    return LLVMFuzzerRunDriver(&sub_argc, &sub_argv_ptr, FuzzCompiler);
}
