// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2022-2026 AlliedModders LLC
//
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
