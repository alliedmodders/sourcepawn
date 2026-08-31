// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2021-2026 AlliedModders LLC
// Copyright (c) ITB CompuPhase, 1997-2006
//
#pragma once

#include <string>
#include <utility>
#include <vector>

#define CTRL_CHAR '\\'  /* default control character */

namespace sp {
namespace cc {

struct CompileOptions {
    bool need_semicolon = false;
    std::vector<std::string> source_files;
    std::vector<std::string> include_paths;
    int tabsize = 8;
    bool require_newdecls = false;
    bool warnings_are_errors = false;
    bool use_stderr = false;
    int pragma_dynamic = 0;
    int ctrlchar_org = CTRL_CHAR;
    int compression = 9;
    bool show_includes = false;
    bool print_ast = false;
    bool syntax_only = false;
    int verbosity = 1;             /* verbosity level, 0=quiet, 1=normal, 2=verbose */
    bool sema_only = false;
    std::vector<std::pair<std::string, std::string>> predefines;
};

} // namespace cc
} // namespace sp
