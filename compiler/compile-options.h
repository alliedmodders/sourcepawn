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

#include <string>
#include <utility>
#include <vector>

#define CTRL_CHAR '\\'  /* default control character */

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
    bool syntax_only = false;
    int verbosity = 1;             /* verbosity level, 0=quiet, 1=normal, 2=verbose */
    std::vector<std::pair<std::string, int>> constants;
};
