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
#include <vector>

class CompileContext;
struct symbol;

class CodegenContext
{
  public:
    explicit CodegenContext(CompileContext& cc, symbol* func)
      : cc_(cc),
        func_(func)
    {}

    void AddDebugFile(const std::string& line);
    void AddDebugLine(int linenr);
    void AddDebugSymbol(symbol* sym);

    CompileContext& cc() { return cc_; }
    symbol* func() const { return func_; }
    const std::vector<std::string>& debug_strings() const { return debug_strings_; }

  private:
    CompileContext& cc_;
    symbol* func_;

    std::vector<std::string> debug_strings_;
};
