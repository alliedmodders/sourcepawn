// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
//  Copyright (c) 2023 AlliedModders LLC
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

#include "builtin-generator.h"

#include "compile-context.h"

namespace sp {
namespace cc {

BuiltinGenerator::BuiltinGenerator(CompileContext& cc)
  : cc_(cc)
{}

void BuiltinGenerator::AddDefine(const std::string& key, const std::string& value) {
    buffer_ += "#define ";
    buffer_ += key;
    buffer_ += " ";
    buffer_ += value;
    buffer_ += "\n";
}

void BuiltinGenerator::AddBuiltinConstants() {
    buffer_ += "const int EOS = 0;\n";
    buffer_ += "const int cellmax = " + std::to_string(INT_MAX) + ";\n";
    buffer_ += "const int cellmin = " + std::to_string(INT_MIN) + ";\n";
}

void BuiltinGenerator::AddDefaultInclude() {
    if (cc_.default_include().empty())
        return;
    buffer_ += "#tryinclude <" + cc_.default_include() + ">\n";
}

std::shared_ptr<SourceFile> BuiltinGenerator::Generate(const std::string& name) {
    return cc_.sources()->Open(name, std::move(buffer_));
}

} // namespace cc
} // namespace sp
