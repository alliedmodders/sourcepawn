// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2023-2026 AlliedModders LLC

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
    buffer_ += "#define __sourcepawn_intrinsic_float\n";
    buffer_ += "#define __sourcepawn2\n";
    buffer_ += "const int EOS = 0;\n";
    buffer_ += "const int cellmax = " + std::to_string(INT_MAX) + ";\n";
    buffer_ += "const int cellmin = " + std::to_string(INT_MIN) + ";\n";
    buffer_ += "builtin float float(int n);\n";
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
