// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2023-2026 AlliedModders LLC

#pragma once

#include "source-file.h"

namespace sp {
namespace cc {

class CompileContext;

class BuiltinGenerator final {
  public:
    explicit BuiltinGenerator(CompileContext& cc);

    void AddDefine(const std::string& key, const std::string& value);

    void AddBuiltinConstants();
    void AddDefaultInclude();

    std::shared_ptr<SourceFile> Generate(const std::string& name);

  private:
    CompileContext& cc_;
    tr::string buffer_;
};

} // namespace cc
} // namespace sp
