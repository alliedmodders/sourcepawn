// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
// Copyright (c) ITB CompuPhase, 1997-2006
//
#pragma once

#include <vector>

#include "code-generator.h"
#include "compile-context.h"
#include "libsmx/data-pool.h"
#include "libsmx/smx-builder.h"
#include "libsmx/smx-encoding.h"
#include "sc.h"
#include "utils/byte-buffer.h"
#include "utils/string-pool.h"

namespace sp {
namespace cc {

bool assemble(CompileContext& cc, CodeGenerator& cg, const char* outname,
              int compression_level);

class Assembler
{
  public:
    explicit Assembler(CompileContext& cc, CodeGenerator& cg);

    void Assemble(sp::SmxByteBuffer* buffer);

  private:
    CompileContext& cc_;
    CodeGenerator& cg_;
};

} // namespace cc
} // namespace sp
