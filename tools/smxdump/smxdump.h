// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
//
#pragma once

#include <string>
#include <string_view>
#include <memory>
#include <sp_vm_types.h>
#include "vm/legacy/opcodes.h"
#include "vm/v2/opcodes.h"

#include <amtl/experimental/am-argparser.h>

namespace sp {
class SmxImage;
struct smx_rtti_method;
class TypeDesc;

namespace debug {
class FastRtti;
}
using debug::FastRtti;

namespace v2 {
class Runtime;
}
}

extern ke::args::ToggleOption show_lowered;
extern ke::args::ToggleOption show_jit;

class DumpTool final {
  public:
    explicit DumpTool(const char* file, std::unique_ptr<sp::SmxImage> smx, sp::v2::Runtime* runtime = nullptr);
    ~DumpTool();

    void Dump();
    sp::SmxImage* smx() const;

  private:
    void DumpHeaders();
    void DumpPublics();
    void DumpNatives();
    void DumpPubvars();
    void DumpData();
    void DumpCode();
    void DumpRttiMethods();
    void DumpSignature(uint32_t offset);
    void DumpLocals(const sp::smx_rtti_method* method);
    std::string DumpType(sp::FastRtti& rtti);
    std::string DumpType(const sp::TypeDesc* td);
    void DumpRttiEnums();
    const char* GetClassdefPrefix(uint32_t flags);
    void DumpRttiClassdefs();
    void DumpUpvars(sp::FastRtti& rtti, uint16_t count);
    void DumpRttiGlobals();
    void DumpLegacyCode();
    void DumpLoweredCode(uint32_t method_index);
    void DumpJitCode(uint32_t method_index);

    template <bool SearchForMethods>
    void DumpCodeRangeV1(cell_t pcode_start, cell_t pcode_end);

    void DumpOpcodeV1(const cell_t* method_start, const cell_t* cip, sp::v1::OPCODE op);
    void DumpCodeRangeV2(uint32_t pcode_start, uint32_t pcode_end);
    std::string EscapeString(std::string_view s);
    std::string DumpString(uint16_t index);
    void DumpOpcodeV2(const uint8_t* method_start, const uint8_t* cip, sp::v2::OPCODE op);

  private:
    std::unique_ptr<sp::SmxImage> smx_;
    std::string file_;
    sp::v2::Runtime* runtime_;
};
