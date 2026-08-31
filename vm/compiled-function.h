// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2006-2026 AlliedModders LLC
//
#pragma once

#include <memory>

#include <amtl/am-fixedarray.h>
#include <amtl/am-refcounting.h>
#include <sp_vm_types.h>
#include "code-allocator.h"
#include "linking.h"

namespace sp {

using namespace ke;

class PluginRuntime;

struct LoopEdge {
    // Offset to the patchable jump instruction, such that (base + offset - 4)
    // yields a patchable location.
    uint32_t offset;
    // The displacement to either the timeout routine or the original
    // displacement, depending on the timeout state.
    int32_t disp32;
};

struct CipMapEntry {
    // Offset from the first cip of the function.
    uint32_t cipoffs;
    // Offset from the first pc of the function.
    uint32_t pcoffs;
};

static const ucell_t kInvalidCip = 0xffffffff;

class CompiledFunction
{
  public:
    CompiledFunction(const LinkedCode& code, FixedArray<LoopEdge>* edges,
                     FixedArray<CipMapEntry>* cip_map);
    ~CompiledFunction();

  public:
    void* GetEntryAddress() const {
        return code_.entry;
    }
    size_t GetCodeSize() const {
        return code_.code_size();
    }
    uint32_t NumLoopEdges() const {
        return edges_->size();
    }
    LoopEdge& GetLoopEdge(size_t i) {
        return edges_->at(i);
    }

    ucell_t FindCipByPc(void* pc);

    static size_t offsetOfEntry() {
        return offsetof(CompiledFunction, code_) + offsetof(LinkedCode, entry);
    }

  private:
    LinkedCode code_;
    std::unique_ptr<FixedArray<LoopEdge>> edges_;
    std::unique_ptr<FixedArray<CipMapEntry>> cip_map_;
    bool cip_map_sorted_;
};

} // namespace sp
