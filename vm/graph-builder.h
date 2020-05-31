// vim: set sts=2 ts=8 sw=2 tw=99 et:
// 
// Copyright (C) 2006-2015 AlliedModders LLC
// 
// This file is part of SourcePawn. SourcePawn is free software: you can
// redistribute it and/or modify it under the terms of the GNU General Public
// License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
//
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.
//

#ifndef _include_sourcepawn_vm_graph_builder_h_
#define _include_sourcepawn_vm_graph_builder_h_

#include <assert.h>

#include <memory>

#include <amtl/am-fixedarray.h>
#include <amtl/am-hashmap.h>
#include <amtl/am-refcounting.h>
#include <amtl/am-string.h>
#include <smx/smx-v1-opcodes.h>
#include <sp_vm_types.h>
#include "control-flow.h"
#include "bitset.h"

namespace sp {

class PluginRuntime;

class GraphBuilder
{
 public:
  GraphBuilder(PluginRuntime* rt, uint32_t start_offset);

  ke::RefPtr<ControlFlowGraph> build();

  int error_code() const {
    return error_code_;
  }

 private:
  bool prescan();
  bool prescan_jump_target(OPCODE op, cell_t target);
  bool prescan_casetable(const uint8_t* pos, cell_t size);
  bool error(int code);

  enum class FlowState {
    // Continue processing opcodes in the current block.
    Continue,
    // Do not process any more opcodes in the current block.
    Ended,
    // An error occurred, abort.
    Error
  };

  bool scan();
  FlowState scanFlow();
  FlowState scanSwitchFlow(const uint8_t* insn);
  ke::RefPtr<Block> getOrAddBlock(const uint8_t* cip);
  void enqueueBlock(Block* block);

  bool cleanup();

  bool more() {
    return cip_ + sizeof(cell_t) <= stop_at_;
  }
  cell_t peek() {
    assert(more());
    return *reinterpret_cast<const cell_t*>(cip_);
  }
  cell_t read() {
    cell_t value = peek();
    cip_ += sizeof(cell_t);
    return value;
  }
  OPCODE peekOp() {
    return (OPCODE)peek();
  }
  OPCODE readOp() {
    return (OPCODE)read();
  }

  // We use bitmaps to efficiently to track true/false information about
  // addresses. To do this, we convert each instruction addresses into
  // a cell # from the start of the function. This effectively compresses
  // each address by 4 * 32, so a 256KB method has 65,536 instructions, which
  // needs only 8,192 bytes to encode a bitmap.
  uint32_t getCellNumber(const uint8_t* cip) {
    assert(cip >= start_at_);
    return static_cast<uint32_t>((cip - start_at_) / sizeof(cell_t));
  }

 private:
  PluginRuntime* rt_;
  uint32_t start_offset_;
  ke::RefPtr<ControlFlowGraph> graph_;
  int error_code_;

  // Reader state.
  const uint8_t* start_at_;
  const uint8_t* stop_at_;
  const uint8_t* cip_;

  // Computed by prescan().
  BitSet insn_bitmap_;
  BitSet jump_targets_;

  // Block building.
  ke::RefPtr<Block> current_;
  std::vector<ke::RefPtr<Block>> work_queue_;

  typedef ke::HashMap<const uint8_t*,
                      ke::RefPtr<Block>,
                      ke::PointerPolicy<const uint8_t>> BlockMap;
  BlockMap block_map_;
  BitSet block_bitmap_;
};

} // namespace sp

#endif // _include_sourcepawn_vm_graph_builder_h_
