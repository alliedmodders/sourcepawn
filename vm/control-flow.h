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
#ifndef _include_sourcepawn_vm_control_flow_h_
#define _include_sourcepawn_vm_control_flow_h_

#include <amtl/am-inlinelist.h>
#include <amtl/am-refcounting.h>
#include <amtl/am-vector.h>
#include <stdio.h>
#include "plugin-runtime.h"

namespace sp {

class ControlFlowGraph;

enum class BlockEnd {
  Unknown,
  Insn,
  Jump
};

class Block :
  public ke::Refcounted<Block>,
  public ke::InlineListNode<Block>
{
  friend class ControlFlowGraph;

 private:
  Block(ControlFlowGraph& graph, const uint8_t* start);

 public:
  ~Block() {
    assert(!predecessors_.length());
    assert(!successors_.length());
  }

  const uint8_t* start() const {
    return start_;
  }
  const uint8_t* end() const {
    return end_;
  }
  bool ended() const {
    return !!end_;
  }
  BlockEnd endType() const {
    return end_type_;
  }
  const ke::Vector<ke::RefPtr<Block>>& predecessors() const {
    return predecessors_;
  }
  const ke::Vector<ke::RefPtr<Block>>& successors() const {
    return successors_;
  }
  uint32_t id() const {
    return id_;
  }
  void setId(uint32_t id) {
    id_ = id;
  }
  Block* idom() const {
    return idom_.get();
  }

  void addTarget(Block* target);
  void endWithJump(const uint8_t* cip, Block* target);
  void end(const uint8_t* end_at, BlockEnd end_type);

  void setImmediateDominator(Block* block);

  bool visited() const;
  void setVisited();

  // Zap all references so the block has no cycles.
  void unlink();

 private:
  ControlFlowGraph& graph_;
  ke::Vector<ke::RefPtr<Block>> predecessors_;
  ke::Vector<ke::RefPtr<Block>> successors_;

  // Note that |end| is dependent on end_type. If it's Insn, then end_ should
  // be the |start| of the last instruction, since that is the terminating
  // instruction.
  const uint8_t* start_;
  const uint8_t* end_;
  BlockEnd end_type_;

  // Reverse post-order index.
  uint32_t id_;

  // Immediate dominator.
  ke::RefPtr<Block> idom_;

  // Counter for fast already-visited testing.
  uint32_t epoch_;
};

typedef ke::InlineList<Block>::iterator RpoIterator;
typedef ke::InlineList<Block>::reverse_iterator PostorderIterator;

class ControlFlowGraph : public ke::Refcounted<ControlFlowGraph>
{
 public:
  explicit ControlFlowGraph(PluginRuntime* rt, const uint8_t* start_offset);
  ~ControlFlowGraph();

  ke::RefPtr<Block> entry() const {
    return entry_;
  }

  ke::RefPtr<Block> newBlock(const uint8_t* start);

  // Compute reverse/postorder traversal of the graph. This re-orders blocks.
  void computeOrdering();

  // Compute dominance. This should be called after the graph is finalized.
  void computeDominators();

  // Iterators for blocks - reverse postorder, and postorder.
  RpoIterator rpoBegin() {
    return blocks_.begin();
  }
  RpoIterator rpoEnd() {
    return blocks_.end();
  }
  PostorderIterator poBegin() {
    return blocks_.rbegin();
  }
  PostorderIterator poEnd() {
    return blocks_.rend();
  }

  // Increase the epoch number, effectively resetting which blcoks have been
  // visited.
  void newEpoch() {
    epoch_++;
  }
  uint32_t epoch() const {
    return epoch_;
  }

  void dump(FILE* fp);
  void dumpDot(FILE* fp);

 private:
  PluginRuntime* rt_;
  ke::RefPtr<Block> entry_;
  ke::InlineList<Block> blocks_;
  uint32_t epoch_;
};

} // namespace sp

#endif // _include_sourcepawn_vm_control_flow_h_
