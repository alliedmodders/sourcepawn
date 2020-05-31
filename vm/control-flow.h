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

#include <stdio.h>

#include <utility>
#include <vector>

#include <amtl/am-inlinelist.h>
#include <amtl/am-refcounting.h>
#include "plugin-runtime.h"
#include "label.h"

namespace sp {

class ControlFlowGraph;

enum class BlockEnd {
  Unknown,

  // The end cip for the block is the final instruction.
  Insn,

  // The end cip for the block is not part of the block; instead there is an
  // implicit jump.
  Jump
};

class IBlockData
{
 public:
  virtual ~IBlockData()
  {}
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
    assert(!predecessors_.size());
    assert(!successors_.size());
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
  const std::vector<ke::RefPtr<Block>>& predecessors() const {
    return predecessors_;
  }
  const std::vector<ke::RefPtr<Block>>& successors() const {
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
  uint32_t domTreeId() const {
    return domtree_id_;
  }
  void setDomTreeId(uint32_t id) {
    domtree_id_ = id;
  }
  uint32_t numDominated() const {
    return num_dominated_;
  }
  const std::vector<ke::RefPtr<Block>>& immediatelyDominated() const {
    return immediately_dominated_;
  }
  bool dominates(const Block* other) const {
    // The dominator tree is numbered in pre-order, and num_dominated_ is total
    // number of children in our position in the tree. Therefore, we can check
    // if we dominate a node by seeing if its index is within the range of
    // indices under this node.
    return other->domtree_id_ >= domtree_id_ &&
           other->domtree_id_ < (domtree_id_ + num_dominated_);
  }
  void setIsLoopHeader() {
    is_loop_header_ = true;
  }
  bool isLoopHeader() const {
    return is_loop_header_;
  }
  ControlFlowGraph* graph() const {
    return &graph_;
  }

  template <typename T>
  T* data() const {
    return static_cast<T*>(data_.get());
  }
  void setData(IBlockData* data) {
    std::unique_ptr<IBlockData> ptr(data);
    data_ = std::move(ptr);
  }

  void addTarget(Block* target);
  void endWithJump(const uint8_t* cip, Block* target);
  void end(const uint8_t* end_at, BlockEnd end_type);

  void setImmediateDominator(Block* block);
  void addImmediatelyDominated(Block* block);

  bool visited() const;
  void setVisited();

  // The JIT uses this to manage jump targets.
  Label* label() {
    return &label_;
  }

  // Zap all references so the block has no cycles.
  void unlink();

 private:
  ControlFlowGraph& graph_;
  std::vector<ke::RefPtr<Block>> predecessors_;
  std::vector<ke::RefPtr<Block>> successors_;
  std::unique_ptr<IBlockData> data_;

  // Note that |end| is dependent on end_type. If it's Insn, then end_ should
  // be the |start| of the last instruction, since that is the terminating
  // instruction.
  const uint8_t* start_;
  const uint8_t* end_;
  BlockEnd end_type_;

  // Reverse post-order index.
  uint32_t id_;

  // Immediate dominator, and list of dominated blocks.
  ke::RefPtr<Block> idom_;
  std::vector<ke::RefPtr<Block>> immediately_dominated_;
  // Dominator tree index.
  uint32_t domtree_id_;
  // The number of nodes this block dominates, including itself.
  uint32_t num_dominated_;

  // Set to true if this is a loop header.
  bool is_loop_header_;

  // Label, for the JIT.
  Label label_;

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
  void computeDominance();

  // Compute validity of loops.
  bool computeLoopHeaders();

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
  void dumpDomTreeDot(FILE* fp);

 private:
  PluginRuntime* rt_;
  ke::RefPtr<Block> entry_;
  ke::InlineList<Block> blocks_;
  uint32_t epoch_;
};

template <typename T>
class AutoClearBlockData
{
 public:
  AutoClearBlockData(ControlFlowGraph* graph)
   : graph_(graph)
  {
    for (auto iter = graph_->rpoBegin(); iter != graph_->rpoEnd(); iter++)
      iter->setData(new T());
  }
  ~AutoClearBlockData() {
    for (auto iter = graph_->rpoBegin(); iter != graph_->rpoEnd(); iter++)
      iter->setData(nullptr);
  }

 private:
  ControlFlowGraph* graph_;
};

} // namespace sp

#endif // _include_sourcepawn_vm_control_flow_h_
