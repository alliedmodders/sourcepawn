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

#include "control-flow.h"
#include "opcodes.h"

namespace sp {

using namespace ke;

ControlFlowGraph::ControlFlowGraph(PluginRuntime* rt, const uint8_t* start_offset)
 : rt_(rt),
   epoch_(1)
{
  entry_ = newBlock(start_offset);
}

ControlFlowGraph::~ControlFlowGraph()
{
  // This is necessary because blocks contain cycles between each other.
  auto iter = blocks_.begin();
  while (iter != blocks_.end()) {
    Block* block = *iter;
    iter = blocks_.erase(iter);
    block->Release();
  }
}

RefPtr<Block>
ControlFlowGraph::newBlock(const uint8_t* start)
{
  Block* block = new Block(*this, start);
  block->AddRef();
  blocks_.append(block);
  return block;
}

// By default, we want to store blocks in reverse postorder, which is the
// easiest form for forward flow analysis.
void
ControlFlowGraph::computeOrdering()
{
  // Note, the following graphs might look similar but they are *not*
  // ambiguous:
  //
  // A -> B -> D -> E
  // A -> C
  // B -> C
  //
  // And its mirror,
  //
  // A -> C -> E
  // A -> B -> D
  // B -> C
  //
  // In both cases the correct traversal order is A, B, C, D, E. The only way
  // to get this is to compute a post-order traversal, left-to-right and
  // reverse it. This will guarantee the critical condition that a predecessor
  // always appears before its successor in the reverse postordering, unless
  // there is a backedge. (In fact, this will be our definition of a backedge).
  struct Entry {
    // All blocks will stay alive during this time, so we don't store RefPtrs.
    Block* block;
    size_t index;
  };
  Vector<Entry> work;

#if !defined(NDEBUG)
  size_t block_count = blocks_.length();
#endif

  // Clear the old block list.
  auto iter = blocks_.begin();
  while (iter != blocks_.end())
    iter = blocks_.erase(iter);

  Vector<Block*> postordering;

  // Compute the postorder traversal.
  newEpoch();
  work.append(Entry{entry_, 0});
  while (!work.empty()) {
    Block* block = work.back().block;
    size_t successor_index = work.back().index;

    if (successor_index >= block->successors().length()) {
      postordering.append(block);
      work.pop();
      continue;
    }
    work.back().index++;

    Block* child = block->successors()[successor_index];
    if (child->visited())
      continue;
    child->setVisited();

    if (!child->successors().empty())
      work.append(Entry{child, 0});
    else
      postordering.append(child);
  }
  assert(postordering.length() == block_count);

  // Add and number everything in RPO.
  uint32_t id = 1;
  for (size_t i = postordering.length() - 1; i < postordering.length(); i--) {
    Block* block = postordering[i];

    assert(!block->id());
    block->setId(id++);
    blocks_.append(block);
  }
}

void
ControlFlowGraph::dump(FILE* fp)
{
  for (RpoIterator iter = rpoBegin(); iter != rpoEnd(); iter++) {
    RefPtr<Block> block = *iter;
    fprintf(fp, "Block %p:\n", block.get());
    for (const auto& pred : block->predecessors())
      fprintf(fp, "  predecessor: %p\n", pred.get());
    for (const auto& child : block->successors())
      fprintf(fp, "  successor: %p\n", child.get());
    fprintf(fp, "  ---\n");
    const uint8_t* cip = block->start();
    while (true) {
      if (block->endType() == BlockEnd::Jump && cip >= block->end())
        break;
      SpewOpcode(fp,
                 rt_,
                 reinterpret_cast<const cell_t*>(block->start()),
                 reinterpret_cast<const cell_t*>(cip));
      cip = NextInstruction(cip);
      if (block->endType() == BlockEnd::Insn && cip > block->end())
        break;
    }
    fprintf(fp, "\n");
  }
}

Block::Block(ControlFlowGraph& graph, const uint8_t* start)
 : graph_(graph),
   start_(start),
   end_(nullptr),
   end_type_(BlockEnd::Unknown),
   id_(0),
   epoch_(0)
{
}

void
Block::addTarget(Block* target)
{
  target->predecessors_.append(this);
  successors_.append(target);
}

void
Block::endWithJump(const uint8_t* end_at, Block* target)
{
  end(end_at, BlockEnd::Jump);
  addTarget(target);
}

void
Block::end(const uint8_t* end_at, BlockEnd end_type)
{
  assert(!end_);
  end_ = end_at;
  end_type_ = end_type;
}

bool
Block::visited() const
{
  return epoch_ == graph_.epoch();
}

void
Block::setVisited()
{
  epoch_ = graph_.epoch();
}

void
Block::unlink()
{
  predecessors_.clear();
  successors_.clear();
}

} // namespace sp
