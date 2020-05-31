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
#include <amtl/am-string.h>

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
    block->unlink();
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
  std::vector<Entry> work;

#if !defined(NDEBUG)
  size_t block_count = blocks_.length();
#endif

  // Clear the old block list.
  auto iter = blocks_.begin();
  while (iter != blocks_.end())
    iter = blocks_.erase(iter);

  std::vector<Block*> postordering;

  // Compute the postorder traversal.
  newEpoch();
  work.push_back(Entry{entry_, 0});
  while (!work.empty()) {
    Block* block = work.back().block;
    size_t successor_index = work.back().index;

    if (successor_index >= block->successors().size()) {
      postordering.push_back(block);
      work.pop_back();
      continue;
    }
    work.back().index++;

    Block* child = block->successors()[successor_index];
    if (child->visited())
      continue;
    child->setVisited();

    if (!child->successors().empty())
      work.push_back(Entry{child, 0});
    else
      postordering.push_back(child);
  }
  assert(postordering.size() == block_count);

  // Add and number everything in RPO.
  uint32_t id = 1;
  for (size_t i = postordering.size() - 1; i < postordering.size(); i--) {
    Block* block = postordering[i];

    assert(!block->id());
    block->setId(id++);
    blocks_.append(block);
  }
}

// "A Simple, Fast Dominance Algorithm" by Keith D. Cooper, Timothy J. HArvey,
// and Ken Kennedy.
//
// Iterative solution with a two-finger intersection algorithm. Note the paper
// uses postordered numbers, whereas we use RPO. The id comparisons are
// therefore inverted.
static Block*
IntersectDominators(Block* block1, Block* block2)
{
  Block* finger1 = block1;
  Block* finger2 = block2;
  while (finger1 != finger2) {
    while (finger1->id() > finger2->id())
      finger1 = finger1->idom();
    while (finger2->id() > finger1->id())
      finger2 = finger2->idom();
  }
  return finger1;
}

void
ControlFlowGraph::computeDominance()
{
  // The entry block dominates itself.
  entry_->setImmediateDominator(entry_);

  // Compute immediate dominators. This is technically an O(n^2) algorithm,
  // with a worst case being that every block is a join point. In practice
  // it is nowhere near that bad. In addition, since we traverse the graph
  // in RPO, the maximum number of iterations is 2.
  //
  // This is not technically true, if the graph is irreducible. Irreducible
  // graphs will fail to validate in the computeLoopHeaders pass.
  bool changed = false;
  do {
    changed = false;

    for (auto iter = rpoBegin(); iter != rpoEnd(); iter++) {
      Block* block = *iter;

      if (block->predecessors().empty()) {
        // There is only one entry block.
        assert(block == entry_);
        continue;
      }

      // Pick a candidate for this node's dominator.
      Block* idom = nullptr;
      for (size_t i = 0; i < block->predecessors().size(); i++) {
        Block* pred = block->predecessors()[i];
        if (!pred->idom())
          continue;
        if (idom)
          idom = IntersectDominators(idom, pred);
        else
          idom = pred;
        assert(idom);
      }

      // We should always get one candidate dominator, since RPO order
      // guarantees we've processed at least one predecessor.
      assert(idom);

      if (idom != block->idom()) {
        block->setImmediateDominator(idom);
        changed = true;
      }
    }
  } while (changed);

  // Build the dominator tree, walking the graph bottom-up.
  for (auto iter = poBegin(); iter != poEnd(); iter++) {
    Block* block = *iter;
    Block* idom = block->idom();

    // Only the entry should have itself as an immediate dominator.
    if (block == idom) {
      assert(block == entry_);
      continue;
    }

    idom->addImmediatelyDominated(block);
  }
  assert(entry_->numDominated() == blocks_.length());

  // Process the dominator tree. Note that it is acyclic, so we do not need a
  // visited marker. We walk the tree and assign a pre-order index to each
  // dominator.
  std::vector<Block*> work;
  work.push_back(entry_);

  uint32_t id = 0;
  while (!work.empty()) {
    Block* block = ke::PopBack(&work);

    // We should never visit the same block twice.
    assert(!block->domTreeId());

    block->setDomTreeId(id++);
    for (const auto& child : block->immediatelyDominated())
      work.push_back(child);
  }

#if !defined(NDEBUG)
  // Every node should have an index in the dominator tree.
  for (auto iter = rpoBegin(); iter != rpoEnd(); iter++)
    assert(iter->domTreeId() || *iter == entry_);
#endif
}

bool
ControlFlowGraph::computeLoopHeaders()
{
  // This algorithm is O(E), where E is the number of edges in the graph.
  for (auto iter = rpoBegin(); iter != rpoEnd(); iter++) {
    Block* block = *iter;
    for (const auto& child : iter->successors()) {
      // Since we number blocks in RPO, any backedge will precede this block
      // in the graph.
      if (child->id() <= block->id()) {
        child->setIsLoopHeader();

        // If the loop header does not dominate the contained block, then
        // the graph is not reducible. We want this to be invalid for now.
        if (!child->dominates(block))
          return false;
      }
    }
  }
  return true;
}

void
ControlFlowGraph::dump(FILE* fp)
{
  for (RpoIterator iter = rpoBegin(); iter != rpoEnd(); iter++) {
    Block* block = *iter;
    fprintf(fp, "Block %p (%d):\n", block, block->id());
    for (const auto& pred : block->predecessors())
      fprintf(fp, "  predecessor: %p\n", pred.get());
    for (const auto& child : block->successors())
      fprintf(fp, "  successor: %p\n", child.get());
    fprintf(fp, "  ---\n");
    const uint8_t* cip = block->start();
    while (cip < block->end()) {
      SpewOpcode(fp,
                 rt_,
                 reinterpret_cast<const cell_t*>(block->start()),
                 reinterpret_cast<const cell_t*>(cip));
      cip = NextInstruction(cip);
    }
    if (block->endType() == BlockEnd::Insn) {
      SpewOpcode(fp,
                 rt_,
                 reinterpret_cast<const cell_t*>(block->start()),
                 reinterpret_cast<const cell_t*>(cip));
    }
    fprintf(fp, "\n");
  }
}

static std::string
MakeDotBlockname(Block* block)
{
  return ke::StringPrintf("block%d_%p", block->id(), block);
}

void
ControlFlowGraph::dumpDot(FILE* fp)
{
  fprintf(fp, "digraph cfg {\n");
  for (RpoIterator iter = rpoBegin(); iter != rpoEnd(); iter++) {
    Block* block = *iter;
    for (const auto& successor : block->successors()) {
      fprintf(fp, "  %s -> %s;\n",
        MakeDotBlockname(block).c_str(),
        MakeDotBlockname(successor).c_str());
    }
  }
  fprintf(fp, "}\n");
}

void
ControlFlowGraph::dumpDomTreeDot(FILE* fp)
{
  fprintf(fp, "digraph domtree {\n");

  std::vector<Block*> work;
  work.push_back(entry_);
  while (!work.empty()) {
    Block* block = ke::PopBack(&work);
    for (const auto& child : block->immediatelyDominated()) {
      fprintf(fp, "  %s -> %s;\n",
        MakeDotBlockname(block).c_str(),
        MakeDotBlockname(child).c_str());
      work.push_back(child);
    }
  }

  fprintf(fp, "}\n");
}

Block::Block(ControlFlowGraph& graph, const uint8_t* start)
 : graph_(graph),
   start_(start),
   end_(nullptr),
   end_type_(BlockEnd::Unknown),
   id_(0),
   domtree_id_(0),
   num_dominated_(1),
   epoch_(0)
{
}

void
Block::addTarget(Block* target)
{
  target->predecessors_.push_back(this);
  successors_.push_back(target);
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
Block::setImmediateDominator(Block* block)
{
  idom_ = block;
}

void
Block::addImmediatelyDominated(Block* child)
{
  immediately_dominated_.push_back(child);
  num_dominated_ += child->numDominated();
}

void
Block::unlink()
{
  predecessors_.clear();
  successors_.clear();
  idom_ = nullptr;
  immediately_dominated_.clear();
}

} // namespace sp
