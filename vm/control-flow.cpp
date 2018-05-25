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
  for (const auto& block : blocks_)
    block->unlink();
}

ke::RefPtr<Block>
ControlFlowGraph::newBlock(const uint8_t* start)
{
  ke::RefPtr<Block> block = new Block(*this, start);
  blocks_.append(block);
  return block;
}

void
ControlFlowGraph::dump(FILE* fp)
{
  for (const auto& block : blocks_) {
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
