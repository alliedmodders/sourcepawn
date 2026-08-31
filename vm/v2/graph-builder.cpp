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

#include "graph-builder.h"
#include <smx/smx-v2-opcodes.h>
#include <smx/smx-typeinfo.h>
#include "v2/opcodes.h"
#include "v2/runtime.h"

namespace sp::v2 {

using namespace ke;

GraphBuilder::GraphBuilder(Runtime* rt, const smx_rtti_method* method)
 : rt_(rt),
   start_offset_(method->pcode_start)
{
    start_at_ = rt_->code().bytes + method->pcode_start;
    stop_at_ = rt_->code().bytes + method->pcode_end;
}

RefPtr<ControlFlowGraph>
GraphBuilder::build() {
    // Prescan to figure out what offsets are instructions and/or jump targets.
    if (!prescan())
        return nullptr;

    if (!scan())
        return nullptr;

    graph_->computeOrdering();
    graph_->computeDominance();
    if (!graph_->computeLoopHeaders()) {
        error(SP_ERROR_INVALID_INSTRUCTION);
        return nullptr;
    }
    return graph_;
}

// Scan the instruction stream for control-flow opcodes. For each such opcode,
// we push its target onto a stack so it can be visited again later.
bool GraphBuilder::scan() {
    block_map_.init(16);

    // Set cip, start at the method entry.
    cip_ = start_at_;

    graph_ = new ControlFlowGraph(rt_);
    graph_->setEntry(getOrAddBlock(cip_));
    current_ = graph_->entry();

    // Begin an epoch to track which blocks have been visited.
    graph_->newEpoch();

    for (;;) {
        FlowState state = scanFlow();
        if (state == FlowState::Error)
            return false;

        // If there's no more ops to read in the stream, we consider this the same
        // as ending control-flow, even though it's illegal. We check for these
        // blocks in a separate pass.
        if (state == FlowState::Ended || !more()) {
            assert(!current_);

            // If there's no more blocks to process, we're done.
            if (work_queue_.empty())
                return true;

            current_ = ke::PopBack(&work_queue_);
            assert(!current_->ended());

            // Set the cip_ accordingly and proceed.
            cip_ = current_->start();
            continue;
        }

        assert(state == FlowState::Continue);
        assert(current_);

        // Skip past the opcode.
        cip_ = NextInstruction(cip_);
    }
}

static inline bool
IsControlOpcode(OPCODE op) {
    switch (op) {
        case OP_RETN:
        case OP_RETV:
        case OP_JUMP:
        case OP_JZER:
        case OP_JNZ:
        case OP_JEQ:
        case OP_JNEQ:
        case OP_JSLESS:
        case OP_JSLEQ:
        case OP_JSGRTR:
        case OP_JSGEQ:
        case OP_SWITCH:
            return true;
        default:
            return false;
    }
}

auto
GraphBuilder::scanFlow() -> FlowState {
    if (!more()) {
        error(SP_ERROR_INVALID_INSTRUCTION);
        return FlowState::Error;
    }

    uint32_t byte_number = getByteNumber(cip_);
    assert(insn_bitmap_.test(byte_number));

    // Does this opcode mark the start of a new block? Note that we skip this if
    // the current block starts at this cip. This could happen, for example, with
    // a JUMP opcode that loops back to itself. While this is totally pointless,
    // it is not illegal.
    if (jump_targets_.test(byte_number) && current_->start() != cip_) {
        RefPtr<Block> block = getOrAddBlock(cip_);
        current_->endWithJump(cip_, block);
        current_ = nullptr;
        return FlowState::Ended;
    }

    OPCODE op = peekOp();
    if (!IsControlOpcode(op)) {
        // This opcode does not affect control flow, so return.
        return FlowState::Continue;
    }

    // Save a pointer to the start of the instruction.
    const uint8_t* insn = cip_;
    cip_++;

    switch (op) {
        case OP_RETN:
        case OP_RETV:
            current_->end(cip_, BlockEnd::Insn);
            current_ = nullptr;
            return FlowState::Ended;

        case OP_SWITCH: {
            // If this is a switch, we need specialized logic.
            return scanSwitchFlow(insn);
        }

        case OP_JUMP:
        case OP_JZER:
        case OP_JNZ:
        case OP_JEQ:
        case OP_JNEQ:
        case OP_JSLESS:
        case OP_JSLEQ:
        case OP_JSGRTR:
        case OP_JSGEQ: {
            cell_t target_pos = read();
            const uint8_t* target = rt_->code().bytes + target_pos;
            uint32_t target_byte_number = getByteNumber(target);

            // This will check that (a) we target a valid instruction, and (b) that
            // the instruction is within method bounds.
            if (!insn_bitmap_.test(target_byte_number)) {
                error(SP_ERROR_INSTRUCTION_PARAM);
                return FlowState::Error;
            }
            assert(jump_targets_.test(target_byte_number));

            RefPtr<Block> target_block = getOrAddBlock(target);

            // If this is an unconditional jump, there is only one target, so end.
            if (op == OP_JUMP) {
                current_->addTarget(target_block);
                current_->end(cip_, BlockEnd::Insn);
                current_ = nullptr;
                return FlowState::Ended;
            }

            // If we've reached the end of the stream, the instruction is not valid,
            // because the default case proceeds to the next instruction.
            if (!more()) {
                error(SP_ERROR_INVALID_INSTRUCTION);
                return FlowState::Error;
            }

            // Acquire a block for the next cip, and implicitly link it to this
            // block. The first successor is if the jump is not taken; the second
            // successor is if the jump is taken.
            RefPtr<Block> next_block = getOrAddBlock(cip_);
            current_->addTarget(next_block);
            current_->addTarget(target_block);
            current_->end(cip_, BlockEnd::Insn);
            current_ = nullptr;
            return FlowState::Ended;
        }

        default:
            assert(false);
    }
    return FlowState::Ended;
}

auto
GraphBuilder::scanSwitchFlow(const uint8_t* insn) -> FlowState {
    cell_t ncases = read();
    cell_t default_offset = read();

    // Add the default case.
    std::vector<cell_t> cases;
    cases.push_back(default_offset);

    for (int i = 0; i < ncases; i++) {
        // Skip the value.
        read();
        cases.push_back(read());
    }

    // Process each case.
    for (cell_t target_pos : cases) {
        const uint8_t* target = rt_->code().bytes + target_pos;
        if (!insn_bitmap_.test(getByteNumber(target))) {
            error(SP_ERROR_INSTRUCTION_PARAM);
            return FlowState::Error;
        }
        assert(jump_targets_.test(getByteNumber(target)));

        RefPtr<Block> target_block = getOrAddBlock(target);
        current_->addTarget(target_block);
    }

    current_->end(NextInstruction(insn), BlockEnd::Insn);
    current_ = nullptr;
    return FlowState::Ended;
}

ke::RefPtr<Block> GraphBuilder::getOrAddBlock(const uint8_t* cip) {
    // We use a quick existence test before diving into the hash table.
    uint32_t byte_number = getByteNumber(cip);
    if (!block_bitmap_.test(byte_number)) {
        RefPtr<Block> block = graph_->newBlock(cip);
        if (cip != start_at_)
            enqueueBlock(block);

        BlockMap::Insert p = block_map_.findForAdd(cip);
        assert(!p.found());

        block_map_.add(p, cip, block);
        block_bitmap_.set(byte_number);
        return block;
    }

    BlockMap::Result r = block_map_.find(cip);
    assert(r.found());
    return r->value;
}

void GraphBuilder::enqueueBlock(Block* block) {
    if (block->visited())
        return;

    work_queue_.push_back(block);
    block->setVisited();
}

// This pass generates the following information:
//  (1) The code stream bounds, by updating stop_at_.
//  (2) A bitmap of which cells are valid instructions (insn_bitmap_).
//  (3) A bitmap of which cells are the target of jumps (jump_targets_).
//
// It also verifies that jump targets occur within a reasonable boudnary and
// are cell-aligned. We will harden that verification in the full scan phase.
bool
GraphBuilder::prescan() {
    cip_ = start_at_;

    // Allocate the jump bitmap.
    size_t max_bytes = (stop_at_ - start_at_);
    insn_bitmap_ = BitSet(max_bytes);
    jump_targets_ = BitSet(max_bytes);

    while (more()) {
        const uint8_t* insn = cip_;
        OPCODE op = readOp();

        if (op <= 0 || op >= OPCODES_LAST)
            return error(SP_ERROR_INVALID_INSTRUCTION);

        // Mark the bitmap.
        insn_bitmap_.set(getByteNumber(insn));

        // Deduce parameter count.
        int opcode_bytes;
        if (op == OP_SWITCH) {
            if (cip_ + sizeof(cell_t) > stop_at_)
                return error(SP_ERROR_INVALID_INSTRUCTION);
            cell_t ncases = *reinterpret_cast<const cell_t*>(cip_);
            if (ncases > (INT_MAX - 1) / 4)
                return error(SP_ERROR_INVALID_INSTRUCTION);
            opcode_bytes = sizeof(cell_t) * 2 + (ncases * (sizeof(cell_t) * 2));
        } else {
            int opcode_size = GetOpcodeSize(op);
            if (opcode_size == 0) {
                // This opcode is not generated, and is therefore illegal.
                return error(SP_ERROR_INVALID_INSTRUCTION);
            }
            opcode_bytes = opcode_size - 1;
        }
        assert(opcode_bytes >= 0);

        // Make sure the parameters can be read.
        if (cip_ + opcode_bytes > stop_at_)
            return error(SP_ERROR_INVALID_INSTRUCTION);

        // If this is a control opcode, we need to markup any jump targets.
        if (IsControlOpcode(op) && op != OP_RETN && op != OP_RETV && op != OP_SWITCH) {
            // All jump instructions, and SWITCH, have the target as an immediate
            // value.
            if (!prescan_jump_target(op, *reinterpret_cast<const cell_t*>(cip_)))
                return false;
        } else if (op == OP_SWITCH) {
            if (!prescan_casetable(cip_, opcode_bytes))
                return false;
        }

        // Advance to the next instruction.
        cip_ += opcode_bytes;
    }

    // Update the stop-at point.
    stop_at_ = cip_;
    return true;
}

bool
GraphBuilder::prescan_jump_target(OPCODE op, cell_t target) {
    if (target < 0)
        return error(SP_ERROR_INSTRUCTION_PARAM);

    // Note that stop_at_ is still the end of the code section, so this is a
    // valid check. If we ever pre-fill stop_at_ with the correct value, this
    // will still be valid, just redundant.
    if (size_t(target) >= size_t(stop_at_ - rt_->code().bytes))
        return error(SP_ERROR_INSTRUCTION_PARAM);

    // Note that the target must be within the method.
    const uint8_t* cip = rt_->code().bytes + target;
    if (cip < start_at_)
        return error(SP_ERROR_INSTRUCTION_PARAM);

    // Since OP_SWITCH points to a CASETBL, not an actual jump target, ignore it
    // lest we create a pointless block.
    if (op != OP_SWITCH)
        jump_targets_.set(getByteNumber(cip));
    return true;
}

bool
GraphBuilder::prescan_casetable(const uint8_t* pos, cell_t size) {
    const uint8_t* end = pos + size;
    // OP_SWITCH:
    //   ncases
    //   default_offset
    //   [value, offset]
    //
    // Skip ncases.
    pos += sizeof(cell_t);

    cell_t default_offset = *reinterpret_cast<const cell_t*>(pos);
    if (!prescan_jump_target(OP_JUMP, default_offset))
        return false;
    pos += sizeof(cell_t);

    while (pos < end) {
        // Skip the value.
        pos += sizeof(cell_t);
        cell_t target = *reinterpret_cast<const uint32_t*>(pos);
        if (!prescan_jump_target(OP_JUMP, target))
            return false;
        pos += sizeof(cell_t);
    }
    return true;
}

bool
GraphBuilder::cleanup() {
    assert(work_queue_.empty());

    graph_->newEpoch();

    // Find all reachable blocks, from the entrypoint.
    graph_->entry()->setVisited();
    work_queue_.push_back(graph_->entry());
    while (!work_queue_.empty()) {
        ke::RefPtr<Block> block = ke::PopBack(&work_queue_);
        assert(block->visited());

        if (!block->ended()) {
            // This is a serious condition since the function can proceed into
            // uninitialized memory.
            return error(SP_ERROR_INVALID_INSTRUCTION);
        }

        for (const auto& successor : block->successors()) {
            if (successor->visited())
                continue;
            work_queue_.push_back(successor);
            successor->setVisited();
        }
    }

#if !defined(NDEBUG)
    // It should not be possible to have dangling blocks.
    for (BlockMap::iterator iter = block_map_.iter(); !iter.empty(); iter.next())
        assert(iter->value->visited());
#endif
    return true;
}

bool
GraphBuilder::error(int code) {
    rt_->ReportErrorNumber(code);
    return false;
}

} // namespace sp::v2
