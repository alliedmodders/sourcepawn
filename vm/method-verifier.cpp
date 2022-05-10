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
#include <assert.h>
#include <limits.h>

#include <amtl/am-vector.h>
#include "method-verifier.h"
#include "graph-builder.h"
#include "opcodes.h"
#include "plugin-runtime.h"
#include "plugin-context.h"

namespace sp {

using namespace ke;

MethodVerifier::MethodVerifier(PluginRuntime* rt, uint32_t startOffset)
 : rt_(rt),
   block_(nullptr),
   startOffset_(startOffset),
   memSize_(rt_->context()->HeapSize()),
   datSize_(rt_->image()->DescribeData().length),
   heapSize_(memSize_ - datSize_),
   max_stack_(0),
   code_(nullptr),
   cip_(nullptr),
   prev_cip_(nullptr),
   stop_at_(nullptr),
   error_(SP_ERROR_NONE)
{
  assert(datSize_ < memSize_);
  assert(heapSize_ <= memSize_ - datSize_);

  code_features_ = rt_->image()->DescribeCode().features;

  auto& code = rt_->code();
  code_ = reinterpret_cast<const cell_t*>(code.bytes);
  stop_at_ = reinterpret_cast<const cell_t*>(code.bytes + code.length);
}

ke::RefPtr<ControlFlowGraph>
MethodVerifier::verify()
{
  if (!IsAligned(startOffset_, sizeof(cell_t))) {
    reportError(SP_ERROR_INVALID_ADDRESS);
    return nullptr;
  }

  auto image = rt_->image();
  if (image->HasRtti()) {
    auto rtti = image->GetMethodRttiByOffset(startOffset_);
    if (!rtti || rtti->pcode_start != startOffset_) {
      reportError(SP_ERROR_INVALID_ADDRESS);
      return nullptr;
    }
  }

  GraphBuilder gb(rt_, startOffset_);
  graph_ = gb.build();
  if (!graph_) {
    reportError(gb.error_code());
    return nullptr;
  }

  AutoClearBlockData<VerifyData> acbd(graph_);

  method_ = code_ + (startOffset_ / sizeof(cell_t));

  for (auto iter = graph_->rpoBegin(); iter != graph_->rpoEnd(); iter++) {
    block_ = *iter;
    if (!handleJoins())
      return nullptr;

    prev_cip_ = nullptr;

    cip_ = reinterpret_cast<const cell_t*>(block_->start());
    while (cip_ < reinterpret_cast<const cell_t*>(block_->end())) {
      insn_ = cip_;
      OPCODE op = (OPCODE)*cip_++;
      if (!verifyOp(op))
        return nullptr;
      prev_cip_ = insn_;
    }
  }

  // Verify loop headers.
  for (const auto& block : verify_joins_) {
    if (!verifyJoins(block))
      return nullptr;
  }

  if (max_stack_ > INT_MAX / 4) {
    reportError(SP_ERROR_STACKLOW);
    return nullptr;
  }
  max_stack_ *= sizeof(cell_t);

  return graph_;
}

static inline bool
ExtractPushConstant(const cell_t* cip, cell_t* value)
{
  switch (*cip) {
    case OP_PUSH_C:
      *value = cip[1];
      return true;
    case OP_PUSH2_C:
      *value = cip[2];
      return true;
    case OP_PUSH3_C:
      *value = cip[3];
      return true;
    case OP_PUSH4_C:
      *value = cip[4];
      return true;
    case OP_PUSH5_C:
      *value = cip[5];
      return true;
    default:
      return false;
  }
}

bool
MethodVerifier::verifyOp(OPCODE op)
{
  switch (op) {
  case OP_PROC:
  case OP_NOP:
  case OP_BREAK:
  case OP_LOAD_I:
  case OP_STOR_I:
  case OP_LIDX:
  case OP_IDXADDR:
  case OP_MOVE_PRI:
  case OP_MOVE_ALT:
  case OP_XCHG:
  case OP_SHL:
  case OP_SHR:
  case OP_SSHR:
  case OP_SMUL:
  case OP_SDIV:
  case OP_SDIV_ALT:
  case OP_ADD:
  case OP_SUB:
  case OP_SUB_ALT:
  case OP_AND:
  case OP_OR:
  case OP_XOR:
  case OP_NOT:
  case OP_NEG:
  case OP_INVERT:
  case OP_ZERO_PRI:
  case OP_ZERO_ALT:
  case OP_EQ:
  case OP_NEQ:
  case OP_SLESS:
  case OP_SLEQ:
  case OP_SGRTR:
  case OP_SGEQ:
  case OP_INC_PRI:
  case OP_INC_ALT:
  case OP_INC_I:
  case OP_DEC_PRI:
  case OP_DEC_ALT:
  case OP_DEC_I:
  case OP_STRADJUST_PRI:
    return true;

  case OP_TRACKER_POP_SETHEAP:
  {
    if (code_features_ & SmxConsts::kCodeFeatureHeapScopes) {
      reportError(SP_ERROR_INVALID_INSTRUCTION);
      return false;
    }

    VerifyData* v = block_->data<VerifyData>();
    if (v->tracker_balance.empty()) {
      reportError(SP_ERROR_INVALID_INSTRUCTION);
      return false;
    }

    // As a special case we allow pushing 0 for balance across ternary operations.
    auto amount = ke::PopBack(&v->tracker_balance);
    if (amount == 0)
      return true;

    if (amount == -1) {
      // There must be an indeterminate heap allocation.
      if (v->heap_balance.empty() || v->heap_balance.back() != -1) {
        reportError(SP_ERROR_INVALID_INSTRUCTION);
        return false;
      }
      v->heap_balance.pop_back();
      return true;
    }

    while (!v->heap_balance.empty()) {
      auto value = ke::PopBack(&v->heap_balance);
      if (value == -1)
        break;
      if (value > amount) {
        v->heap_balance.emplace_back(value - amount);
        return true;
      }
      amount -= value;
      if (amount == 0)
        return true;
    }

    reportError(SP_ERROR_INVALID_INSTRUCTION);
    return false;
  }

  case OP_SWAP_PRI:
  case OP_SWAP_ALT:
    // Simulate the swap operation.
    if (!popStack(1))
      return false;
    return pushStack(1);

  case OP_PUSH_PRI:
  case OP_PUSH_ALT:
    return pushStack(1);

  case OP_POP_PRI:
  case OP_POP_ALT:
    return popStack(1);

  case OP_ADDR_ALT:
  case OP_ADDR_PRI:
  case OP_LOAD_S_PRI:
  case OP_LOAD_S_ALT:
  case OP_LREF_S_PRI:
  case OP_LREF_S_ALT:
  case OP_SREF_S_PRI:
  case OP_SREF_S_ALT:
  case OP_STOR_S_ALT:
  case OP_STOR_S_PRI:
  case OP_INC_S:
  case OP_DEC_S:
  case OP_ZERO_S:
  {
    cell_t offset = readCell();
    return verifyStackOffset(offset);
  }

  case OP_LOAD_PRI:
  case OP_LOAD_ALT:
  case OP_STOR_PRI:
  case OP_STOR_ALT:
  case OP_INC:
  case OP_DEC:
  case OP_ZERO:
  {
    cell_t offset = readCell();
    return verifyDatOffset(offset);
  }

  case OP_LODB_I:
  case OP_STRB_I:
  {
    cell_t val = readCell();
    if (val != 1 && val != 2 && val != 4) {
      reportError(SP_ERROR_INVALID_INSTRUCTION);
      return false;
    }
    return true;
  }

  case OP_PUSH_C:
  case OP_PUSH2_C:
  case OP_PUSH3_C:
  case OP_PUSH4_C:
  case OP_PUSH5_C:
  {
    size_t n = 1;
    if (op >= OP_PUSH2_C)
      n = ((op - OP_PUSH2_C) / 4) + 2;

    cip_ += n;
    return pushStack(n);
  }

  case OP_PUSH:
  case OP_PUSH2:
  case OP_PUSH3:
  case OP_PUSH4:
  case OP_PUSH5:
  {
    size_t n = 1;
    if (op >= OP_PUSH2)
      n = ((op - OP_PUSH2) / 4) + 2;
    
    for (size_t i = 0; i < n; i++) {
      cell_t offset = readCell();
      if (!verifyDatOffset(offset))
        return false;
    }
    return pushStack(n);
  }

  case OP_PUSH_S:
  case OP_PUSH2_S:
  case OP_PUSH3_S:
  case OP_PUSH4_S:
  case OP_PUSH5_S:
  {
    size_t n = 1;
    if (op >= OP_PUSH2_S)
      n = ((op - OP_PUSH2_S) / 4) + 2;

    for (size_t i = 0; i < n; i++) {
      cell_t offset = readCell();
      if (!verifyStackOffset(offset))
        return false;
    }
    return pushStack(n);
  }

  case OP_PUSH_ADR:
  case OP_PUSH2_ADR:
  case OP_PUSH3_ADR:
  case OP_PUSH4_ADR:
  case OP_PUSH5_ADR:
  {
    size_t n = 1;
    if (op >= OP_PUSH2_ADR)
      n = ((op - OP_PUSH2_ADR) / 4) + 2;

    for (size_t i = 0; i < n; i++) {
      cell_t offset = readCell();
      if (!verifyStackOffset(offset))
        return false;
    }
    return pushStack(n);
  }

  case OP_CALL:
  {
    // An OP_CALL must be preceded by a PUSH_C variant, and it must be in the
    // same block.
    cell_t nparams;
    if (!prev_cip_ || !ExtractPushConstant(prev_cip_, &nparams)) {
      reportError(SP_ERROR_INVALID_INSTRUCTION);
      return false;
    }
    cell_t offset = readCell();
    if (!verifyCallOffset(offset))
      return false;
    if (!popStack(nparams + 1))
      return false;
    if (collect_func_refs_)
      collect_func_refs_(offset);
    return true;
  }

  case OP_JUMP:
  case OP_JZER:
  case OP_JNZ:
  case OP_JEQ:
  case OP_JNEQ:
  case OP_JSLESS:
  case OP_JSLEQ:
  case OP_JSGRTR:
  case OP_JSGEQ:
  case OP_SHL_C_PRI:
  case OP_SHL_C_ALT:
  case OP_ADD_C:
  case OP_SMUL_C:
  case OP_EQ_C_PRI:
  case OP_EQ_C_ALT:
  case OP_CONST_PRI:
  case OP_CONST_ALT:
  case OP_BOUNDS:
  case OP_SWITCH:
  {
    cip_++;
    return true;
  }

  case OP_TRACKER_PUSH_C:
  {
    if (code_features_ & SmxConsts::kCodeFeatureHeapScopes) {
      reportError(SP_ERROR_INVALID_INSTRUCTION);
      return false;
    }

    cell_t val = readCell();
    if (val < 0 || !ke::IsAligned(val, sizeof(cell_t))) {
        reportError(SP_ERROR_INVALID_INSTRUCTION);
        return false;
    }

    val /= sizeof(cell_t);
    if (val > 0) {
      // The amount being tracked must be a statically verifiable amount allocated
      // on the heap.
      cell_t total = 0;
      auto& heap = block_->data<VerifyData>()->heap_balance;
      for (auto iter = heap.rbegin(); iter != heap.rend(); iter++) {
        if (*iter == -1)
          break;
        total += *iter;
        if (total >= val)
          break;
      }
      if (val > total) {
        reportError(SP_ERROR_INVALID_INSTRUCTION);
        return false;
      }
    }

    block_->data<VerifyData>()->tracker_balance.push_back(val);
    return true;
  }

  case OP_HALT:
  {
    reportError(SP_ERROR_INVALID_INSTRUCTION);
    return false;
  }

  case OP_MOVS:
  {
    cell_t val = readCell();
    return verifyMemAmount(val);
  }

  case OP_FILL:
  {
    cell_t val = readCell();
    return verifyMemAmount(val);
  }

  // Note - STACK and HEAP are verified at runtime.
  case OP_STACK:
  case OP_HEAP:
  {
    cell_t value = readCell();
    if (!ke::IsAligned(value, sizeof(cell_t))) {
      reportError(SP_ERROR_INSTRUCTION_PARAM);
      return false;
    }
    cell_t num_cells = abs(value / (cell_t)sizeof(cell_t));
    if (op == OP_STACK) {
      if (value < 0)
        return pushStack(num_cells);
      return popStack(num_cells);
    } else if (op == OP_HEAP) {
      if (!(code_features_ & SmxConsts::kCodeFeatureHeapScopes)) {
        if (value >= 0)
          return pushHeap(num_cells);
        return popHeap(num_cells);
      } else if (value < 0) {
        reportError(SP_ERROR_INSTRUCTION_PARAM);
        return false;
      }
    }
    return true;
  }

  case OP_SYSREQ_C:
  {
    cell_t index = readCell();
    if (index < 0 || size_t(index) >= rt_->image()->NumNatives()) {
      reportError(SP_ERROR_INSTRUCTION_PARAM);
      return false;
    }
    return true;
  }

  case OP_SYSREQ_N:
  {
    cell_t index = readCell();
    if (index < 0 || size_t(index) >= rt_->image()->NumNatives()) {
      reportError(SP_ERROR_INSTRUCTION_PARAM);
      return false;
    }
    cell_t nparams = readCell();
    if (!pushStack(1))
      return false;
    if (!popStack(nparams + 1))
      return false;
    return verifyParamCount(nparams);
  }

  case OP_LOAD_BOTH:
  {
    cell_t offs1 = readCell();
    cell_t offs2 = readCell();
    if (!verifyDatOffset(offs1) ||
        !verifyDatOffset(offs2))
    {
      return false;
    }
    return true;
  }

  case OP_LOAD_S_BOTH:
  {
    cell_t offs1 = readCell();
    cell_t offs2 = readCell();
    if (!verifyStackOffset(offs1) ||
        !verifyStackOffset(offs2))
    {
      return false;
    }
    return true;
  }

  case OP_CONST:
  {
    cell_t offset = readCell();
    cip_++;
    return verifyDatOffset(offset);
  }

  case OP_CONST_S:
  {
    cell_t offset = readCell();
    cip_++;
    return verifyStackOffset(offset);
  }

  case OP_GENARRAY:
  case OP_GENARRAY_Z:
  {
    cell_t ndims = readCell();
    if (!verifyDimensionCount(ndims))
      return false;
    if (!popStack(ndims - 1))
      return false;
    if (!(code_features_ & SmxConsts::kCodeFeatureHeapScopes)) {
      block_->data<VerifyData>()->heap_balance.push_back(-1);
      block_->data<VerifyData>()->tracker_balance.push_back(-1);
    }
    return true;
  }

  case OP_INITARRAY_PRI:
  case OP_INITARRAY_ALT:
  {
    if (!(code_features_ & SmxConsts::kCodeFeatureDirectArrays)) {
      reportError(SP_ERROR_INVALID_INSTRUCTION);
      return false;
    }

    constexpr cell_t kMaxCells = INT_MAX / (2 * (int)sizeof(cell_t));

    cell_t addr = readCell();
    cell_t iv_size = readCell();
    cell_t data_copy_size = readCell();
    cell_t data_fill_size = readCell();
    cell_t fill_value = readCell();
    if (iv_size < 0 ||
        data_copy_size < 0 ||
        data_fill_size < 0 ||
        iv_size >= kMaxCells ||
        data_copy_size >= kMaxCells ||
        data_fill_size >= kMaxCells ||
        (!data_fill_size && fill_value) ||
        !ke::IsAligned(addr, sizeof(cell_t)))
    {
      reportError(SP_ERROR_INSTRUCTION_PARAM);
      return false;
    }

    cell_t copy_addr = addr + iv_size * sizeof(cell_t);
    cell_t fill_addr = copy_addr + data_copy_size * sizeof(cell_t);
    if (copy_addr < addr || fill_addr < copy_addr ||
        !ke::IsUintAddSafe<uint32_t>(fill_addr, data_fill_size * sizeof(cell_t)))
    {
      reportError(SP_ERROR_INSTRUCTION_PARAM);
      return false;
    }

    // If there's nothing to read from DAT, we can early return.
    if (!iv_size && !data_copy_size)
      return true;

    cell_t end_addr = addr + (iv_size + data_copy_size) * sizeof(cell_t);
    if (!verifyDatOffset(addr) || !verifyDatOffset(end_addr - 1))
      return false;
    return true;
  }

  case OP_CASETBL:
    cip_ = insn_ + GetCaseTableSize(reinterpret_cast<const uint8_t*>(insn_));
    return true;

  case OP_HEAP_SAVE:
    if (!(code_features_ & SmxConsts::kCodeFeatureHeapScopes)) {
      reportError(SP_ERROR_INVALID_INSTRUCTION);
      return false;
    }
    block_->data<VerifyData>()->heap_scope_depth++;
    return true;

  case OP_HEAP_RESTORE:
    if (!(code_features_ & SmxConsts::kCodeFeatureHeapScopes)) {
      reportError(SP_ERROR_INVALID_INSTRUCTION);
      return false;
    }
    if (!block_->data<VerifyData>()->heap_scope_depth) {
      reportError(SP_ERROR_INVALID_INSTRUCTION);
      return false;
    }
    block_->data<VerifyData>()->heap_scope_depth--;
    return true;

  case OP_RETN:
    block_->heap_scope_depth() = block_->data<VerifyData>()->heap_scope_depth;
    return true;

  default:
    // Should have been caught earlier.
    assert(op != OP_PROC);
    reportError(SP_ERROR_INVALID_INSTRUCTION);
    return false;
  }
}

cell_t
MethodVerifier::readCell()
{
  assert(cip_ < stop_at_);
  return *cip_++;
}

// spcomp had a long standing bug where "break" in a loop would not correctly
// free dynamic arrays. This bug was extremely rare, and probably never
// manifested in noticeable behavior. Nonetheless it did occur and we need to
// make sure validation does not get confused.
static inline bool
DetectCompilerBreakBug(Block* block)
{
  // First, walk up the dominator tree until we find a loop header.
  Block* dom = block->idom();
  while (!dom->isLoopHeader() && !dom->predecessors().empty())
    dom = dom->idom();

  // If no loop was found, this is not the bug we're looking for.
  if (!dom->isLoopHeader())
    return false;

  block->graph()->newEpoch();

  // To figure out whether or not this block is "leaving" the loop, we need to
  // compute the set of blocks owned by the loop. We do this by computing
  // the predecessors of each backedge.
  std::vector<Block*> worklist;
  for (const auto& pred : dom->predecessors()) {
    if (pred->id() >= dom->id()) {
      pred->setVisited();
      worklist.emplace_back(pred);
    }
  }

  while (!worklist.empty()) {
    Block* block = ke::PopBack(&worklist);
    for (const auto& pred : block->predecessors()) {
      // Note: we need to make sure we don't predecessors beyond the initial
      // loop header.
      if (pred->visited() || pred->id() < dom->id())
        continue;

      // This node should be dominated by the loop header.
      assert(dom->dominates(pred));

      // Keep walking up nodes.
      pred->setVisited();
      worklist.emplace_back(pred);
    }
  }

  // If we never visited the original block, that means it is not in the
  // predecessor set of of any backedge. It could be a break.
  return !block->visited();
}

bool
MethodVerifier::verifyJoin(VerifyData* first, VerifyData* other)
{
  if (first->stack_balance != other->stack_balance) {
    reportError(SP_ERROR_INSTRUCTION_PARAM);
    return false;
  }

  if (first->tracker_balance.size() != other->tracker_balance.size()) {
    if (DetectCompilerBreakBug(block_)) {
      block_->set_has_compiler_break_bug();
      return true;
    }
    reportError(SP_ERROR_INSTRUCTION_PARAM);
    return false;
  }

  if (first->heap_scope_depth != other->heap_scope_depth) {
    reportError(SP_ERROR_INSTRUCTION_PARAM);
    return false;
  }
  return true;
}

bool
MethodVerifier::mergeTracker(Block* block, VerifyData* other)
{
  VerifyData* join = block->data<VerifyData>();
  if (!verifyJoin(join, other))
    return false;

  if (code_features_ & SmxConsts::kCodeFeatureHeapScopes)
    return true;

  if (block->has_compiler_break_bug())
    return true;

  // If our tracker value is determinate, but another branch was different
  // (or indeterminate), we allow this and convert the propagated amount to
  // be indeterminate.
  auto heap_cursor = join->heap_balance.size();
  for (auto cursor = join->tracker_balance.size(); cursor != 0; cursor--) {
    size_t index = cursor - 1;
    cell_t this_amount = join->tracker_balance[index];
    cell_t other_amount = other->tracker_balance[index];
    if (this_amount == -1 || other_amount == this_amount)
      continue;

    // Convert this tracker entry into an unknown quantity.
    join->tracker_balance[index] = -1;

    // If the amount was zero, there's no heap cursor entry, so fab one.
    if (this_amount == 0) {
      // note: heap_cursor is +1, so we're inserting after the current element.
      join->heap_balance.emplace(join->heap_balance.begin() + heap_cursor, -1);
      continue;
    }

    // If there's no heap entries, something went wrong when we analyzec the
    // tracker opcode.
    if (heap_cursor == 0) {
      reportError(SP_ERROR_INSTRUCTION_PARAM);
      return false;
    }

    // Walk the heap and fix up static tracking to be indeterminate.
    while (heap_cursor != 0) {
      auto entry = join->heap_balance.begin() + (heap_cursor - 1);
      if (*entry < this_amount) {
        this_amount -= *entry;
        if (join->heap_balance.erase(entry) != join->heap_balance.begin())
          heap_cursor--;
        continue;
      }
      if (*entry == this_amount) {
        *entry = -1;
        heap_cursor--;
        break;
      }
      if (*entry > this_amount) {
        *entry -= this_amount;

        // note: heap_cursor doesn't change, since we modified in-place and
        // inserted the new value after.
        join->heap_balance.emplace(entry + 1, -1);
        break;
      }
    }
  }
  return true;
}

bool
MethodVerifier::handleJoins()
{
  if (block_->predecessors().empty())
    return true;

  bool verify_later = false;

  bool found_pred = false;
  for (size_t i = 0; i < block_->predecessors().size(); i++) {
    Block* pred = block_->predecessors()[i];

    // Backedges won't have been visited yet, so we'll have to verify this
    // block again later. However, we keep going to get at least one
    // predecessor to use for our initial stack balance.
    if (pred->id() >= block_->id()) {
      verify_later = true;
      continue;
    }

    VerifyData* pred_data = pred->data<VerifyData>();
    if (!found_pred) {
      // Inherit everything from the first already-visited preceding block.
      VerifyData* data = block_->data<VerifyData>();
      *data = *pred_data;

      // Save the entry state.
      data->entry = std::make_unique<VerifyData>(*pred_data);

      found_pred = true;
      continue;
    }

    if (!mergeTracker(block_, pred_data))
      return false;
  }

  if (verify_later)
    verify_joins_.push_back(block_);

  // If the block had no incoming edges other than backedges, then this would
  // be an illegal backedge to the entry block. While this is not allowed
  // currently, because of OP_PROC, it may be allowed in the future, so we
  // handle it.
  if (!found_pred) {
    assert(verify_later);
    return true;
  }
  return true;
}

bool
MethodVerifier::verifyJoins(Block* block)
{
  VerifyData* join_data = block->data<VerifyData>();
  VerifyData* entry = join_data->entry.get();

  for (size_t i = 0; i < block->predecessors().size(); i++) {
    Block* other_pred = block->predecessors()[i];
    VerifyData* other_edge = other_pred->data<VerifyData>();

    if (!verifyJoin(entry, other_edge))
      return false;
  }
  return true;
}

bool
MethodVerifier::pushStack(uint32_t num_cells)
{
  VerifyData* v = block_->data<VerifyData>();
  if (!ke::IsUint32AddSafe(v->stack_balance, num_cells)) {
    reportError(SP_ERROR_INSTRUCTION_PARAM);
    return false;
  }

  v->stack_balance += num_cells;
  if (v->stack_balance > INT_MAX / sizeof(cell_t)) {
    reportError(SP_ERROR_INSTRUCTION_PARAM);
    return false;
  }

  if (v->stack_balance > max_stack_)
    max_stack_ = v->stack_balance;
  return true;
}

bool
MethodVerifier::popStack(uint32_t num_cells)
{
  VerifyData* v = block_->data<VerifyData>();
  if (num_cells > v->stack_balance) {
    reportError(SP_ERROR_INSTRUCTION_PARAM);
    return false;
  }

  v->stack_balance -= num_cells;
  return true;
}

bool
MethodVerifier::pushHeap(uint32_t num_cells)
{
  VerifyData* v = block_->data<VerifyData>();
  if (!num_cells || num_cells > INT_MAX / 4) {
    reportError(SP_ERROR_INSTRUCTION_PARAM);
    return false;
  }
  if (!(code_features_ & SmxConsts::kCodeFeatureHeapScopes)) {
    if (v->heap_balance.empty() || v->heap_balance.back() == -1)
      v->heap_balance.push_back(num_cells);
    else
      v->heap_balance.back() += num_cells;
  }
  return true;
}

bool
MethodVerifier::popHeap(uint32_t num_cells)
{
  VerifyData* v = block_->data<VerifyData>();
  if (v->heap_balance.empty() ||
      v->heap_balance.back() == -1 ||
      uint32_t(v->heap_balance.back()) < num_cells)
  {
    reportError(SP_ERROR_INSTRUCTION_PARAM);
    return false;
  }
  if (!(code_features_ & SmxConsts::kCodeFeatureHeapScopes)) {
    v->heap_balance.back() -= num_cells;
    if (v->heap_balance.back() == 0)
      v->heap_balance.pop_back();
  }
  return true;
}

bool
MethodVerifier::verifyStackOffset(cell_t offset)
{
  if (offset >= 0) {
    // This is a rough estimate, we just make sure it definitely won't go out of
    // the heap. We can verify this better later with RTTI tables, which store
    // parameter counts.
    size_t estimate = size_t((offset < 0) ? -offset : offset);
    if (estimate >= heapSize_) {
      reportError(SP_ERROR_INSTRUCTION_PARAM);
      return false;
    }
    return true;
  }

  uint32_t addr = abs(offset);

  // This is not a rough estimate. We know exactly how much stack space is
  // available.
  VerifyData* data = block_->data<VerifyData>();
  if (addr > data->stack_balance * sizeof(cell_t)) {
    reportError(SP_ERROR_INSTRUCTION_PARAM);
    return false;
  }
  return true;
}

bool
MethodVerifier::verifyDatOffset(cell_t offset)
{
  if (offset < 0 || size_t(offset) >= datSize_) {
    reportError(SP_ERROR_INSTRUCTION_PARAM);
    return false;
  }
  return true;
}

bool
MethodVerifier::verifyDimensionCount(cell_t ndims)
{
  if (ndims <= 0) {
    reportError(SP_ERROR_INSTRUCTION_PARAM);
    return false;
  }
  return true;
}

bool
MethodVerifier::verifyParamCount(cell_t nparams)
{
  if (nparams < 0 || nparams > SP_MAX_CALL_ARGUMENTS) {
    reportError(SP_ERROR_INSTRUCTION_PARAM);
    return false;
  }
  return true;
}

bool
MethodVerifier::verifyCallOffset(cell_t offset)
{
  if (offset < 0 || !IsAligned(offset, sizeof(cell_t))) {
    reportError(SP_ERROR_INSTRUCTION_PARAM);
    return false;
  }

  const cell_t* target = code_ + (offset / sizeof(cell_t));
  if (target < code_ || target >= stop_at_) {
    reportError(SP_ERROR_INSTRUCTION_PARAM);
    return false;
  }
  if (target[0] != OP_PROC) {
    reportError(SP_ERROR_INSTRUCTION_PARAM);
    return false;
  }
  return true;
}

bool
MethodVerifier::verifyStackAmount(cell_t amount)
{
  // This is a rough estimate, we just make sure it definitely
  // won't go out of the heap.
  size_t estimate = size_t((amount < 0) ? -amount : amount);
  if (estimate >= heapSize_) {
    reportError(SP_ERROR_INSTRUCTION_PARAM);
    return false;
  }
  return true;
}

bool
MethodVerifier::verifyHeapAmount(cell_t amount)
{
  // This is a rough estimate, we just make sure it definitely
  // won't go out of the heap.
  size_t estimate = size_t((amount < 0) ? -amount : amount);
  if (estimate >= heapSize_) {
    reportError(SP_ERROR_INSTRUCTION_PARAM);
    return false;
  }
  return true;
}

bool
MethodVerifier::verifyMemAmount(cell_t amount)
{
  if (amount < 0 || size_t(amount) > memSize_) {
    reportError(SP_ERROR_INSTRUCTION_PARAM);
    return false;
  }
  return true;
}

void
MethodVerifier::collectExternalFuncRefs(const ExternalFuncRefCallback& callback)
{
  collect_func_refs_ = callback;
}

void
MethodVerifier::reportError(int err)
{
  // Break here to find why verification failed.
  error_ = err;
}

} // namespace sp
