// vim: set ts=8 sts=2 sw=2 tw=99 et:
//
// This file is part of SourcePawn.
// 
// SourcePawn is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
// 
// SourcePawn is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with SourcePawn.  If not, see <http://www.gnu.org/licenses/>.
//
#include <fenv.h>
#include <math.h>
#include <stdlib.h>

#include <utility>

#include "interpreter.h"
#include "debugging.h"
#include "environment.h"
#include "method-info.h"
#include "plugin-context.h"
#include "pcode-reader.h"
#include "runtime-helpers.h"
#include "watchdog_timer.h"
#include <amtl/am-float.h>

namespace sp {

bool
Interpreter::Run(PluginContext* cx, RefPtr<MethodInfo> method, cell_t* rval)
{
  Interpreter interpreter(cx, method);
  if (!interpreter.run())
    return false;

  *rval = interpreter.return_value();
  return true;
}

Interpreter::Interpreter(PluginContext* cx, RefPtr<MethodInfo> method)
 : env_(Environment::get()),
   rt_(cx->runtime()),
   cx_(cx),
   reader_(rt_, method->pcode_offset(), this),
   method_(method),
   has_returned_(false),
   return_value_(0)
{
}

bool
Interpreter::run()
{
  assert(reader_.peekOpcode() == OP_PROC);

  InterpInvokeFrame ivk(cx_, method_, reader_.cip());
  ke::SaveAndSet<InterpInvokeFrame*> enterIvk(&ivk_, &ivk);

  reader_.begin();

  if (!cx_->pushAmxFrame())
    return false;

  while (!has_returned_ && reader_.more()) {
    if (reader_.peekOpcode() == OP_PROC || reader_.peekOpcode() == OP_ENDPROC)
      break;
    if (!reader_.visitNext())
      return false;
  }

  return true;
}

bool
Interpreter::invokeNative(uint32_t native_index)
{
  NativeEntry* native = rt_->NativeAt(native_index);

  ivk_->enterNativeCall(native_index);
  if (native->status == SP_NATIVE_BOUND) {
    ke::SaveAndSet<cell_t> saveSp(cx_->addressOfSp(), cx_->sp());
    ke::SaveAndSet<cell_t> saveHp(cx_->addressOfHp(), cx_->hp());

    const cell_t* params = reinterpret_cast<const cell_t*>(cx_->memory() + cx_->sp());

    if (native->legacy_fn)
      regs_.pri() = native->legacy_fn(cx_, params);
    else
      regs_.pri() = native->callback->Invoke(cx_, params);
  } else {
    cx_->ReportErrorNumber(SP_ERROR_INVALID_NATIVE);
  }
  ivk_->leaveNativeCall();

  return !env_->hasPendingException();
}

bool
Interpreter::visitRETN()
{
  if (!cx_->popAmxFrame())
    return false;

  has_returned_ = true;
  return_value_ = regs_.pri();
  return true;
}

bool
Interpreter::visitPUSH_C(const cell_t* vals, size_t nvals)
{
  for (size_t i = 0; i < nvals; i++) {
    if (!cx_->pushStack(vals[i]))
      return false;
  }
  return true;
}

bool
Interpreter::visitPUSH_ADR(const cell_t* offsets, size_t nvals)
{
  for (size_t i = 0; i < nvals; i++) {
    cell_t address = cx_->frm() + offsets[i];
    if (!cx_->pushStack(address))
      return false;
  }
  return true;
}

bool
Interpreter::visitPUSH(const cell_t* addresses, size_t nvals)
{
  for (size_t i = 0; i < nvals; i++) {
    cell_t value;
    if (!cx_->getCellValue(addresses[i], &value))
      return false;
    if (!cx_->pushStack(value))
      return false;
  }
  return true;
}

bool
Interpreter::visitCALL(cell_t offset)
{
  RefPtr<MethodInfo> target = cx_->runtime()->AcquireMethod(offset);
  if (!target) {
    cx_->ReportErrorNumber(SP_ERROR_INVALID_ADDRESS);
    return false;
  }
  int err = target->Validate();
  if (err != SP_ERROR_NONE) {
    cx_->ReportErrorNumber(err);
    return false;
  }

  // We don't interleave between the interpreter and JIT (yet).
  cell_t value = 0;
  if (!Run(cx_, target, &value))
    return false;

  regs_.pri() = value;
  return true;
}

bool
Interpreter::visitHEAP(cell_t amount)
{
  return cx_->heapAlloc(amount, &regs_.alt());
}

bool
Interpreter::visitLOAD_I()
{
  return cx_->getCellValue(regs_.pri(), &regs_.pri());
}

bool
Interpreter::visitSTOR_I()
{
  return cx_->setCellValue(regs_.alt(), regs_.pri());
}

bool
Interpreter::visitPUSH(PawnReg src)
{
  return cx_->pushStack(regs_[src]);
}

bool
Interpreter::visitPOP(PawnReg dest)
{
  return cx_->popStack(&regs_[dest]);
}

bool
Interpreter::visitSYSREQ_C(uint32_t native_index)
{
  return invokeNative(native_index);
}

bool
Interpreter::visitSYSREQ_N(uint32_t native_index, uint32_t nparams)
{
  if (!cx_->pushStack(nparams))
    return false;
  if (!invokeNative(native_index))
    return false;

  cell_t ignore;
  for (size_t i = 0; i < nparams + 1; i++) {
    if (!cx_->popStack(&ignore))
      return false;
  }
  return true;
}

bool
Interpreter::visitZERO(PawnReg dest)
{
  regs_[dest] = 0;
  return true;
}

bool
Interpreter::visitZERO(cell_t address)
{
  return cx_->setCellValue(address, 0);
}

bool
Interpreter::visitZERO_S(cell_t offset)
{
  return cx_->setFrameValue(offset, 0);
}

bool
Interpreter::visitSTACK(cell_t amount)
{
  return cx_->addStack(amount);
}

bool
Interpreter::visitPUSH_S(const cell_t* offsets, size_t nvals)
{
  for (size_t i = 0; i < nvals; i++) {
    cell_t value;
    if (!cx_->getFrameValue(offsets[i], &value))
      return false;
    if (!cx_->pushStack(value))
      return false;
  }
  return true;
}

bool
Interpreter::visitCONST(PawnReg dest, cell_t imm)
{
  regs_[dest] = imm;
  return true;
}

bool
Interpreter::visitCONST(cell_t address, cell_t value)
{
  return cx_->setCellValue(address, value);
}

bool
Interpreter::visitCONST_S(cell_t offset, cell_t value)
{
  return cx_->setFrameValue(offset, value);
}

bool
Interpreter::visitLOAD_S(PawnReg dest, cell_t srcoffs)
{
  return cx_->getFrameValue(srcoffs, &regs_[dest]);
}

bool
Interpreter::visitSTOR_S(cell_t offset, PawnReg src)
{
  return cx_->setFrameValue(offset, regs_[src]);
}

bool
Interpreter::visitJUMP(cell_t offset)
{
  if (offset < reader_.cip_offset()) {
    // Check the watchdog timer if we're looping backwards.
    if (!Environment::get()->watchdog()->HandleInterrupt()) {
      cx_->ReportErrorNumber(SP_ERROR_TIMEOUT);
      return false;
    }
  }

  reader_.jump(offset);
  return true;
}

bool
Interpreter::visitJcmp(CompareOp op, cell_t offset)
{
  bool jump = false;
  switch (op) {
  case CompareOp::Zero:
    jump = regs_.pri() == 0;
    break;
  case CompareOp::NotZero:
    jump = regs_.pri() != 0;
    break;
  case CompareOp::Eq:
    jump = regs_.pri() == regs_.alt();
    break;
  case CompareOp::Neq:
    jump = regs_.pri() != regs_.alt();
    break;
  case CompareOp::Sless:
    jump = regs_.pri() < regs_.alt();
    break;
  case CompareOp::Sleq:
    jump = regs_.pri() <= regs_.alt();
    break;
  case CompareOp::Sgrtr:
    jump = regs_.pri() > regs_.alt();
    break;
  case CompareOp::Sgeq:
    jump = regs_.pri() >= regs_.alt();
    break;
  default:
    assert(false);
  }

  if (jump) {
    if (offset < reader_.cip_offset()) {
      // Check the watchdog timer if we're looping backwards.
      if (!Environment::get()->watchdog()->HandleInterrupt()) {
        cx_->ReportErrorNumber(SP_ERROR_TIMEOUT);
        return false;
      }
    }

    reader_.jump(offset);
  }

  return true;
}

bool
Interpreter::visitADD_C(cell_t value)
{
  regs_.pri() += value;
  return true;
}

bool
Interpreter::visitSMUL_C(cell_t value)
{
  regs_.pri() *= value;
  return true;
}

bool
Interpreter::visitADD()
{
  regs_.pri() += regs_.alt();
  return true;
}

bool
Interpreter::visitNOT()
{
  regs_.pri() = regs_.pri() ? 0 : 1;
  return true;
}

bool
Interpreter::visitNEG()
{
  regs_.pri() = -regs_.pri();
  return true;
}

bool
Interpreter::visitINVERT()
{
  regs_.pri() = ~regs_.pri();
  return true;
}

bool
Interpreter::visitINC(PawnReg dest)
{
  regs_[dest] += 1;
  return true;
}

bool
Interpreter::visitINC(cell_t address)
{
  cell_t* addr = cx_->throwIfBadAddress(address);
  if (!addr)
    return false;
  *addr += 1;
  return true;
}

bool
Interpreter::visitINC_S(cell_t offset)
{
  cell_t value;
  if (!cx_->getFrameValue(offset, &value))
    return false;
  return cx_->setFrameValue(offset, value + 1);
}

bool
Interpreter::visitINC_I()
{
  cell_t* addr = cx_->throwIfBadAddress(regs_.pri());
  if (!addr)
    return false;
  *addr += 1;
  return true;
}

bool
Interpreter::visitDEC(PawnReg dest)
{
  regs_[dest] -= 1;
  return true;
}

bool
Interpreter::visitDEC(cell_t address)
{
  cell_t* addr = cx_->throwIfBadAddress(address);
  if (!addr)
    return false;
  *addr -= 1;
  return true;
}

bool
Interpreter::visitDEC_S(cell_t offset)
{
  cell_t value;
  if (!cx_->getFrameValue(offset, &value))
    return false;
  return cx_->setFrameValue(offset, value - 1);
}

bool
Interpreter::visitDEC_I()
{
  cell_t* addr = cx_->throwIfBadAddress(regs_.pri());
  if (!addr)
    return false;
  *addr -= 1;
  return true;
}

bool
Interpreter::visitLOAD_BOTH(cell_t addressForPri, cell_t addressForAlt)
{
  if (!cx_->getCellValue(addressForPri, &regs_.pri()))
    return false;
  if (!cx_->getCellValue(addressForAlt, &regs_.alt()))
    return false;
  return true;
}

bool
Interpreter::visitLOAD_S_BOTH(cell_t offsetForPri, cell_t offsetForAlt)
{
  if (!cx_->getFrameValue(offsetForPri, &regs_.pri()))
    return false;
  if (!cx_->getFrameValue(offsetForAlt, &regs_.alt()))
    return false;
  return true;
}

bool
Interpreter::visitAND()
{
  regs_.pri() &= regs_.alt();
  return true;
}

bool
Interpreter::visitOR()
{
  regs_.pri() |= regs_.alt();
  return true;
}

bool
Interpreter::visitXOR()
{
  regs_.pri() ^= regs_.alt();
  return true;
}

bool
Interpreter::visitSUB()
{
  regs_.pri() -= regs_.alt();
  return true;
}

bool
Interpreter::visitSUB_ALT()
{
  regs_.pri() = regs_.alt() - regs_.pri();
  return true;
}

bool
Interpreter::visitSMUL()
{
  regs_.pri() *= regs_.alt();
  return true;
}

bool
Interpreter::visitSDIV(PawnReg dest)
{
  PawnReg dividendReg = dest;
  PawnReg divisorReg = (dest == PawnReg::Pri) ? PawnReg::Alt : PawnReg::Pri;

  cell_t divisor = regs_[divisorReg];
  cell_t dividend = regs_[dividendReg];

  if (divisor == 0) {
    cx_->ReportErrorNumber(SP_ERROR_DIVIDE_BY_ZERO);
    return false;
  }

  // -INT_MIN / -1 is an overflow.
  if (divisor == -1 && dividend == cell_t(0x80000000)) {
    cx_->ReportErrorNumber(SP_ERROR_INTEGER_OVERFLOW);
    return false;
  }

  regs_.pri() = dividend / divisor;
  regs_.alt() = dividend % divisor;
  return true;
}

bool
Interpreter::visitSHL()
{
  regs_.pri() <<= regs_.alt();
  return true;
}

bool
Interpreter::visitSHR()
{
  uint32_t left = regs_.pri();
  uint32_t right = regs_.alt();
  regs_.pri() = left >> right;
  return true;
}

bool
Interpreter::visitSSHR()
{
  regs_.pri() >>= regs_.alt();
  return true;
}

bool
Interpreter::visitSHL_C(PawnReg dest, cell_t amount)
{
  // Only generated without the peephole optimizer.
  regs_[dest] <<= amount;
  return true;
}

bool
Interpreter::visitEQ_C(PawnReg src, cell_t value)
{
  regs_.pri() = (regs_[src] == value) ? 1 : 0;
  return true;
}

bool
Interpreter::visitCompareOp(CompareOp op)
{
  switch (op) {
  case CompareOp::Sgrtr:
    regs_.pri() = (regs_.pri() > regs_.alt()) ? 1 : 0;
    break;
  case CompareOp::Sgeq:
    regs_.pri() = (regs_.pri() >= regs_.alt()) ? 1 : 0;
    break;
  case CompareOp::Sleq:
    regs_.pri() = (regs_.pri() <= regs_.alt()) ? 1 : 0;
    break;
  case CompareOp::Sless:
    regs_.pri() = (regs_.pri() < regs_.alt()) ? 1 : 0;
    break;
  case CompareOp::Eq:
    regs_.pri() = (regs_.pri() == regs_.alt()) ? 1 : 0;
    break;
  case CompareOp::Neq:
    regs_.pri() = (regs_.pri() != regs_.alt()) ? 1 : 0;
    break;
  default:
    assert(false);
  }
  return true;
}

bool
Interpreter::visitADDR(PawnReg dest, cell_t offset)
{
  regs_[dest] = cx_->frm() + offset;
  return true;
}

bool
Interpreter::visitMOVS(uint32_t amount)
{
  cell_t* src = cx_->acquireAddrRange(regs_.pri(), amount);
  if (!src)
    return false;
  cell_t* dest = cx_->acquireAddrRange(regs_.alt(), amount);
  if (!dest)
    return false;
  memmove(dest, src, amount);
  return true;
}

bool
Interpreter::visitFILL(uint32_t amount)
{
  cell_t* dest = cx_->acquireAddrRange(regs_.alt(), amount);
  if (!dest)
    return false;
  for (size_t i = 0; i < (amount / sizeof(cell_t)); i++)
    dest[i] = regs_.pri();
  return true;
}

bool
Interpreter::visitSWITCH(cell_t defaultOffset, const CaseTableEntry* cases, size_t ncases)
{
  for (size_t i = 0; i < ncases; i++) {
    if (cases[i].value == regs_.pri()) {
      reader_.jump(cases[i].address);
      return true;
    }
  }

  reader_.jump(defaultOffset);
  return true;
}

bool
Interpreter::visitBOUNDS(uint32_t limit)
{
  if (size_t(regs_.pri()) > limit) {
    ReportOutOfBoundsError(regs_.pri(), limit);
    return false;
  }
  return true;
}

bool
Interpreter::visitIDXADDR()
{
  regs_.pri() = regs_.alt() + (regs_.pri() * sizeof(cell_t));
  return true;
}

bool
Interpreter::visitLIDX()
{
  cell_t address = regs_.alt() + (regs_.pri() * sizeof(cell_t));
  return cx_->getCellValue(address, &regs_.pri());
}

bool
Interpreter::visitLREF_S(PawnReg dest, cell_t srcoffs)
{
  cell_t address;
  if (!cx_->getFrameValue(srcoffs, &address))
    return false;
  return cx_->getCellValue(address, &regs_[dest]);
}

bool
Interpreter::visitSREF_S(cell_t destoffs, PawnReg src)
{
  cell_t address;
  if (!cx_->getFrameValue(destoffs, &address))
    return false;
  return cx_->setCellValue(address, regs_[src]);
}

bool
Interpreter::visitLODB_I(cell_t width)
{
  if (!cx_->getCellValue(regs_.pri(), &regs_.pri()))
    return false;

  switch (width) {
  case 1:
    regs_.pri() &= 0xff;
    break;
  case 2:
    regs_.pri() &= 0xffff;
    break;
  case 4:
    break;
  default:
    assert(false);
  }
  return true;
}

bool
Interpreter::visitSTRB_I(cell_t width)
{
  cell_t* addr = cx_->throwIfBadAddress(regs_.alt());
  if (!addr)
    return false;

  switch (width) {
  case 1:
    *reinterpret_cast<uint8_t*>(addr) = uint8_t(regs_.pri());
    break;
  case 2:
    *reinterpret_cast<uint16_t*>(addr) = uint16_t(regs_.pri());
    break;
  case 4:
    *addr = regs_.pri();
    break;
  default:
    assert(false);
  }
  return true;
}

bool
Interpreter::visitLOAD(PawnReg dest, cell_t srcaddr)
{
  return cx_->getCellValue(srcaddr, &regs_[dest]);
}

bool
Interpreter::visitSTOR(cell_t address, PawnReg src)
{
  return cx_->setCellValue(address, regs_[src]);
}

bool
Interpreter::visitMOVE(PawnReg reg)
{
  PawnReg other = (reg == PawnReg::Pri) ? PawnReg::Alt : PawnReg::Pri;
  regs_[reg] = regs_[other];
  return true;
}

bool
Interpreter::visitXCHG()
{
  std::swap(regs_.pri(), regs_.alt());
  return true;
}

bool
Interpreter::visitSWAP(PawnReg dest)
{
  cell_t temp = regs_[dest];
  if (!cx_->popStack(&regs_[dest]))
    return false;
  return cx_->pushStack(temp);
}

bool
Interpreter::visitFABS()
{
  if (!cx_->popStack(&regs_.pri()))
    return false;
  regs_.pri() &= 0x7fffffff;
  return true;
}

bool
Interpreter::visitFLOAT()
{
  cell_t value;
  if (!cx_->popStack(&value))
    return false;
  regs_.pri() = sp_ftoc(float(value));
  return true;
}

bool
Interpreter::visitFLOATADD()
{
  cell_t leftVal, rightVal;
  if (!cx_->popStack(&leftVal) || !cx_->popStack(&rightVal))
    return false;
  float left = sp_ctof(leftVal);
  float right = sp_ctof(rightVal);
  regs_.pri() = sp_ftoc(left + right);
  return true;
}

bool
Interpreter::visitFLOATSUB()
{
  cell_t leftVal, rightVal;
  if (!cx_->popStack(&leftVal) || !cx_->popStack(&rightVal))
    return false;
  float left = sp_ctof(leftVal);
  float right = sp_ctof(rightVal);
  regs_.pri() = sp_ftoc(left - right);
  return true;
}

bool
Interpreter::visitFLOATMUL()
{
  cell_t leftVal, rightVal;
  if (!cx_->popStack(&leftVal) || !cx_->popStack(&rightVal))
    return false;
  float left = sp_ctof(leftVal);
  float right = sp_ctof(rightVal);
  regs_.pri() = sp_ftoc(left * right);
  return true;
}

bool
Interpreter::visitFLOATDIV()
{
  cell_t leftVal, rightVal;
  if (!cx_->popStack(&leftVal) || !cx_->popStack(&rightVal))
    return false;
  float left = sp_ctof(leftVal);
  float right = sp_ctof(rightVal);
  regs_.pri() = sp_ftoc(left / right);
  return true;
}

bool
Interpreter::visitRND_TO_NEAREST()
{
  cell_t value;
  if (!cx_->popStack(&value))
    return false;

  int oldmethod = fegetround();
  fesetround(FE_TONEAREST);

  float f = sp_ctof(value);
  regs_.pri() = lrintf(f);

  fesetround(oldmethod);
  return true;
}

bool
Interpreter::visitRND_TO_FLOOR()
{
  cell_t value;
  if (!cx_->popStack(&value))
    return false;

  float f = sp_ctof(value);
  regs_.pri() = int(floor(f));
  return true;
}

bool
Interpreter::visitRND_TO_CEIL()
{
  cell_t value;
  if (!cx_->popStack(&value))
    return false;

  float f = sp_ctof(value);
  regs_.pri() = int(ceil(f));
  return true;
}

bool
Interpreter::visitRND_TO_ZERO()
{
  cell_t value;
  if (!cx_->popStack(&value))
    return false;

  float f = sp_ctof(value);
  if (f >= 0.0f)
    regs_.pri() = int(floor(f));
  else
    regs_.pri() = int(ceil(f));
  return true;
}

bool
Interpreter::visitFLOATCMP()
{
  cell_t leftVal, rightVal;
  if (!cx_->popStack(&leftVal) || !cx_->popStack(&rightVal))
    return false;

  float left = sp_ctof(leftVal);
  float right = sp_ctof(rightVal);

  if (left > right)
    regs_.pri() = 1;
  else if (left < right)
    regs_.pri() = -1;
  else
    regs_.pri() = 0;
  return true;
}

bool
Interpreter::visitFLOAT_CMP_OP(CompareOp op)
{
  cell_t leftVal, rightVal;
  if (!cx_->popStack(&leftVal) || !cx_->popStack(&rightVal))
    return false;

  float left = sp_ctof(leftVal);
  float right = sp_ctof(rightVal);
  if (ke::IsNaN(left) || ke::IsNaN(right)) {
    regs_.pri() = 0;
    return true;
  }

  switch (op) {
  case CompareOp::Eq:
    regs_.pri() = left == right;
    break;
  case CompareOp::Neq:
    regs_.pri() = left != right;
    break;
  case CompareOp::Sless:
    regs_.pri() = left < right;
    break;
  case CompareOp::Sleq:
    regs_.pri() = left <= right;
    break;
  case CompareOp::Sgrtr:
    regs_.pri() = left > right;
    break;
  case CompareOp::Sgeq:
    regs_.pri() = left >= right;
    break;
  default:
    assert(false);
  }

  return true;
}

bool
Interpreter::visitFLOAT_NOT()
{
  cell_t value;
  if (!cx_->popStack(&value))
    return false;

  float f = sp_ctof(value);
  if (ke::IsNaN(f))
    regs_.pri() = 1;
  else
    regs_.pri() = f ? 0 : 1;
  return true;
}

bool
Interpreter::visitGENARRAY(uint32_t dims, bool autozero)
{
  cell_t* stack = cx_->acquireAddrRange(cx_->sp(), dims * sizeof(cell_t));
  if (!stack)
    return false;

  int err = cx_->generateArray(dims, stack, autozero);
  if (err != SP_ERROR_NONE) {
    cx_->ReportErrorNumber(err);
    return false;
  }

  // Remove all but the last argument, which is where the new address is
  // stored.
  cell_t ignore;
  for (size_t i = 0; i < dims - 1; i++) {
    if (!cx_->popStack(&ignore))
      return false;
  }

  return true;
}

bool
Interpreter::visitTRACKER_PUSH_C(cell_t amount)
{
  int err = cx_->pushTracker(amount);
  if (err != SP_ERROR_NONE) {
    cx_->ReportErrorNumber(err);
    return false;
  }
  return true;
}

bool
Interpreter::visitTRACKER_POP_SETHEAP()
{
  int err = cx_->popTrackerAndSetHeap();
  if (err != SP_ERROR_NONE) {
    cx_->ReportErrorNumber(err);
    return false;
  }
  return true;
}

bool
Interpreter::visitSTRADJUST_PRI()
{
  regs_.pri() = (regs_.pri() + 4) >> 2;
  return true;
}

bool
Interpreter::visitBREAK()
{
  // Ignore opcode if this isn't enabled.
  if (!Environment::get()->IsDebugBreakEnabled())
    return true;

  InvokeDebugger(cx_, nullptr);
  return !env_->hasPendingException();
}

bool
Interpreter::visitHALT(cell_t value)
{
  // We don't support this. It's included in the bytestream by default, but it
  // must be unreachable.
  cx_->ReportErrorNumber(SP_ERROR_INVALID_INSTRUCTION);
  return false;
}

bool
Interpreter::visitREBASE(cell_t addr, cell_t iv_size, cell_t data_size)
{
  int err = cx_->rebaseArray(regs_.pri(), addr, iv_size, data_size);
  if (err != SP_ERROR_NONE) {
    cx_->ReportErrorNumber(err);
    return false;
  }
  return true;
}

} // namespace sp
