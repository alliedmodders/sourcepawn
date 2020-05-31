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
#ifndef _include_sourcepawn_vm_assembler_x64_h__
#define _include_sourcepawn_vm_assembler_x64_h__

#include <assembler.h>
#include <amtl/am-platform.h>
#include <amtl/am-assert.h>
#include <amtl/am-vector.h>

namespace sp {

struct Register
{
  const char* name() const {
    static const char* names[] = {
      "rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi",
      "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"
    };
    return names[code];
  }

  int code;

  bool operator == (const Register& other) const {
    return code == other.code;
  }
  bool operator != (const Register& other) const {
    return code != other.code;
  }

  // If an extended register (r8-r15), returns 1. Otherwise returns 0.
  uint8_t rex_bit() const {
    assert(code <= 15);
    return uint8_t(code) >> 3;
  }

  // Return the low bits used for encoding reg/rm fields.
  uint8_t low_bits() const {
    return uint8_t(code) & 0x7;
  }
};

struct FloatRegister
{
  const char* name() const {
    static const char* names[] = {
      "xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7",
      "xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15"
    };
    return names[code];
  }

  int code;

  bool operator == (const FloatRegister& other) const {
    return code == other.code;
  }
  bool operator != (const FloatRegister& other) const {
    return code != other.code;
  }

  // If an extended register (xmm8-xmm15), returns 1. Otherwise returns 0.
  uint8_t rex_bit() const {
    assert(code <= 15);
    return uint8_t(code) >> 3;
  }
};

const Register rax = { 0 };
const Register rcx = { 1 };
const Register rdx = { 2 };
const Register rbx = { 3 };
const Register rsp = { 4 };
const Register rbp = { 5 };
const Register rsi = { 6 };
const Register rdi = { 7 };
const Register r8 = { 8 };
const Register r9 = { 9 };
const Register r10 = { 10 };
const Register r11 = { 11 };
const Register r12 = { 12 };
const Register r13 = { 13 };
const Register r14 = { 14 };
const Register r15 = { 15 };

const FloatRegister xmm0 = { 0 };
const FloatRegister xmm1 = { 1 };
const FloatRegister xmm2 = { 2 };
const FloatRegister xmm3 = { 3 };
const FloatRegister xmm4 = { 4 };
const FloatRegister xmm5 = { 5 };
const FloatRegister xmm6 = { 6 };
const FloatRegister xmm7 = { 7 };
const FloatRegister xmm8 = { 8 };
const FloatRegister xmm9 = { 9 };
const FloatRegister xmm10 = { 10 };
const FloatRegister xmm11 = { 11 };
const FloatRegister xmm12 = { 12 };
const FloatRegister xmm13 = { 13 };
const FloatRegister xmm14 = { 14 };
const FloatRegister xmm15 = { 15 };

#if defined(KE_WINDOWS)
static const Register ArgReg0 = rcx;
static const Register ArgReg1 = rdx;
static const Register ArgReg2 = r8;
static const Register ArgReg3 = r9;
#else
static const Register ArgReg0 = rdi;
static const Register ArgReg1 = rsi;
static const Register ArgReg2 = rdx;
static const Register ArgReg3 = rcx;
static const Register ArgReg4 = r8;
static const Register ArgReg5 = r9;
#endif

enum ConditionCode {
  overflow,
  no_overflow,
  below,
  not_below,
  equal,
  not_equal,
  not_above,
  above,
  negative,
  not_negative,
  even_parity,
  odd_parity,
  less,
  not_less,
  not_greater,
  greater,

  zero = equal,
  not_zero = not_equal,
  less_equal = not_greater,
  below_equal = not_above,
  greater_equal = not_less,
  above_equal = not_below,
  parity = even_parity,
  not_parity = odd_parity
};

enum Scale {
  NoScale,
  ScaleTwo,
  ScaleFour,
  ScaleEight,
  ScalePointer = ScaleEight
};

static const uint8_t kModeDisp0 = 0;
static const uint8_t kModeDisp8 = 1;
static const uint8_t kModeDisp32 = 2;
static const uint8_t kModeReg = 3;
static const Register kSIB = rsp;
static const Register kNoIndex = rsp;

// For expressing addresses as memory R/Ms without using Operand.
class AddressOperand
{
 public:
  explicit AddressOperand(void* ptr)
   : address_(ptr)
  {}

  const AddressValue& asValue() const {
    return address_;
  }
  intptr_t asIntPtr() const {
    return address_.value();
  }
  bool has32BitEncoding() const {
    return address_.has32BitEncoding();
  }

 private:
  AddressValue address_;
};

struct Operand
{
  friend class Assembler;
  friend class MacroAssembler;

 public:
  Operand(Register reg, int32_t disp)
   : rex_bits_(0),
     length_(0)
  {
    if (reg == rsp || reg == r12) {
      // If the reg is rsp/r12, we need a SIB encoding.
      if (disp == 0)
        sib_disp0(NoScale, kNoIndex, reg);
      else if (disp >= SCHAR_MIN && disp <= SCHAR_MAX)
        sib_disp8(NoScale, kNoIndex, reg, disp);
      else
        sib_disp32(NoScale, kNoIndex, reg, disp);
    } else if (disp == 0 && reg != rbp && reg != r13) {
      // note, [ebp/r13+0] is disp32/rip
      modrm_disp0(reg);
    } else if (disp >= SCHAR_MIN && disp <= SCHAR_MAX) {
      modrm_disp8(reg, disp);
    } else {
      modrm_disp32(reg, disp);
    }
  }

  Operand(Register base, Scale scale, int32_t disp = 0)
   : rex_bits_(0),
     length_(0)
  {
    if (disp == 0 && base != rbp && base != r13)
      sib_disp0(scale, kNoIndex, base);
    else if (disp >= SCHAR_MIN && disp <= SCHAR_MAX)
      sib_disp8(scale, kNoIndex, base, disp);
    else
      sib_disp32(scale, kNoIndex, base, disp);
  }

  Operand(Register base, Register index, Scale scale, int32_t disp = 0)
   : rex_bits_(0),
     length_(0)
  {
    assert(index != kNoIndex);
    if (disp == 0 && base != rbp && base != r13)
      sib_disp0(scale, index, base);
    else if (disp >= SCHAR_MIN && disp <= SCHAR_MAX)
      sib_disp8(scale, index, base, disp);
    else
      sib_disp32(scale, index, base, disp);
  }

  uint8_t getByte(size_t index) const {
    assert(index < length());
    return bytes_[index];
  }

  size_t length() const {
    assert(length_);
    return length_;
  }

  uint64_t rex_bits() const {
    return rex_bits_;
  }

 private:
  explicit Operand(Register reg)
   : length_(0)
  {
    modrm(kModeReg, reg);
  }
  explicit Operand(const AddressValue& address)
   : rex_bits_(0)
  {
    // Callers must ensure this is encodable as a 32-bit address.
    KE_RELEASE_ASSERT(address.has32BitEncoding());
    sib(kModeDisp0, NoScale, kNoIndex, rbp);
    *reinterpret_cast<int32_t*>(bytes_ + 2) = int32_t(address.value());
    length_ = 6;
  }

  void modrm(uint8_t mode, Register rm) {
    assert(mode <= 3);
    bytes_[0] = (mode << 6) | rm.low_bits();
    rex_bits_ |= rm.rex_bit();
    length_ = 1;
  }
  void modrm_disp0(Register rm) {
    modrm(kModeDisp0, rm);
  }
  void modrm_disp8(Register rm, int8_t disp) {
    modrm(kModeDisp8, rm);
    bytes_[1] = disp;
    length_ = 2;
  }
  void modrm_disp32(Register rm, int32_t disp) {
    modrm(kModeDisp32, rm);
    *reinterpret_cast<int32_t*>(bytes_ + 1) = disp;
    length_ = 5;
  }
  void sib(uint8_t mode, Scale scale, Register index, Register base) {
    modrm(mode, kSIB);
    bytes_[1] = (uint8_t(scale) << 6) | (index.low_bits() << 3) | base.low_bits();

    // rm and sib base are mutually exclusive, so the rm rex bit should be 0.
    rex_bits_ |= (index.rex_bit() << 1) | base.rex_bit();
  }
  void sib_disp0(Scale scale, Register index, Register base) {
    sib(kModeDisp0, scale, index, base);
    length_ = 2;
  }
  void sib_disp8(Scale scale, Register index, Register base, int8_t disp) {
    sib(kModeDisp8, scale, index, base);
    bytes_[2] = disp;
    length_ = 3;
  }
  void sib_disp32(Scale scale, Register index, Register base, int32_t disp) {
    sib(kModeDisp32, scale, index, base);
    *reinterpret_cast<int32_t*>(bytes_ + 2) = disp;
    length_ = 6;
  }

 private:
  uint8_t rm() const {
    return bytes_[0] & 7;
  }
  uint8_t mode() const {
    return bytes_[0] >> 6;
  }

 private:
  uint8_t bytes_[6];
  uint8_t rex_bits_;
  uint8_t length_;
};

class Assembler : public AssemblerBase
{
 public:
  void emitToExecutableMemory(void* code);

  void bind(Label* target) {
    if (outOfMemory()) {
      // If we ran out of memory, the code stream is potentially invalid and
      // we cannot use the embedded linked list.
      target->bind(pc());
      return;
    }

    assert(!target->bound());
    uint32_t status = target->status();
    while (Label::More(status)) {
      // Grab the offset. It should be at least a 1byte op + rel32.
      uint32_t offset = Label::ToOffset(status);
      assert(offset >= 5);

      // Grab the delta from target to pc.
      ptrdiff_t delta = pos_ - (buffer() + offset);
      assert(delta >= INT_MIN && delta <= INT_MAX);

      int32_t* p = reinterpret_cast<int32_t*>(buffer() + offset - 4);
      status = *p;
      *p = static_cast<int32_t>(delta);
    }
    target->bind(pc());
  }
  void bind(CodeLabel* label) {
    if (outOfMemory())
      return;
    if (label->used()) {
      uint32_t offset = OffsetLabel::ToOffset(label->status());
      *reinterpret_cast<uint64_t*>(buffer() + offset - 8) = pc();
    }
    label->bind(pc());
  }

  void call(Label* dest) {
    emit1(0xe8);
    emitJumpTarget(dest);
  }
  void call(Register reg) {
    emit1(0xff, 2, reg);
  }
  void leave() {
    emit1(0xc9);
  }
  void ret() {
    emit1(0xc3);
  }

  void breakpoint() {
    emit1(0xcc);
  }

  void jmp(Label* dest) {
    int8_t d8;
    if (canEmitSmallJump(dest, &d8)) {
      emit2(0xeb, d8);
    } else {
      emit1(0xe9);
      emitJumpTarget(dest);
    }
  }
  template <typename T>
  void jmp(const T& target) {
    emit1(0xff, 4, target);
  }

  void j(ConditionCode cc, Label* dest) {
    int8_t d8;
    if (canEmitSmallJump(dest, &d8)) {
      emit2(0x70 + uint8_t(cc), d8);
    } else {
      emit2(0x0f, 0x80 + uint8_t(cc));
      emitJumpTarget(dest);
    }
  }

  void push(Register reg) {
    emit1_maybe_rex(0x50 + reg.low_bits(), reg);
  }
  void push(int32_t imm) {
    if (imm >= SCHAR_MIN && imm <= SCHAR_MAX) {
      emit2(0x6a, int8_t(imm));
    } else {
      emit1(0x68);
      writeInt32(imm);
    }
  }

  void pop(Register reg) {
    emit1_maybe_rex(0x58 + reg.low_bits(), reg);
  }

  void leaq(Register dest, const Operand& src) {
    emit1_64(0x8d, dest, src);
  }

  void movq(Register dest, Register src) {
    emit1_64(0x8b, dest, src);
  }
  void movq(Register dest, const Operand& src) {
    emit1_64(0x8b, dest, src);
  }
  void movq(const Operand& src, Register dest) {
    emit1_64(0x89, dest, src);
  }
  void movq(Register dest, intptr_t value) {
    if (value >= 0 && value <= UINT32_MAX) {
      // Do a truncated mov; this will zero-extend.
      movl(dest, int32_t(value));
    } else if (value >= INT_MIN && value <= INT_MAX) {
      // Perform a sign-extended move.
      emit1_64(0xc7, 1, dest);
    } else {
      // Do a full 64-bit move.
      emit1_64_rex(0xb8 + dest.low_bits(), dest);
      writeInt64(value);
    }
  }
  void movq(Register dest, CodeLabel* src, PatchLabel* patch = nullptr) {
    emit1_64_rex(0xb8 + dest.low_bits(), dest);
    if (!src->bound()) {
      writeInt64(0xfedcba987654321f);
      src->use(pc());
    } else {
      writeInt64(src->offset());
    }
    if (patch)
      bind(patch);
    absolute_code_refs_.push_back(pc());
  }
  void movl(Register dest, int32_t value) {
    emit1_maybe_rex(0xb8 + dest.low_bits(), dest);
    writeInt32(value);
  }
  void movl(const Operand& dest, int32_t value) {
    emit1(0xc7, 0, dest);
    writeInt32(value);
  }
  void movq(Register dest, const AddressValue& address) {
    movq(dest, address.value());
  }
  void movl(Register dest, const Operand& src) {
    emit1(0x8b, dest, src);
  }
  template <typename T>
  void movl(const T& src, Register dest) {
    emit1(0x89, dest, src);
  }

  void addq(Register dest, Register src) {
    emit1_64(0x01, src, dest);
  }
  template <typename T>
  void addq(const T& rm, int32_t imm) {
    alu_imm_64(0, imm, rm);
  }
  template <typename T>
  void addl(const T& rm, int32_t imm) {
    alu_imm_32(0, imm, rm);
  }

  void subq(Register dest, Register src) {
    emit1_64(0x29, src, dest);
  }
  template <typename T>
  void subq(const T& rm, int32_t imm) {
    alu_imm_64(5, imm, rm);
  }

  template <typename T>
  void andq(const T& rm, int32_t imm) {
    alu_imm_64(4, imm, rm);
  }

  template <typename T>
  void testq(const T& left, Register right) {
    emit1_64(0x85, right, left);
  }
  template <typename T>
  void testl(const T& left, Register right) {
    emit1(0x85, right, left);
  }
  void testq(Register left, int32_t imm) {
    if (left == rax)
      emit1_64(0xa9);
    else
      emit1_64(0xf7, 0, left);
    writeInt32(imm);
  }
  void testq(const Operand& left, int32_t imm) {
    emit1_64(0xf7, 0, left);
    writeInt32(imm);
  }

  template <typename T>
  void cmpq(const T& left, Register right) {
    emit1_64(0x39, right, left);
  }
  template <typename T>
  void cmpq(Register left, const Operand& right) {
    emit1_64(0x3b, left, right);
  }
  template <typename T>
  void cmpl(const T& left, Register right) {
    emit1(0x39, right, left);
  }
  template <typename T>
  void cmpl(Register left, const Operand& right) {
    emit1(0x3b, left, right);
  }
  template <typename T>
  void cmpl(const T& left, int32_t imm) {
    alu_imm_32(7, imm, left);
  }

  template <typename T>
  void xorq(const T& left, Register right) {
    emit1_64(0x31, right, left);
  }

 protected:
  // If address does not fit in a 32-bit value, src must be rax.
  void movq(const AddressOperand& address, Register src) {
    if (src == rax) {
      emit1_64(0xa3);
      writeInt64(address.asIntPtr());
    } else {
      movq(Operand(address.asValue()), src);
    }
  }
  void movq(Register dest, const AddressOperand& src) {
    if (dest == rax) {
      emit1_64(0xa1);
      writeInt64(src.asIntPtr());
    } else {
      movq(dest, Operand(src.asValue()));
    }
  }

 private:
  bool canEmitSmallJump(Label* dest, int8_t* deltap) {
    if (!dest->bound())
      return false;

    // All small jumps are assumed to be 2 bytes.
    ptrdiff_t delta = ptrdiff_t(dest->offset()) - (position() + 2);
    if (delta < SCHAR_MIN || delta > SCHAR_MAX)
      return false;
    *deltap = static_cast<int8_t>(delta);
    return true;
  }
  void emitJumpTarget(Label* dest) {
    if (dest->bound()) {
      ptrdiff_t delta = ptrdiff_t(dest->offset()) - (position() + 4);
      assert(delta >= INT_MIN && delta <= INT_MAX);
      writeInt32(static_cast<int32_t>(delta));
    } else {
      writeUint32(dest->addPending(position() + 4));
    }
  }

  void alu_imm_64(uint8_t r, int32_t imm, Register rm) {
    if (imm >= SCHAR_MIN && imm <= SCHAR_MAX) {
      emit1_64(0x83, r, rm);
      *pos_++ = uint8_t(imm & 0xff);
    } else if (rm == rax) {
      emit1(0x05 | (r << 3));
      writeInt32(imm);
    } else {
      emit1_64(0x81, r, rm);
      writeInt32(imm);
    }
  }
  void alu_imm_64(uint8_t r, int32_t imm, const Operand& rm) {
    if (imm >= SCHAR_MIN && imm <= SCHAR_MAX) {
      emit1_64(0x83, r, rm);
      *pos_++ = uint8_t(imm & 0xff);
    } else {
      emit1_64(0x81, r, rm);
      writeInt32(imm);
    }
  }
  void alu_imm_32(uint8_t r, int32_t imm, Register rm) {
    if (imm >= SCHAR_MIN && imm <= SCHAR_MAX) {
      emit1(0x83, r, rm);
      *pos_++ = uint8_t(imm & 0xff);
    } else if (rm == rax) {
      emit1(0x05 | (r << 3));
      writeInt32(imm);
    } else {
      emit1(0x81, r, rm);
      writeInt32(imm);
    }
  }
  void alu_imm_32(uint8_t r, int32_t imm, const Operand& rm) {
    if (imm >= SCHAR_MIN && imm <= SCHAR_MAX) {
      emit1(0x83, r, rm);
      *pos_++ = uint8_t(imm & 0xff);
    } else {
      emit1(0x81, r, rm);
      writeInt32(imm);
    }
  }

  // Instructions can fall into one or more of the following categories, and
  // we slice up helpers to cover them all:
  //  - REX prefix definitely needed (64-bit operand size).
  //  - REX prefix maybe needed.
  //  - Non-standard R/M (for example push/pop).
  //  - Non-register "opreg" (for example call /2).

  // Emit a 64-bit with instruction with REX prefix.
  template <typename T>
  void emit1_64(uint8_t opcode, Register opreg, const T& rm) {
    ensureSpace();
    emit_rex_64(opreg, rm);
    emit1_tail(opcode, opreg, rm);
  }
  template <typename T>
  void emit1_64(uint8_t opcode, uint8_t opreg, const T& rm) {
    ensureSpace();
    emit_rex_64(rm);
    emit1_tail(opcode, opreg, rm);
  }
  // Emit a precomputed opcode that might need a rex adjustment.
  void emit1_64_rex(uint8_t opcode, Register rex_rm) {
    ensureSpace();
    emit_rex_64(rex_rm);
    *pos_++ = opcode;
  }
  // Emit a single precomputed byte with rex.
  void emit1_64(uint8_t opcode) {
    ensureSpace();
    *pos_++ = 0x48;
    *pos_++ = opcode;
  }
  // Emit a 32-bit or neutral instruction that might need a REX prefix.
  template <typename RMType>
  void emit1(uint8_t opcode, Register opreg, const RMType& rm) {
    ensureSpace();
    maybe_emit_rex(opreg, rm);
    emit1_tail(opcode, opreg, rm);
  }
  template <typename RMType>
  void emit1(uint8_t opcode, uint8_t opreg, const RMType& rm) {
    ensureSpace();
    maybe_emit_rex(rm);
    emit1_tail(opcode, opreg, rm);
  }
  // Emit a precomputed opcode that might need a rex prefix.
  void emit1_maybe_rex(uint8_t opcode, Register rex_rm) {
    ensureSpace();
    maybe_emit_rex(rex_rm);
    *pos_++ = opcode;
  }
  // Emit a single precomputed byte.
  void emit1(uint8_t opcode) {
    ensureSpace();
    *pos_++ = opcode;
  }

  // Emit two precomputed bytes.
  void emit2(uint8_t prefix, uint8_t opcode) {
    ensureSpace();
    *pos_++ = prefix;
    *pos_++ = opcode;
  }

  // Helpers.
  template <typename RegType, typename RMType>
  void emit1_tail(uint8_t opcode, const RegType& opreg, const RMType& rm) {
    *pos_++ = opcode;
    emit_modrm(opreg, rm);
  }

  // W=1, R=0, X=0, B=?
  void emit_rex_64(Register rm) {
    *pos_++ = (0x48 | rm.rex_bit());
  }
  // W=1, R=0, X=?, B=?
  void emit_rex_64(const Operand& rm) {
    *pos_++ = static_cast<uint8_t>(0x48 | rm.rex_bits());
  }
  // W=1, R=?, X=0, B=?
  void emit_rex_64(Register opreg, Register rm) {
    *pos_++ = (0x48 | (opreg.rex_bit() << 2) | rm.rex_bit());
  }
  // W=1, R=?, X=?, B=?
  void emit_rex_64(Register opreg, const Operand& rm) {
    *pos_++ = static_cast<uint8_t>(0x48 | (opreg.rex_bit() << 2) | rm.rex_bits());
  }
  // W=0, R=?, X=0, B=?
  void maybe_emit_rex(Register opreg, Register rm) {
    uint8_t bits = (opreg.rex_bit() << 2) | rm.rex_bit();
    if (bits)
      *pos_++ = 0x40 | bits;
  }
  // W=0, R=?, X=?, B=?
  void maybe_emit_rex(Register opreg, const Operand& rm) {
    uint8_t bits = static_cast<uint8_t>((opreg.rex_bit() << 2) | rm.rex_bits());
    if (bits)
      *pos_++ = 0x40 | bits;
  }
  // W=0, R=0, X=0, B=?
  void maybe_emit_rex(Register rm) {
    if (rm.rex_bit())
      *pos_++ = 0x41;
  }
  // W=0, R=0, X=?, B=?
  void maybe_emit_rex(const Operand& rm) {
    if (rm.rex_bits())
      *pos_++ = static_cast<uint8_t>(0x40 | rm.rex_bits());
  }

  // ModR/M encoding.
  void emit_modrm(Register opreg, const Register& rm) {
    emit_modrm(opreg.low_bits(), rm.low_bits());
  }
  void emit_modrm(uint8_t opreg, const Register& rm) {
    emit_modrm(opreg, rm.low_bits());
  }
  void emit_modrm(uint8_t opreg, uint8_t rm) {
    *pos_++ = (kModeReg << 6) | (opreg << 3) | rm;
  }
  void emit_modrm(Register opreg, const Operand& operand) {
    emit_modrm(opreg.low_bits(), operand);
  }
  void emit_modrm(uint8_t opreg, const Operand& operand) {
    *pos_++ = operand.getByte(0) | (opreg << 3);
    size_t length = operand.length();
    for (size_t i = 1; i < length; i++)
      *pos_++ = operand.getByte(i);
  }

 private:
  std::vector<uint32_t> absolute_code_refs_;
};

} // namespace sp

#endif // _include_sourcepawn_vm_assembler_x64_h__
