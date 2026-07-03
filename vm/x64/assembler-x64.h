// vim: set sts=4 ts=8 sw=4 tw=99 et:
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

#include <utility>

#include <amtl/am-assert.h>
#include <amtl/am-platform.h>
#include <amtl/am-vector.h>
#include <assembler.h>

namespace sp {

struct LinkedCode;

struct Register {
    const char* name() const {
        static const char* names[] = {"rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi",
                                      "r8",  "r9",  "r10", "r11", "r12", "r13", "r14", "r15"};
        return names[code];
    }

    int code;

    bool operator==(const Register& other) const {
        return code == other.code;
    }
    bool operator!=(const Register& other) const {
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

struct FloatRegister {
    const char* name() const {
        static const char* names[] = {"xmm0",  "xmm1",  "xmm2",  "xmm3", "xmm4",  "xmm5",
                                      "xmm6",  "xmm7",  "xmm8",  "xmm9", "xmm10", "xmm11",
                                      "xmm12", "xmm13", "xmm14", "xmm15"};
        return names[code];
    }

    int code;

    bool operator==(const FloatRegister& other) const {
        return code == other.code;
    }
    bool operator!=(const FloatRegister& other) const {
        return code != other.code;
    }

    // If an extended register (xmm8-xmm15), returns 1. Otherwise returns 0.
    uint8_t rex_bit() const {
        assert(code <= 15);
        return uint8_t(code) >> 3;
    }

    // Return the low bits used for encoding reg/rm fields.
    uint8_t low_bits() const {
        return uint8_t(code) & 0x7;
    }
};

const Register rax = {0};
const Register rcx = {1};
const Register rdx = {2};
const Register rbx = {3};
const Register rsp = {4};
const Register rbp = {5};
const Register rsi = {6};
const Register rdi = {7};
const Register r8 = {8};
const Register r9 = {9};
const Register r10 = {10};
const Register r11 = {11};
const Register r12 = {12};
const Register r13 = {13};
const Register r14 = {14};
const Register r15 = {15};

const Register r8_al = {0};
const Register r8_cl = {1};
const Register r8_dl = {2};
const Register r8_bl = {3};
const Register r8_ah = {4};
const Register r8_ch = {5};
const Register r8_dh = {6};
const Register r8_bh = {7};

struct RIP {};
constexpr RIP rip{};

const FloatRegister xmm0 = {0};
const FloatRegister xmm1 = {1};
const FloatRegister xmm2 = {2};
const FloatRegister xmm3 = {3};
const FloatRegister xmm4 = {4};
const FloatRegister xmm5 = {5};
const FloatRegister xmm6 = {6};
const FloatRegister xmm7 = {7};
const FloatRegister xmm8 = {8};
const FloatRegister xmm9 = {9};
const FloatRegister xmm10 = {10};
const FloatRegister xmm11 = {11};
const FloatRegister xmm12 = {12};
const FloatRegister xmm13 = {13};
const FloatRegister xmm14 = {14};
const FloatRegister xmm15 = {15};

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

// Trash registers that are not arguments on any platform.
static const Register ClobberReg0 = rax;
static const Register ClobberReg1 = r10;
static const Register ClobberReg2 = r11;

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

enum Scale { NoScale, ScaleTwo, ScaleFour, ScaleEight, ScalePointer = ScaleEight };

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
     : address_(ptr) {
    }

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

struct Operand {
    friend class Assembler;
    friend class MacroAssembler;

  public:
    // Note: this does not allow RIP-based addressing. It automatically handles
    // encoding of SIB bytes when using RBP+0. Instead, use the RIP constructor.
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

    explicit Operand(RIP rip, int32_t offset = 0)
      : rex_bits_(0),
        length_(0)
    {
        modrm_disp0(rbp);
        *reinterpret_cast<int32_t*>(bytes_ + 2) = offset;
        length_ = 5;
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
     : rex_bits_(0),
       length_(0)
    {
        modrm(kModeReg, reg);
    }
    explicit Operand(const AddressValue& address)
     : rex_bits_(0) {
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

    bool isRegister(Register r) const {
        return mode() == kModeReg && rm() == r.code;
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

// For binding rip-relative offsets within code that will never have addresses
// exported or patched. It is only used with the LEA instruction.
class RipLabel : public CodeLabelBase{};
class RipCodeLabel final : public RipLabel{};
class RipDataLabel final : public RipLabel{};

class Assembler : public AssemblerBase
{
  public:
    virtual size_t data_size() const override;

    void emitToExecutableMemory(LinkedCode* code);

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
    void bind(RipCodeLabel* label) {
        if (outOfMemory())
            return;
        if (label->used()) {
            intptr_t orig_rip = RipCodeLabel::ToOffset(label->status());
            intptr_t delta = intptr_t(pc()) - orig_rip;
            assert(delta >= INT_MIN && delta <= INT_MAX);
            *reinterpret_cast<int32_t*>(buffer() + orig_rip - 4) = delta;
        }
        label->bind(pc());
    }
    void bind(RipDataLabel* label) {
        if (outOfMemory())
            return;
        if (label->used()) {
            intptr_t orig_rip = RipCodeLabel::ToOffset(label->status());
            intptr_t delta = -(orig_rip + address_table_.size() * sizeof(uintptr_t));
            assert(delta >= INT_MIN && delta <= INT_MAX);
            *reinterpret_cast<int32_t*>(buffer() + orig_rip - 4) = delta;
        }
        label->bind(pc());
    }
    void bind(PatchCodeLabel* label) {
        if (outOfMemory())
            return;
        if (label->used()) {
            // call [rip + N]
            // We need to patch the address at [rip + N]
            int32_t src_pc = PatchCodeLabel::ToOffset(label->status());

            uint32_t data_index;
            if (src_pc >= 0) {
                assert(src_pc <= code_size());

                uint8_t* base = buffer() + src_pc - 6;
                assert(*base == 0xff);
                assert(*(base + 1) == 0x15);

                // offset = -(pc + data_loc)
                //  : -offset = pc + data_loc
                //  : -offset - pc = data_loc
                int32_t rip_offset = *reinterpret_cast<int32_t*>(base + 2);
                assert(rip_offset != INT_MIN);
                assert(-rip_offset >= src_pc);

                // data_loc = (table_size + 1) * sizeof(uintptr_t)
                //  : (data_loc / sizeof(uintptr_t)) = table_index + 1
                //  : (data_loc / sizeof(uintptr_t)) - 1 = table_index
                uint32_t data_loc = -rip_offset - src_pc;
                assert(data_loc % sizeof(uintptr_t) == 0);
                data_index = (data_loc / sizeof(uintptr_t)) - 1;
            } else {
                data_index = -src_pc - 1;
            }

            // Finally, we've recovered the location to patch.
            for (;;) {
                assert(data_index < address_table_.size());
                int32_t next = int32_t(address_table_[data_index]);
                assert(next <= 0);
                address_table_[data_index] = pc();
                if (!next)
                    break;
                data_index = uint32_t(-next - 1);
            }
        }
        label->bind(pc());
    }

    static void PatchCallThunk(uint8_t* pc, uintptr_t target) {
        // call [rip + N]
        // We need to patch the address at [rip + N]
        uint8_t* base = pc - 6;
        assert(*base == 0xff);
        assert(*(base + 1) == 0x15);

        int32_t offset = *reinterpret_cast<int32_t*>(base + 2);
        uintptr_t* data = reinterpret_cast<uintptr_t*>(pc + offset);
        *data = target;
    }

    void call(Label* dest) {
        emit1(0xe8);
        emitJumpTarget(dest);
    }
    void call(Register reg) {
        emit1(0xff, 2, reg);
    }
    void call(const AddressValue& address) {
        emit1(0xff, 2, Operand(rip));
        emitRipRelativeConstant(address.value());
    }
    void call(PatchCodeLabel* label) {
        emit1(0xff, 2, Operand(rip));
        emitRipRelativeLabel(label);
    }
    void leave() {
        emit1(0xc9);
    }
    void ret() {
        emit1(0xc3);
    }

    // Store the address of a code label in the data section, without an
    // instruction that uses it. This is used for building switch tables.
    void emit_absolute_address(PatchCodeLabel* label) {
        if (label->bound()) {
            address_table_.emplace_back(label->offset());
        } else {
            int32_t prev = label->addPendingUse(-int32_t(address_table_.size() + 1));
            assert(prev <= 0);
            address_table_.emplace_back(uintptr_t(intptr_t(prev)));
        }
        address_table_reloc_.emplace_back((uint32_t)address_table_.size() - 1);
    }

    void cld() {
        emit1(0xfc);
    }
    void rep_movsb() {
        emit2(0xf3, 0xa4);
    }
    void rep_movsd() {
        emit2(0xf3, 0xa5);
    }
    void rep_stosd() {
        emit2(0xf3, 0xab);
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

    void jmp(const AddressValue& address) {
        emit1(0xff, 4, Operand(rip));
        emitRipRelativeConstant(address.value());
    }

    void jmp32(Label* dest) {
        emit1(0xe9);
        emitJumpTarget(dest);
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

    void j32(ConditionCode cc, Label* dest) {
        emit2(0x0f, 0x80 + uint8_t(cc));
        emitJumpTarget(dest);
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

    void lea(Register dest, const Operand& src) {
        emit1_64(0x8d, dest, src);
    }
    void lea(Register dest, RipLabel* src) {
        emit1_64(0x8d, dest, Operand(rip, 0));

        intptr_t rip = pc();
        if (src->bound()) {
            assert(src->offset() < pc());

            intptr_t delta = rip - intptr_t(src->offset());
            assert(delta >= INT_MIN && delta <= INT_MAX);
            *(pos_ - 4) = delta;
        } else {
            src->use(pc());
        }
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
            emit1_64(0xc7, 0, dest);
            writeInt32(value);
        } else {
            // Do a full 64-bit move.
            emit1_64_rex(0xb8 + dest.low_bits(), dest);
            writeInt64(value);
        }
    }
    void movq(Register dest, const AddressValue& address) {
        movq(dest, address.value());
    }
    void movq(Register dest, PatchCodeLabel* src) {
        emit1_64(0x8b, dest, Operand(rip));
        emitRipRelativeLabel(src);
    }
    void movl(Register dest, int32_t value) {
        emit1_maybe_rex(0xb8 + dest.low_bits(), dest);
        writeInt32(value);
    }
    void movl(const Operand& dest, int32_t value) {
        emit1(0xc7, 0, dest);
        writeInt32(value);
    }
    void movl(Register dest, const Operand& src) {
        emit1(0x8b, dest, src);
    }
    template <typename T>
    void movl(const T& src, Register dest) {
        emit1(0x89, dest, src);
    }

    template <typename DestType>
    void movw(const DestType& dest, Register src) {
        emit1(0x89, src, dest);
    }

    template <typename DestType>
    void movb(const DestType& dest, Register src) {
        emit1(0x88, src, dest);
    }

    template <typename T>
    void movsxd(Register dest, const T& src) {
        emit1_64(0x63, dest, src);
    }
    void movzxb(Register dest, const Register src) {
        emit2(0x0f, 0xb6, dest, src);
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
    void addl(Register dest, Register src) {
        emit1(0x01, src, dest);
    }

    void subq(Register dest, Register src) {
        emit1_64(0x29, src, dest);
    }
    template <typename T>
    void subq(const T& rm, int32_t imm) {
        alu_imm_64(5, imm, rm);
    }
    void subl(Register dest, int32_t imm) {
        alu_imm_32(5, imm, dest);
    }
    void subl(Register dest, Register src) {
        emit1(0x29, src, dest);
    }

    void andq(Register dest, Register src) {
        emit1_64(0x21, src, dest);
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
    void cmpq(const T& left, int32_t imm) {
        alu_imm_64(7, imm, left);
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

    template <typename T>
    void xorl(const T& left, Register right) {
        emit1(0x31, right, left);
    }

    void orq(Register dest, Register src) {
        emit1_64(0x09, src, dest);
    }
    void orl(Register dest, Register src) {
        emit1(0x09, src, dest);
    }

    void notq(Register srcdest) {
        emit1_64(0xf7, 2, srcdest);
    }
    void notl(Register srcdest) {
        emit1(0xf7, 2, srcdest);
    }

    void negq(Register srcdest) {
        emit1_64(0xf7, 3, srcdest);
    }
    void negl(Register srcdest) {
        emit1(0xf7, 3, srcdest);
    }

    template <typename T>
    void sarq(const T& dest, uint8_t imm) {
        shift_imm_64(dest, 7, imm);
    }
    void sarq_cl(Register dest) {
        shift_cl_64(dest, 7);
    }
    template <typename T>
    void sarl(const T& dest, uint8_t imm) {
        shift_imm_32(dest, 7, imm);
    }
    void sarl_cl(Register dest) {
        shift_cl_32(dest, 7);
    }

    void shrq_cl(Register dest) {
        shift_cl_64(dest, 5);
    }
    void shrl_cl(Register dest) {
        shift_cl_32(dest, 5);
    }

    void shlq_cl(Register dest) {
        shift_cl_64(dest, 4);
    }
    void shll_cl(Register dest) {
        shift_cl_32(dest, 4);
    }
    void shll(Register dest, uint8_t imm) {
        shift_imm_32(dest, 4, imm);
    }

    void andl(Register dest, int32_t imm) {
        alu_imm_32(4, imm, Operand(dest));
    }
    void andl(const Operand& dest, int32_t imm) {
        alu_imm_32(4, imm, dest);
    }
    void andl(Register dest, Register src) {
        emit1(0x21, src, dest);
    }

    void imulq(Register dest, Register src) {
        emit2_64(0x0f, 0xaf, dest, src);
    }
    void imull(Register dest, Register src) {
        emit2(0x0f, 0xaf, dest, src);
    }
    void imull(Register dest, Register src, int32_t imm) {
        if (imm >= SCHAR_MIN && imm <= SCHAR_MAX) {
            emit1(0x6b, dest, src);
            *pos_++ = imm;
        } else {
            emit1(0x69, dest, src);
            writeInt32(imm);
        }
    }

    void idivq(Register dividend) {
        emit1_64(0xf7, 7, dividend);
    }
    void idivl(Register dividend) {
        emit1(0xf7, 7, dividend);
    }

    void xchgq(Register dest, Register src) {
        if (src == rax)
            emit1_64(0x90 + dest.code);
        else if (dest == rax)
            emit1_64(0x90 + src.code);
        else
            emit1_64(0x87, src.code, dest);
    }

    template <typename DestType>
    void set(ConditionCode cc, const DestType& dest) {
        emit2(0x0f, 0x90 + uint8_t(cc), 0, dest);
    }

    template <typename SrcType>
    void movd(Register dest, const SrcType& src) {
        emit3_sse(0x66, 0x0f, 0x7e, dest, src);
    }
    void movd(FloatRegister dest, Register src) {
        emit3_sse(0x66, 0x0f, 0x6e, dest, src);
    }

    void addss(FloatRegister dest, FloatRegister src) {
        emit3_sse(0xf3, 0x0f, 0x58, dest, src);
    }
    void subss(FloatRegister dest, FloatRegister src) {
        emit3_sse(0xf3, 0x0f, 0x5c, dest, src);
    }
    void mulss(FloatRegister dest, FloatRegister src) {
        emit3_sse(0xf3, 0x0f, 0x59, dest, src);
    }
    void divss(FloatRegister dest, FloatRegister src) {
        emit3_sse(0xf3, 0x0f, 0x5e, dest, src);
    }
    void xorps(FloatRegister dest, FloatRegister src) {
        emit2(0x0f, 0x57, src, dest);
    }

    template <typename LeftType>
    void ucomiss(const LeftType& left, FloatRegister right) {
        emit2(0x0f, 0x2e, right, left);
    }

    template <typename SrcType>
    void cvtsi2ss(FloatRegister dest, const SrcType& src) {
        emit3_sse(0xf3, 0x0f, 0x2a, dest, src);
    }
    void cvtss2si(Register dest, const Operand& src) {
        emit3_sse(0xf3, 0x0f, 0x2d, dest, src);
    }
    template <typename SrcType>
    void cvttss2si(Register dest, const SrcType& src) {
        emit3_sse(0xf3, 0x0f, 0x2c, dest, src);
    }

    void roundss_floor(FloatRegister dest, const Operand& src) {
        emit4_sse(0x66, 0x0f, 0x3a, 0x0a, dest, src);
        *pos_++ = 0x1;
    }
    void roundss_ceil(FloatRegister dest, const Operand& src) {
        emit4_sse(0x66, 0x0f, 0x3a, 0x0a, dest, src);
        *pos_++ = 0x2;
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

    void emitRipRelativeConstant(uintptr_t value) {
        // The data table will be written backwards, before the code start.
        // This allows us to emit rip-relative offsets that don't need
        // fixups. Since it is backwards, however, we have to add 1 to skip
        // from the end of the slot (byte 7) to the beginning (byte 0).
        int32_t offset = pc();
        int32_t data_size = (int32_t)(address_table_.size() + 1) * sizeof(uintptr_t);
        if (offset > INT_MAX - data_size) {
            outOfMemory_ = true;
            return;
        }
        *reinterpret_cast<int32_t*>(pos_ - 4) = -(offset + data_size);
        address_table_.emplace_back(value);
    }
    void emitRipRelativeLabel(PatchCodeLabel* label) {
        emitRipRelativeConstant(0);
        if (label->bound())
            address_table_.back() = label->offset();
        else
            label->use(pc());
        address_table_reloc_.emplace_back((uint32_t)address_table_.size() - 1);
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

    template <typename T>
    void shift_imm_64(const T& t, uint8_t r, int32_t imm) {
        if (imm == 1) {
            emit1_64(0xd1, r, t);
        } else {
            emit1_64(0xc1, r, t);
            *pos_++ = imm & 0x3F;
        }
    }

    template <typename T>
    void shift_imm_32(const T& t, uint8_t r, int32_t imm) {
        if (imm == 1) {
            emit1(0xd1, r, t);
        } else {
            emit1(0xc1, r, t);
            *pos_++ = imm & 0x1F;
        }
    }

    template <typename T>
    void shift_cl_64(const T& t, uint8_t r) {
        emit1_64(0xd3, r, t);
    }

    template <typename T>
    void shift_cl_32(const T& t, uint8_t r) {
        emit1(0xd3, r, t);
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
    template <typename RegType, typename RMType>
    void emit1(uint8_t opcode, const RegType& opreg, const RMType& rm) {
        ensureSpace();
        maybe_emit_rex(opreg, rm);
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

    template <typename RegType, typename RMType>
    void emit2_64(uint8_t prefix, uint8_t opcode, const RegType& opreg, const RMType& rm) {
        ensureSpace();
        emit_rex_64(opreg, rm);
        *pos_++ = prefix;
        emit1_tail(opcode, opreg, rm);
    }

    template <typename RegType, typename RMType>
    void emit2(uint8_t prefix, uint8_t opcode, const RegType& opreg, const RMType& rm) {
        ensureSpace();
        maybe_emit_rex(opreg, rm);
        *pos_++ = prefix;
        emit1_tail(opcode, opreg, rm);
    }

    // Emit two precomputed bytes.
    void emit2(uint8_t prefix, uint8_t opcode) {
        ensureSpace();
        *pos_++ = prefix;
        *pos_++ = opcode;
    }

    template <typename OpRegType, typename RMType>
    void emit3(uint8_t prefix1, uint8_t prefix2, uint8_t opcode, const OpRegType& opreg,
               const RMType& rm)
    {
        ensureSpace();
        maybe_emit_rex(rm);
        *pos_++ = prefix1;
        *pos_++ = prefix2;
        emit1_tail(opcode, opreg, rm);
    }

    template <typename OpRegType, typename RMType>
    void emit3_sse(uint8_t prefix, uint8_t op1, uint8_t op2, const OpRegType& opreg,
               const RMType& rm)
    {
        ensureSpace();
        *pos_++ = prefix;
        maybe_emit_rex(rm);
        *pos_++ = op1;
        emit1_tail(op2, opreg, rm);
    }

    template <typename OpRegType, typename RMType>
    void emit4_sse(uint8_t prefix, uint8_t op1, uint8_t op2, uint8_t op3, const OpRegType& opreg,
                   const RMType& rm)
    {
        ensureSpace();
        *pos_++ = prefix;
        maybe_emit_rex(rm);
        *pos_++ = op1;
        *pos_++ = op2;
        emit1_tail(op3, opreg, rm);
    }

    // Helpers.
    template <typename RegType, typename RMType>
    void emit1_tail(uint8_t opcode, const RegType& opreg, const RMType& rm) {
        *pos_++ = opcode;
        emit_modrm(opreg, rm);
    }

    // W=1, R=0, X=0, B=?
    template <typename RmType>
    void emit_rex_64(const RmType& rm) {
        *pos_++ = (0x48 | rex_bits_for_rm(rm));
    }
    // W=1, R=?, X=0, B=?
    template <typename RegType, typename RmType>
    void emit_rex_64(const RegType& opreg, const RmType& rm) {
        *pos_++ = (0x48 | (rex_bit_for_reg(opreg) << 2) | rex_bits_for_rm(rm));
    }

    // W=0, R=?, X=0, B=?
    template <typename RegType, typename RmType>
    void maybe_emit_rex(const RegType& reg, const RmType& rm) {
        uint8_t bits = (rex_bit_for_reg(reg) << 2) | rex_bits_for_rm(rm);
        if (bits)
            *pos_++ = 0x40 | bits;
    }

    // W=0, R=0, X=0, B=?
    template <typename RmType>
    void maybe_emit_rex(const RmType& rm) {
        if (auto bits = rex_bits_for_rm(rm))
            *pos_++ = static_cast<uint8_t>(0x40 | bits);
    }

    // Helpers for the maybe_emit_rex() template.
    uint8_t rex_bit_for_reg(Register reg) { return reg.rex_bit(); }
    uint8_t rex_bit_for_reg(FloatRegister reg) { return reg.rex_bit(); }
    uint8_t rex_bit_for_reg(uint8_t opreg) { return 0; }
    uint8_t rex_bits_for_rm(const Operand& rm) { return rm.rex_bits(); }
    uint8_t rex_bits_for_rm(Register rm) { return rm.rex_bit(); }
    uint8_t rex_bits_for_rm(FloatRegister rm) { return rm.rex_bit(); }

    // ModR/M encoding helpers.
    void emit_modrm(Register opreg, const Register& rm) {
        emit_modrm(opreg.low_bits(), rm.low_bits());
    }
    void emit_modrm(uint8_t opreg, const Register& rm) {
        emit_modrm(opreg, rm.low_bits());
    }
    void emit_modrm(Register opreg, const Operand& operand) {
        emit_modrm(opreg.low_bits(), operand);
    }
    void emit_modrm(Register opreg, FloatRegister rm) {
        emit_modrm(opreg.low_bits(), rm.low_bits());
    }
    void emit_modrm(FloatRegister opreg, Register rm) {
        emit_modrm(opreg.low_bits(), rm.low_bits());
    }
    void emit_modrm(FloatRegister opreg, FloatRegister rm) {
        emit_modrm(opreg.low_bits(), rm.low_bits());
    }
    void emit_modrm(FloatRegister opreg, const Operand& rm) {
        emit_modrm(opreg.low_bits(), rm);
    }

    // Mod R/M implementation.
    void emit_modrm(uint8_t opreg, uint8_t rm) {
        *pos_++ = (kModeReg << 6) | (opreg << 3) | rm;
    }
    void emit_modrm(uint8_t opreg, const Operand& operand) {
        *pos_++ = operand.getByte(0) | (opreg << 3);
        size_t length = operand.length();
        for (size_t i = 1; i < length; i++)
            *pos_++ = operand.getByte(i);
    }

  private:
    std::vector<uintptr_t> address_table_;
    std::vector<uint32_t> address_table_reloc_;
};

static inline ConditionCode InvertConditionCode(ConditionCode cc) {
    switch (cc) {
        case overflow:
            return no_overflow;
        case no_overflow:
            return overflow;
        case below:
            return not_below;
        case not_below:
            return below;
        case equal:
            return not_equal;
        case not_equal:
            return equal;
        case not_above:
            return above;
        case above:
            return not_above;
        case negative:
            return not_negative;
        case not_negative:
            return negative;
        case even_parity:
            return odd_parity;
        case odd_parity:
            return even_parity;
        case less:
            return not_less;
        case not_less:
            return less;
        case not_greater:
            return greater;
        case greater:
            return not_greater;
        default:
            assert(false);
            return zero;
    }
}

} // namespace sp

#endif // _include_sourcepawn_vm_assembler_x64_h__
