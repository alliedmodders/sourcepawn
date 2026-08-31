// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2004-2026 AlliedModders LLC
//
#ifndef _include_sourcepawn_label_h__
#define _include_sourcepawn_label_h__

#include <assert.h>
#include <limits.h>
#include <stdint.h>

namespace sp {

// A label is a lightweight object to assist in managing relative jumps. It
// exists in three states:
//   * Unbound, Unused: The label has no incoming jumps, and its position has
//     not yet been fixed in the instruction stream.
//   * Unbound, Used: The label has not yet been fixed at a position in the
//     instruction stream, but it has incoming jumps.
//   * Bound: The label has been fixed at a position in the instruction stream.
//
// When a label is unbound and used, the offset stored in the Label is a linked
// list threaded through each individual jump. When the label is bound, each
// jump instruction in this list is immediately patched with the correctly
// computed relative distance to the label.
//
// We keep sizeof(Label) == 4 to make it embeddable within code streams if
// need be (for example, SourcePawn mirrors the source code to maintain jump
// maps).
class Label
{
    // If set on status_, the label is bound.
    static const int32_t kBound = (1 << 0);

  public:
    Label()
     : status_(0) {
    }
    Label(Label&& other)
     : status_(other.status_)
    {
      other.status_ = 0;
    }
    Label(const Label&) = delete;
    ~Label() {
        assert(!used() || bound());
    }

    static inline bool More(uint32_t status) {
        return status != 0;
    }
    static inline uint32_t ToOffset(uint32_t status) {
        return status >> 1;
    }

    bool used() const {
        return bound() || !!(status_ >> 1);
    }
    bool bound() const {
        return !!(status_ & kBound);
    }
    uint32_t offset() const {
        assert(bound());
        return ToOffset(status_);
    }
    uint32_t status() const {
        assert(!bound());
        return status_;
    }
    uint32_t addPending(uint32_t pc) {
        assert(pc <= INT_MAX / 2);
        uint32_t prev = status_;
        status_ = pc << 1;
        return prev;
    }
    void bind(uint32_t offset) {
        assert(!bound());
        status_ = (offset << 1) | kBound;
        assert(static_cast<uint32_t>(this->offset()) == offset);
    }

    Label& operator =(const Label&) = delete;
    Label& operator =(Label&& other) {
        status_ = other.status_;
        other.status_ = 0;
        return *this;
    }

  protected:
    // Note that 0 as an invalid offset is okay, because the offset we save for
    // pending jumps are after the jump opcode itself, and therefore 0 is never
    // valid, since there are no 0-byte jumps.
    uint32_t status_;
};

// Label that suppresses its assert, for non-stack use.
class SilentLabel : public Label
{
  public:
    SilentLabel() {
    }
    ~SilentLabel() {
        status_ = 0;
    }
};

// Some platforms (ARM, x64) cannot encode all possible branch targets
// in a single instruction. This is problematic when generating code that
// either needs absolute addresses to itself, or needs to reference code
// outside of the trivially encodable address range.
//
// CodeLabel, unlike Label, guarantees that absolute branch targets are
// encodable, not just relative branch targets.
class CodeLabelBase
{
    // If set on status_, the label is bound.
    static const int32_t kBound = (1 << 0);

  public:
    CodeLabelBase()
     : status_(0) {
    }
    ~CodeLabelBase() {
        assert(!used() || bound());
    }

    static inline int32_t ToOffset(uint32_t status) {
        return int32_t(status) >> 1;
    }

    bool used() const {
        return bound() || !!(status_ >> 1);
    }
    bool bound() const {
        return !!(status_ & kBound);
    }
    int32_t offset() const {
        assert(bound());
        return ToOffset(status_);
    }
    uint32_t status() const {
        assert(!bound());
        return status_;
    }
    void use(int32_t pc) {
        assert(!used());
        status_ = (pc << 1);
        assert(ToOffset(status_) == pc);
    }
    int32_t addPendingUse(int32_t pc) {
        assert(!bound());
        int32_t prev = used() ? ToOffset(status_) : 0;
        status_ = (pc << 1);
        assert(ToOffset(status_) == pc);
        return prev;
    }
    void bind(uint32_t offset) {
        assert(!bound());
        status_ = (offset << 1) | kBound;
        assert(static_cast<uint32_t>(this->offset()) == offset);
    }

  protected:
    uint32_t status_;
};

// Absolute address, any pointer size. These are fixed up when calling
// emitToExecutableMemory().
class CodeLabel : public CodeLabelBase
{
};

class SilentCodeLabel : public CodeLabel
{
  public:
    ~SilentCodeLabel() {
        status_ = 0;
    }
};

// Same as CodeLabel, except that it is guaranteed patchable to any address
// after emitToExecutableMemory(), not just beforehand.
class PatchCodeLabel : public SilentCodeLabel
{
};

// A 32-bit offset from the start of the code segment.
class OffsetLabel : public CodeLabelBase
{
};

} // namespace sp

#endif // _include_sourcepawn_label_h__
