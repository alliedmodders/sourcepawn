/**
 * vim: set ts=8 sts=2 sw=2 tw=99 et:
 * =============================================================================
 * SourcePawn JIT SDK
 * Copyright (C) 2004-2013 AlliedModders LLC.  All rights reserved.
 * =============================================================================
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License, version 3.0, as published by the
 * Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * As a special exception, AlliedModders LLC gives you permission to link the
 * code of this program (as well as its derivative works) to "Half-Life 2," the
 * "Source Engine," the "SourcePawn JIT," and any Game MODs that run on software
 * by the Valve Corporation.  You must obey the GNU General Public License in
 * all respects for all other code used.  Additionally, AlliedModders LLC grants
 * this exception to all derivative works.  AlliedModders LLC defines further
 * exceptions, found in LICENSE.txt (as of this writing, version JULY-31-2007),
 * or <http://www.sourcemod.net/license.php>.
 *
 * Version: $Id$
 */
#ifndef _include_sourcepawn_label_h__
#define _include_sourcepawn_label_h__

#include <assert.h>
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
   : status_(0)
  {
  }
  ~Label()
  {
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
    assert(this->offset() == offset);
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
  SilentLabel()
  {}
  ~SilentLabel() {
    status_ = 0;
  }
};

// A CodeLabel is a special form of Label intended for absolute that
// are within the code buffer, and thus aren't known yet, and will be
// automatically fixed up later.
// 
// Unlike normal Labels, these do not store a list of incoming uses.
class CodeLabelBase
{
  // If set on status_, the label is bound.
  static const int32_t kBound = (1 << 0);

 public:
  CodeLabelBase()
   : status_(0)
  {
  }
  ~CodeLabelBase()
  {
    assert(!used() || bound());
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
  void use(uint32_t pc) {
    assert(!used());
    status_ = (pc << 1);
    assert(ToOffset(status_) == pc);
  }
  void bind(uint32_t offset) {
    assert(!bound());
    status_ = (offset << 1) | kBound;
    assert(this->offset() == offset);
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

class PatchLabel : public SilentCodeLabel
{
};

// A 32-bit offset from the start of the code segment.
class OffsetLabel : public CodeLabelBase
{
};

} // namespace sp

#endif // _include_sourcepawn_label_h__
