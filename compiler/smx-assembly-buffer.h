// vim: set sts=2 ts=8 sw=2 tw=99 et:
//
// Copyright (C) 2012-2014 AlliedModders LLC, David Anderson
//
// This file is part of SourcePawn.
//
// SourcePawn is free software: you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation, either version 3 of the License, or (at your option)
// any later version.
//
// SourcePawn is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.
#pragma once

#include "utils/byte-buffer.h"
#include <smx/smx-v2-opcodes.h>
#include <sp_vm_types.h>

#include "label.h"
#include "sctracker.h"
#include "symbols.h"

namespace sp {
namespace cc {

using namespace sp::v2;

struct VarSlot {
    explicit VarSlot(int16_t offset) : offset(offset) {}
    int16_t offset;
};

struct Int64Value {
    explicit Int64Value(int64_t value) : value(value) {}
    int64_t value;
};

class SmxAssemblyBuffer : public ByteBuffer
{
 public:
  SmxAssemblyBuffer()
  {}

  void emit(OPCODE op) {
    write<uint8_t>(static_cast<uint8_t>(op));
  }
  void emit(OPCODE op, cell_t param) {
    write<uint8_t>(static_cast<uint8_t>(op));
    write<cell_t>(param);
  }
  void emit(OPCODE op, Int64Value param) {
    write<uint8_t>(static_cast<uint8_t>(op));
    write<int64_t>(param.value);
  }
  void emit(OPCODE op, VarSlot slot) {
    write<uint8_t>(static_cast<uint8_t>(op));
    write<int16_t>(slot.offset);
  }
  void emit(OPCODE op, cell_t param1, cell_t param2) {
    write<uint8_t>(static_cast<uint8_t>(op));
    write<cell_t>(param1);
    write<cell_t>(param2);
  }
  void emit(OPCODE op, VarSlot slot, cell_t param) {
    write<uint8_t>(static_cast<uint8_t>(op));
    write<int16_t>(slot.offset);
    write<cell_t>(param);
  }
  void emit(OPCODE op, VarSlot slot, cell_t param1, cell_t param2) {
    write<uint8_t>(static_cast<uint8_t>(op));
    write<int16_t>(slot.offset);
    write<cell_t>(param1);
    write<cell_t>(param2);
  }
  void emit(OPCODE op, cell_t param1, cell_t param2, cell_t param3) {
    write<uint8_t>(static_cast<uint8_t>(op));
    write<cell_t>(param1);
    write<cell_t>(param2);
    write<cell_t>(param3);
  }
  void emit(OPCODE op, cell_t param1, cell_t param2, cell_t param3, cell_t param4) {
    write<uint8_t>(static_cast<uint8_t>(op));
    write<cell_t>(param1);
    write<cell_t>(param2);
    write<cell_t>(param3);
    write<cell_t>(param4);
  }
  void emit(OPCODE op, cell_t param1, cell_t param2, cell_t param3, cell_t param4, cell_t param5) {
    write<uint8_t>(static_cast<uint8_t>(op));
    write<cell_t>(param1);
    write<cell_t>(param2);
    write<cell_t>(param3);
    write<cell_t>(param4);
    write<cell_t>(param5);
  }
  void emit(OPCODE op, Label* address) {
    write<uint8_t>(static_cast<uint8_t>(op));
    encodeAbsoluteAddress(address);
  }
  void emit(OPCODE op, DataLabel* value) {
    write<uint8_t>(static_cast<uint8_t>(op));
    write<cell_t>(static_cast<cell_t>(0xb0b0b0b0));
    value->use(pc());
  }

  void emit(OPCODE op, Label* address, uint8_t param) {
    write<uint8_t>(static_cast<uint8_t>(op));
    encodeAbsoluteAddress(address);
    write<uint8_t>(param);
  }

  void PUSH_C(cell_t value) {
    if (value >= -128 && value <= 127) {
      emit(OP_PUSH_C_I8);
      write<int8_t>(static_cast<int8_t>(value));
    } else {
      emit(OP_PUSH_C, value);
    }
  }
  void load_hidden_arg(FunctionDecl* decl) {
    assert(decl->signature()->needs_hidden_arg());
    emit(OP_LOAD_S, VarSlot(-1));
  }
  void newbulkarray(uint8_t count, uint32_t type_id) {
    write<uint8_t>(static_cast<uint8_t>(OP_NEWBULKARRAY));
    write<uint8_t>(count);
    write<uint32_t>(type_id);
  }

  void casetbl(cell_t ncases, Label* def) {
    write<cell_t>(ncases);
    encodeAbsoluteAddress(def);
  }
  void casetbl_entry(cell_t value, Label* where) {
    write<cell_t>(value);
    encodeAbsoluteAddress(where);
  }

  void bind(Label* target) {
    bind_to(target, pc());
  }

  void bind_to(Label* target, cell_t value) {
    assert(value >= 0);

    if (oom()) {
      // If we ran out of memory, the code stream is potentially invalid and
      // we cannot use the embedded linked list.
      target->bind(0);
      return;
    }

    assert(!target->bound());
    uint32_t status = target->status();
    while (Label::More(status)) {
      uint32_t offset = Label::ToOffset(status);
      assert(offset >= sizeof(cell_t) && offset <= pc());

      int32_t* p = reinterpret_cast<int32_t*>(bytes() + offset - sizeof(cell_t));
      status = *p;
      *p = value;
    }
    target->bind(value);
  }
  void bind_to(DataLabel* target, cell_t value) {
    if (oom()) {
      // If we ran out of memory, the code stream is potentially invalid and
      // we cannot use the embedded linked list.
      target->bind();
      return;
    }

    uint32_t offset = DataLabel::ToOffset(target->status());
    assert(offset >= sizeof(cell_t) && offset <= pc());

    int32_t* p = reinterpret_cast<int32_t*>(bytes() + offset - sizeof(cell_t));
    assert(*p == int32_t(0xb0b0b0b0));
    *p = value;

    target->bind();
  }

  uint32_t pc() const {
    return position();
  }

 private:
  void encodeAbsoluteAddress(Label* address) {
    if (address->bound()) {
      write<cell_t>(address->offset());
    } else {
      write<cell_t>(address->addPending(pc() + sizeof(cell_t)));
    }
  }
};

} // namespace cc
} // namespace sp
