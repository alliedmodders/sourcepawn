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

#include "shared/byte-buffer.h"
#include <smx/smx-v1-opcodes.h>
#include <sp_vm_types.h>

#include "label.h"
#include "sctracker.h"
#include "symbols.h"

namespace sp {
namespace cc {

enum regid {
    sPRI, /* indicates the primary register */
    sALT, /* indicates the secundary register */
};

class SmxAssemblyBuffer : public ByteBuffer
{
 public:
  SmxAssemblyBuffer()
  {}

  void emit(OPCODE op) {
    write<cell_t>(static_cast<cell_t>(op));
  }
  void emit(OPCODE op, cell_t param) {
    write<cell_t>(static_cast<cell_t>(op));
    write<cell_t>(param);
  }
  void emit(OPCODE op, cell_t param1, cell_t param2) {
    write<cell_t>(static_cast<cell_t>(op));
    write<cell_t>(param1);
    write<cell_t>(param2);
  }
  void emit(OPCODE op, cell_t param1, cell_t param2, cell_t param3) {
    write<cell_t>(static_cast<cell_t>(op));
    write<cell_t>(param1);
    write<cell_t>(param2);
    write<cell_t>(param3);
  }
  void emit(OPCODE op, cell_t param1, cell_t param2, cell_t param3, cell_t param4) {
    write<cell_t>(static_cast<cell_t>(op));
    write<cell_t>(param1);
    write<cell_t>(param2);
    write<cell_t>(param3);
    write<cell_t>(param4);
  }
  void emit(OPCODE op, cell_t param1, cell_t param2, cell_t param3, cell_t param4, cell_t param5) {
    write<cell_t>(static_cast<cell_t>(op));
    write<cell_t>(param1);
    write<cell_t>(param2);
    write<cell_t>(param3);
    write<cell_t>(param4);
    write<cell_t>(param5);
  }
  void emit(OPCODE op, Label* address) {
    write<cell_t>(static_cast<cell_t>(op));
    encodeAbsoluteAddress(address);
  }
  void emit(OPCODE op, DataLabel* value) {
    write<cell_t>(static_cast<cell_t>(op));
    write<cell_t>(static_cast<cell_t>(0xb0b0b0b0));
    value->use(pc());
  }

  void const_pri(cell_t value) {
    if (value == 0)
      emit(OP_ZERO_PRI);
    else
      emit(OP_CONST_PRI, value);
  }
  void const_alt(cell_t value) {
    if (value == 0)
      emit(OP_ZERO_ALT);
    else
      emit(OP_CONST_ALT, value);
  }

  void setheap_pri() {
    emit(OP_HEAP, sizeof(cell));
    emit(OP_STOR_I);
    emit(OP_MOVE_PRI);
  }

  void relop_prefix() {
    emit(OP_PUSH_PRI);
    emit(OP_MOVE_PRI);
  }
  void relop_suffix() {
    emit(OP_SWAP_ALT);
    emit(OP_AND);
    emit(OP_POP_ALT);
  }

  void load_hidden_arg(FunctionDecl* fun, bool save_pri) {
    if (!fun->IsVariadic()) {
      emit(OP_LOAD_S_ALT, fun->return_array()->hidden_address);
      return;
    }

    // Load the number of arguments into PRI. Frame layout:
    //   base + 0*sizeof(cell) == previous "base"
    //   base + 1*sizeof(cell) == function return address
    //   base + 2*sizeof(cell) == number of arguments
    //   base + 3*sizeof(cell) == first argument of the function

    // Compute an address to the first argument, then add the argument count
    // to find the address after the final argument:
    //    push.pri
    //    addr.alt   0xc   ; Compute &first_arg
    //    load.s.pri 0x8   ; Load arg count
    //    idxaddr          ; Compute (&first_arg) + argcount
    //    load.i           ; Load *(&first_arg + argcount)
    //    move.alt
    //    pop.pri
    if (save_pri)
      emit(OP_PUSH_PRI);
    emit(OP_ADDR_ALT, 0xc);
    emit(OP_LOAD_S_PRI, 2 * sizeof(cell));
    emit(OP_IDXADDR);
    emit(OP_LOAD_I);
    emit(OP_MOVE_ALT);
    if (save_pri)
      emit(OP_POP_PRI);
  }

  void address(Decl* sym, regid reg) {
    address(sym->as<VarDeclBase>(), reg);
  }

  void address(VarDeclBase* sym, regid reg) {
    if (IsReferenceType(sym->ident(), sym->type()) && IsLocal(sym->vclass())) {
      if (reg == sPRI)
        emit(OP_LOAD_S_PRI, sym->addr());
      else
        emit(OP_LOAD_S_ALT, sym->addr());
    } else {
      if (sym->type()->isArray())
        assert(sym->vclass() == sGLOBAL || sym->vclass() == sSTATIC);

      if (reg == sPRI) {
        if (sym->vclass() == sLOCAL || sym->vclass() == sARGUMENT)
          emit(OP_ADDR_PRI, sym->addr());
        else
          emit(OP_CONST_PRI, sym->addr());
      } else {
        if (sym->vclass() == sLOCAL || sym->vclass() == sARGUMENT)
          emit(OP_ADDR_ALT, sym->addr());
        else
          emit(OP_CONST_ALT, sym->addr());
      }
    }
  }

  void copyarray(VarDeclBase* sym, cell size) {
    if (sym->type()->isArray()) {
      assert(sym->vclass() == sLOCAL || sym->vclass() == sARGUMENT); // symbol must be stack relative
      emit(OP_LOAD_S_ALT, sym->addr());
    } else if (sym->vclass() == sLOCAL || sym->vclass() == sARGUMENT) {
      emit(OP_ADDR_ALT, sym->addr());
    } else {
      emit(OP_CONST_ALT, sym->addr());
    }
    emit(OP_MOVS, size);
  }

  void casetbl(cell_t ncases, Label* def) {
    write<cell_t>(static_cast<cell_t>(OP_CASETBL));
    write<cell_t>(ncases);
    encodeAbsoluteAddress(def);
  }
  void casetbl_entry(cell_t value, Label* where) {
    write<cell_t>(value);
    encodeAbsoluteAddress(where);
  }

  void sysreq_n(Label* address, uint32_t nparams) {
    write<cell_t>(static_cast<cell_t>(OP_SYSREQ_N));
    encodeAbsoluteAddress(address);
    write<cell_t>(nparams);
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
      assert(offset >= 4 && offset <= pc());

      int32_t* p = reinterpret_cast<int32_t*>(bytes() + offset - 4);
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
    assert(offset >= 4 && offset <= pc());

    int32_t* p = reinterpret_cast<int32_t*>(bytes() + offset - 4);
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
