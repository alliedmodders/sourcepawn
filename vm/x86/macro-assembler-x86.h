/**
 * vim: set ts=8 sts=2 sw=2 tw=99 et:
 * =============================================================================
 * SourcePawn JIT SDK
 * Copyright (C) 2004-2008 AlliedModders LLC.  All rights reserved.
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
#ifndef _include_sourcepawn_macroassembler_x86h__
#define _include_sourcepawn_macroassembler_x86h__

#include <assembler.h>
#include <am-vector.h>
#include <string.h>
#include "assembler-x86.h"
#include "stack-frames.h"
#include "environment.h"

namespace sp {

class MacroAssembler : public Assembler
{
 public:
  void enterFrame(FrameType type, uintptr_t function_id) {
    push(ebp);
    movl(ebp, esp);
    push(uint32_t(type));
    push(function_id);
  }
  void leaveFrame() {
    leave();
  }
  void enterExitFrame(ExitFrameType type, uintptr_t payload) {
    enterFrame(FrameType::Exit, EncodeExitFrameId(type, payload));
    movl(Operand(ExternalAddress(Environment::get()->addressOfExit())), ebp);
  }
  void leaveExitFrame() {
    leaveFrame();
  }

  // Inline exit frames are not entered via a call; instead they simulate a
  // call by pushing a return address.
  void enterInlineExitFrame(ExitFrameType type, uintptr_t payload, CodeLabel* return_address) {
    push(return_address);
    enterExitFrame(type, payload);
  }
};

} // namespace sp

#endif // _include_sourcepawn_macroassembler_x86h__

