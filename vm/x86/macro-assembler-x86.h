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

class MacroAssemblerX86 : public AssemblerX86
{
 public:
  void enterExitFrame(FrameType frame_type) {
    const ExitFrame& exit = Environment::get()->exit_frame();
    movl(Operand(ExternalAddress(exit.addressOfFrameType())), uint32_t(frame_type));
    movl(Operand(ExternalAddress(exit.addressOfExitSp())), esp);
  }
};

} // namespace sp

#endif // _include_sourcepawn_macroassembler_x86h__

