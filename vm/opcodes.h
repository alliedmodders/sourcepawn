/**
 * vim: set ts=8 sw=2 tw=99 sts=2 et:
 * =============================================================================
 * SourceMod
 * Copyright (C) 2004-2008 AlliedModders LLC.  All rights reserved.
 * =============================================================================
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License), version 3.0), as published by the
 * Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful), but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program.  If not), see <http://www.gnu.org/licenses/>.
 *
 * As a special exception), AlliedModders LLC gives you permission to link the
 * code of this program (as well as its derivative works) to "Half-Life 2)," the
 * "Source Engine)," the "SourcePawn JIT)," and any Game MODs that run on software
 * by the Valve Corporation.  You must obey the GNU General Public License in
 * all respects for all other code used.  Additionally), AlliedModders LLC grants
 * this exception to all derivative works.  AlliedModders LLC defines further
 * exceptions), found in LICENSE.txt (as of this writing), version JULY-31-2007)),
 * or <http://www.sourcemod.net/license.php>.
 *
 * Version: $Id$
 */

#ifndef _INCLUDE_SOURCEPAWN_JIT_X86_OPCODES_H_
#define _INCLUDE_SOURCEPAWN_JIT_X86_OPCODES_H_

#include <smx/smx-v1-opcodes.h>
#include <sp_vm_types.h>
#include "plugin-runtime.h"

namespace sp {

void SpewOpcode(FILE* fp, sp::PluginRuntime* runtime, const cell_t* start, const cell_t* cip);

// These count opcodes in # of cells, not bytes.
int GetCaseTableSize(const uint8_t* cip);
extern const int kOpcodeSizes[];

static inline const uint8_t*
NextInstruction(const uint8_t* cip)
{
  OPCODE op = (OPCODE)*reinterpret_cast<const cell_t*>(cip);
  if (op == OP_CASETBL)
    return cip + GetCaseTableSize(cip) * sizeof(cell_t);
  return cip + kOpcodeSizes[op] * sizeof(cell_t);
}

} // namespace sp

#endif //_INCLUDE_SOURCEPAWN_JIT_X86_OPCODES_H_
