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
#ifndef _include_sourcepawn_vm_constants_x64_h__
#define _include_sourcepawn_vm_constants_x64_h__

#include "assembler-x64.h"

namespace sp {

// We prioritize rbx for being non-volatile and not needing an REX encoding,
// and r14/r15 for being non-volatile and not conflicting with mod r/m
// encoding.
static const Register pri = rax;
static const Register alt = rdx;
static const Register stk = r14;
static const Register dat = r15;
static const Register frm = rbx;

static const Register saved0 = r12;
static const Register saved1 = r13;

static const Register scratch0 = rcx;
static const Register scratch1 = r11;
static const Register scratch2 = r10;
static const Register scratch3 = r9;
static const Register reserved_scratch = r8;

} // namespace sp

#endif //_include_sourcepawn_vm_constants_x64_h__
