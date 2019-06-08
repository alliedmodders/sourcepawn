/**
 * vim: set ts=4 :
 * =============================================================================
 * SourcePawn
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

#ifndef _INCLUDE_SOURCEPAWN_VM_TYPEUTIL_H_
#define _INCLUDE_SOURCEPAWN_VM_TYPEUTIL_H_

/**
 * @file sp_typeutil.h
 * @brief Defines type utility functions.
 */

#include "sp_vm_types.h"

namespace sp {
union FloatCellUnion {
    FloatCellUnion(float f32)
     : f32(f32)
    {}
    FloatCellUnion(cell_t cell)
     : cell(cell)
    {}

    float f32;
    cell_t cell;
};
} // namespace sp

/**
 * @brief Reinterpret-casts a float to a cell.
 *
 * @param val		Float value.
 * @return			Cell typed version.
 */
static inline cell_t
sp_ftoc(float val)
{
    return sp::FloatCellUnion(val).cell;
}

/**
 * @brief Reinterpret-casts a cell to a float.
 *
 * @param val		Cell-packed float value.
 * @return			Float typed version.
 */
static inline float
sp_ctof(cell_t val)
{
    return sp::FloatCellUnion(val).f32;
}

#endif //_INCLUDE_SOURCEPAWN_VM_TYPEUTIL_H_
