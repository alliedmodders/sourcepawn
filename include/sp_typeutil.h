// vim: set ts=4 :
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2004-2026 AlliedModders LLC

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

union Int64CellUnion {
    explicit Int64CellUnion(int64_t i64)
      : i64(i64)
    {}

    Int64CellUnion(cell_t a, cell_t b) {
        cells[0] = a;
        cells[1] = b;
    }

    int64_t i64;
    cell_t cells[2];
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

template <size_t Size>
struct CharArraySize {
    static constexpr size_t cells = (Size + sizeof(cell_t) - 1) / sizeof(cell_t);
    static constexpr size_t bytes = cells * sizeof(cell_t);
};

#endif //_INCLUDE_SOURCEPAWN_VM_TYPEUTIL_H_
