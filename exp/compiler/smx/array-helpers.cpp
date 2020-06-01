// vim: set sts=2 ts=8 sw=2 tw=99 et:
// 
// Copyright (C) 2012 David Anderson
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

#include "array-helpers.h"
#include "types.h"
#include <amtl/am-bits.h>
#include <amtl/am-vector.h>
#include <assert.h>
#include <sp_vm_types.h>

namespace sp {

using namespace ke;

// :TODO: warn of array dim overflow in type-resolver.
bool
ComputeArrayInfo(ArrayType* array, ArrayInfo* out)
{
  out->base_type = array;
  std::vector<ArrayType*> work;

  Type* iter = array;
  while (iter->isArray()) {
    ArrayType* array_type = iter->toArray();
    if (!array_type->hasFixedLength())
      break;
    work.push_back(array_type);
    iter = array_type->contained();
  }

  // Compute the final level first.
  ArrayType* level = ke::PopBack(&work);
  uint64_t bytes = level->isCharArray()
                   ? CellLengthOfString(level->fixedLength()) * sizeof(cell_t)
                   : level->fixedLength() * sizeof(cell_t);
  if (bytes > INT_MAX)
    return false;

  out->data_width = int32_t(bytes);

  // Compute each additional level, walking inwards.
  uint64_t iv_size = 0;
  while (!work.empty()) {
    level = ke::PopBack(&work);

    if (!IsUint64MultiplySafe(iv_size, level->fixedLength()))
      return false;
    iv_size *= level->fixedLength();

    uint64_t vector_size = level->fixedLength() * sizeof(cell_t);
    if (!IsUint64AddSafe(iv_size, vector_size))
      return false;
    iv_size += vector_size;

    if (!IsUint64MultiplySafe(bytes, level->fixedLength()))
      return false;
    bytes *= uint64_t(level->fixedLength());
  }
  
  if (!IsUint64AddSafe(iv_size, bytes) || iv_size + bytes > INT_MAX)
    return false;

  out->data_size = bytes;
  out->iv_size = iv_size;
  out->bytes = out->data_size + out->iv_size;
  return true;
}

} // namespace sp
