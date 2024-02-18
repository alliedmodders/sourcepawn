// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
//  Copyright (c) ITB CompuPhase, 1997-2006
//
//  This software is provided "as-is", without any express or implied warranty.
//  In no event will the authors be held liable for any damages arising from
//  the use of this software.
//
//  Permission is granted to anyone to use this software for any purpose,
//  including commercial applications, and to alter it and redistribute it
//  freely, subject to the following restrictions:
//
//  1.  The origin of this software must not be misrepresented; you must not
//      claim that you wrote the original software. If you use this software in
//      a product, an acknowledgment in the product documentation would be
//      appreciated but is not required.
//  2.  Altered source versions must be plainly marked as such, and must not be
//      misrepresented as being the original software.
//  3.  This notice may not be removed or altered from any source distribution.
#pragma once

#include "sc.h"
#include "symbols.h"

namespace sp {
namespace cc {

class DataQueue final
{
  public:
    DataQueue();

    void Add(cell value);
    void Add(tr::vector<cell>&& cells);
    void Add(const char* text, size_t length);
    void AddZeroes(cell count);

    cell size() const { return (cell)buffer_.size() * sizeof(cell); }
    cell dat_address() const { return (cell)buffer_.size() * sizeof(cell); }
    const uint8_t* dat() const { return reinterpret_cast<const uint8_t*>(buffer_.data()); }

  private:
    tr::vector<cell> buffer_;
};

} // namespace cc
} // namespace sp
