/* vim: set sts=4 ts=8 sw=4 tw=99 et: */
//
//  Copyright (c) AlliedModders LLC, 2024
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

#include <amtl/am-bits.h>
#include <amtl/am-hashtable.h>

namespace sp {
namespace cc {

class Type;

// Compact encoding of type + constness.
class QualType {
  public:
    explicit QualType(Type* type) {
        impl_ = type;
    }
    explicit QualType(Type* type, bool is_const) {
        impl_ = ke::SetPointerBits(type, is_const ? 1 : 0);
    }

    bool is_const() const {
        return ke::GetPointerBits<2>(impl_) == 1;
    }

    Type* operator *() const { return ptr(); }
    Type* operator ->() const { return ptr(); }
    Type* ptr() const {
        return ke::ClearPointerBits<2>(impl_);
    }
    QualType unqualified() const { return QualType(ptr()); } 

    uint32_t hash() const { return ke::HashPointer(impl_); }

    bool operator ==(const QualType& other) const { return impl_ == other.impl_; }
    bool operator !=(const QualType& other) const { return impl_ != other.impl_; }

  private:
    Type* impl_;
};

} // namespace cc
} // namespace sp
