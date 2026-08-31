// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// Copyright (C) 2026 AlliedModers LLC
//
// This file is part of SourcePawn. SourcePawn is licensed under the GNU
// General Public License, version 3.0 (GPL). If a copy of the GPL was not
// provided with this file, you can obtain it here:
//   http://www.gnu.org/licenses/gpl.html
//
#pragma once

#include <assert.h>
#include <stddef.h>
#include <stdint.h>

#include <sp_vm_types.h>

namespace sp {

class BinaryReader final {
  public:
    explicit BinaryReader(const uint8_t* cursor, const uint8_t* stop = nullptr)
      : cursor_(cursor),
        stop_(stop)
    {}

    template <typename T> T read() {
        assert(!stop_ || cursor_ + sizeof(T) <= stop_);
        T value = *reinterpret_cast<const T*>(cursor_);
        cursor_ += sizeof(T);
        return value;
    }

    cell_t readCell() {
        return read<cell_t>();
    }
    int16_t readInt16() {
        return read<int16_t>();
    }

    const uint8_t* cursor() const { return cursor_; }

    bool more() const {
        return !stop_ || cursor_ < stop_;
    }

    void set_cursor(const uint8_t* cursor) {
        assert(!stop_ || cursor <= stop_);
        cursor_ = cursor;
    }

    const uint8_t* getBytes(size_t n) {
        assert(!stop_ || cursor_ + n <= stop_);
        const uint8_t* result = cursor_;
        cursor_ += n;
        return result;
    }

  private:
    const uint8_t* cursor_;
    const uint8_t* stop_;
};

} // namespace sp
