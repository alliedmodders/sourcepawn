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

    const uint8_t* cursor() const { return cursor_; }

  private:
    const uint8_t* cursor_;
    const uint8_t* stop_;
};

} // namespace sp
