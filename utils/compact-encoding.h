// vim: set sts=2 ts=8 sw=2 tw=99 et:
//
// Copyright (C) 2026 AlliedModders LLC
//
// This file is part of SourcePawn. SourcePawn is free software: you can
// redistribute it and/or modify it under the terms of the GNU General Public
// License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
//
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.
#pragma once

#include <optional>

#include <stdint.h>
#include <stddef.h>
#include <optional>
#include <string>

namespace sp {

// 0b0??????? - 7 bits (1 byte)
// 0b10?????? ???????? - 14 bits (2 bytes)
// 0b110????? ???????? ???????? ???????? - 29 bits (4 bytes)
// 0b111????? - Invalid

static inline bool EncodeCompactUint32(std::string* out, uint32_t value) {
    if (value < (1 << 7)) {
        out->push_back((uint8_t)value);
        return true;
    }
    if (value < (1 << 14)) {
        out->push_back(uint8_t(0x80 | (uint8_t)(value >> 8)));
        out->push_back((uint8_t)(value & 0xff));
        return true;
    }
    if (value < (1 << 29)) {
        out->push_back(uint8_t(0xc0 | (uint8_t)(value >> 24)));
        out->push_back((uint8_t)(value >> 16));
        out->push_back((uint8_t)(value >> 8));
        out->push_back((uint8_t)(value & 0xff));
        return true;
    }
    return false;
}

static inline std::optional<uint32_t> DecodeCompact(const uint8_t*& cursor, const uint8_t* end) {
    if (cursor >= end)
        return {};

    uint32_t b1 = *cursor++;
    if ((b1 & 0x80) == 0)
        return {b1};

    if ((b1 & 0xc0) == 0x80) {
        if (cursor >= end)
            return {};
        uint32_t b2 = *cursor++;
        return {((b1 & 0x3f) << 8) | b2};
    }

    if ((b1 & 0xe0) == 0xc0) {
        if (cursor + 3 > end)
            return {};
        uint32_t b2 = *cursor++;
        uint32_t b3 = *cursor++;
        uint32_t b4 = *cursor++;
        return {((b1 & 0x1f) << 24) | (b2 << 16) | (b3 << 8) | b4};
    }

    return {};
}

} // namespace sp
