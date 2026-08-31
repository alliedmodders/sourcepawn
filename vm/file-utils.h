// vim: set sts=2 ts=8 sw=2 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2004-2026 AlliedModders LLC
//
#ifndef _include_sourcepawn_file_parser_h_
#define _include_sourcepawn_file_parser_h_

#include <stdio.h>

#include <memory>

namespace sp {

enum class FileType { UNKNOWN, AMX, AMXMODX, SPFF };

FileType DetectFileType(FILE* fp);

class FileReader
{
  public:
    FileReader(FILE* fp);
    FileReader(const uint8_t* addr, size_t length);
    FileReader(uint8_t* addr, size_t length, void (*dtor)(uint8_t*));

    const uint8_t* buffer() const {
        return buffer_.get();
    }
    size_t length() const {
        return length_;
    }

  protected:
    static void DefaultFree(uint8_t* addr);

    std::unique_ptr<uint8_t, decltype(&DefaultFree)> buffer_;
    size_t length_;
};

} // namespace sp

#endif // _include_sourcepawn_file_parser_h_
