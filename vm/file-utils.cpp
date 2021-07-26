// vim: set sts=2 ts=8 sw=2 tw=99 et:
// 
// Copyright (C) 2004-2015 AlliedModers LLC
//
// This file is part of SourcePawn. SourcePawn is licensed under the GNU
// General Public License, version 3.0 (GPL). If a copy of the GPL was not
// provided with this file, you can obtain it here:
//   http://www.gnu.org/licenses/gpl.html
//
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include <memory>
#include <utility>

#include <smx/smx-headers.h>
#include "file-utils.h"

using namespace sp;

FileType
sp::DetectFileType(FILE* fp)
{
  uint32_t magic = 0;
  if (fread(&magic, sizeof(uint32_t), 1, fp) != 1)
    return FileType::UNKNOWN;

  if (magic == SmxConsts::FILE_MAGIC)
    return FileType::SPFF;

  return FileType::UNKNOWN;
}

FileReader::FileReader(FILE* fp)
 : buffer_(nullptr, DefaultFree),
   length_(0)
{
  if (fseek(fp, 0, SEEK_END) != 0)
    return;
  long size = ftell(fp);
  if (size < 0)
    return;
  if (fseek(fp, 0, SEEK_SET) != 0)
    return;

  buffer_.reset(static_cast<uint8_t*>(malloc(size)));
  if (!buffer_ || fread(buffer_.get(), sizeof(uint8_t), size, fp) != (size_t)size)
    return;

  length_ = size;
}

FileReader::FileReader(const uint8_t* addr, size_t length)
  : buffer_(nullptr, DefaultFree),
    length_(length)
{
  buffer_.reset(static_cast<uint8_t*>(malloc(length)));
  if (!buffer_)
    return;
  memcpy(buffer_.get(), addr, length);
}

FileReader::FileReader(uint8_t* addr, size_t length, void (*dtor)(uint8_t*))
  : buffer_(addr, dtor),
    length_(length)
{
}

void
FileReader::DefaultFree(uint8_t* addr)
{
  free(addr);
}
