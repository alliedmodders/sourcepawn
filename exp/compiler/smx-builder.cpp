// vim: set sts=2 ts=8 sw=2 tw=99 et:
//
// Copyright (C) 2012-2014 David Anderson
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
#include "smx-builder.h"

using namespace ke;
using namespace sp;

class FakeCodeSection : public SmxSection
{
 public:
  FakeCodeSection()
   : SmxSection(".code")
  { }

  bool write(FILE *fp) {
    sp_file_code_t cod;
    memset(&cod, 0, sizeof(cod));

    cod.codesize = sizeof(cod);
    cod.cellsize = 4;
    cod.codeversion = SmxConsts::CODE_VERSION_REJECT;

    return fwrite(&cod, sizeof(cod), 1, fp) == 1;
  }

  size_t length() const {
    return sizeof(sp_file_code_t);
  }
};

SmxBuilder::SmxBuilder()
{
  names_ = new SmxNameTable(".names");
  add(names_);
  add(new FakeCodeSection());
}

bool
SmxBuilder::write(FILE *fp)
{
  sp_file_hdr_t header;
  header.magic = SmxConsts::FILE_MAGIC;
  header.version = SmxConsts::SP2_VERSION_MIN;
  header.compression = SmxConsts::FILE_COMPRESSION_NONE;

  header.disksize = sizeof(header) +
                    sizeof(sp_file_section_t) * sections_.length();

  size_t current_string_offset = 0;
  for (size_t i = 0; i < sections_.length(); i++) {
    RefPtr<SmxSection> section = sections_[i];
    header.disksize += section->length();
    current_string_offset += section->name().length() + 1;
  }
  header.disksize += current_string_offset;

  header.imagesize = header.disksize;
  header.sections = sections_.length();
  header.dataoffs = 0;

  // We put the string table after the sections table.
  header.stringtab = sizeof(header) + sizeof(sp_file_section_t) * sections_.length();

  if (fwrite(&header, sizeof(header), 1, fp) != 1)
    return false;

  size_t current_offset = sizeof(header);
  size_t current_data_offset = header.stringtab + current_string_offset;
  current_string_offset = 0;
  for (size_t i = 0; i < sections_.length(); i++) {
    sp_file_section_t s;
    s.nameoffs = current_string_offset;
    s.dataoffs = current_data_offset;
    s.size = sections_[i]->length();
    if (fwrite(&s, sizeof(s), 1, fp) != 1)
      return false;

    current_offset += sizeof(s);
    current_data_offset += s.size;
    current_string_offset += sections_[i]->name().length() + 1;
  }
  assert(size_t(ftell(fp)) == current_offset);
  assert(current_offset == header.stringtab);
  
  for (size_t i = 0; i < sections_.length(); i++) {
    const AString &name = sections_[i]->name();
    if (fwrite(name.chars(), name.length() + 1, 1, fp) != 1)
      return false;
  }
  current_offset += current_string_offset;

  assert(size_t(ftell(fp)) == current_offset);

  for (size_t i = 0; i < sections_.length(); i++) {
    if (!sections_[i]->write(fp))
      return false;
    current_offset += sections_[i]->length();
  }

  assert(size_t(ftell(fp)) == current_offset);
  assert(current_offset == header.disksize);

  return true;
}

bool
SmxNameTable::write(FILE *fp)
{
  for (size_t i = 0; i < names_.length(); i++) {
    Atom *str = names_[i];
    if (fwrite(str->chars(), str->length() + 1, 1, fp) != 1)
      return false;
  }
  return true;
}

