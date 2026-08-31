// vim: set ts=4 sts=4 sw=4 tw=99 et:
//
//  Copyright (c) AlliedModders LLC 2021
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
#include "source-file.h"

#include <assert.h>
#include <sys/stat.h>
#include <sys/types.h>

#ifdef _WIN32
# include <io.h>
#else
# include <unistd.h>
#endif

namespace sp {
namespace cc {

SourceFile::SourceFile()
  : pos_(0)
{
}

SourceFile::SourceFile(const std::string& name, tr::string&& data)
  : name_(name),
    data_(std::move(data)),
    is_builtin_(true)
{}

bool
SourceFile::Open(const std::string& file_name)
{
    struct stat s;
    if (stat(file_name.c_str(), &s) != 0)
        return false;
    if ((s.st_mode & S_IFDIR) == S_IFDIR)
        return false;

    std::unique_ptr<FILE, decltype(&::fclose)> fp(fopen(file_name.c_str(), "rb"), &::fclose);
    if (!fp)
        return false;

    if (fseek(fp.get(), 0, SEEK_END) != 0)
        return false;
    auto len = ftell(fp.get());
    if (len == -1L)
        return false;
    if (fseek(fp.get(), 0, SEEK_SET) != 0)
        return false;

    data_.resize(len, '\0');
    data_.shrink_to_fit();
    if (fread(&data_[0], data_.size(), 1, fp.get()) != 1)
        return false;

    name_ = file_name;
    return true;
}

int64_t SourceFile::Pos() {
    return pos_;
}

void SourceFile::Reset(int64_t pos) {
    assert(pos >= 0);
    assert((size_t)pos <= data_.size());
    pos_ = (size_t)pos;
}

int SourceFile::Eof() {
    return pos_ == data_.size();
}

void SourceFile::ComputeLineExtents() {
    if (!line_extents_.empty())
        return;

    tr::vector<uint32_t> extents;
    extents.emplace_back(0);
    for (uint32_t i = 0; i < data_.size(); i++) {
        if (data_[i] != '\r' && data_[i] != '\n')
            continue;
        if (data_[i] == '\r') {
            // Detect \r\n.
            if (i + 1 < data_.size() && data_[i + 1] == '\n')
                i++;
        }
        extents.emplace_back(i + 1);
    }


    line_extents_.resize(extents.size());
    for (size_t i = 0; i < extents.size(); i++)
        line_extents_[i] = extents[i];
}

bool SourceFile::OffsetToLineAndCol(uint32_t offset, uint32_t* line, uint32_t* col) {
    if (offset > data_.size())
        return false;

    ComputeLineExtents();

    if (offset == data_.size()) {
        *line = line_extents_.size();
        if (col)
            *col = 0;
        return true;
    }

    uint32_t lower = 0;
    uint32_t upper = line_extents_.size();
    while (lower < upper) {
        uint32_t index = (lower + upper) / 2;
        uint32_t line_start = line_extents_[index];
        if (offset < line_start) {
            upper = index;
            continue;
        }

        // The range should be (start, end].
        uint32_t line_end = (index < line_extents_.size() - 1)
                            ? line_extents_[index + 1]
                            : data_.size();
        if (offset >= line_end) {
            lower = index + 1;
            continue;
        }

        // Either the offset is the first character of a line, or before the
        // first character of the next line, or it should be the terminal
        // offset.
        assert(offset >= line_start && offset < line_end);
        *line = index + 1;
        if (col)
            *col = offset - line_start + 1;
        return true;
    }

    assert(false);
    return false;
}

bool SourceFile::OffsetOfLine(uint32_t line, uint32_t* offset) {
    ComputeLineExtents();

    uint32_t line_index = line - 1;
    if (line_index > line_extents_.size())
        return false;

    if (line_index == line_extents_.size())
        *offset = data_.size();
    else
        *offset = line_extents_[line_index];
    return true;
}

tr::string SourceFile::GetLine(uint32_t line) {
    ComputeLineExtents();

    uint32_t offset;
    if (!OffsetOfLine(line, &offset))
        return {};

    uint32_t end;
    if (!OffsetOfLine(line + 1, &end))
        end = data_.size();

    return data_.substr(offset, end - offset);
}

} // namespace cc
} // namespace sp
