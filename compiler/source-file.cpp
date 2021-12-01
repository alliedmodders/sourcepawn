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

SourceFile::SourceFile()
  : fp_(nullptr, ::fclose),
    pos_(0)
{
}

bool
SourceFile::Open(const std::string& file_name)
{
    struct stat s;
    if (stat(file_name.c_str(), &s) != 0)
        return false;
    if ((s.st_mode & S_IFDIR) == S_IFDIR)
        return false;

    fp_.reset(fopen(file_name.c_str(), "rb"));
    if (!fp_)
        return false;

    if (fseek(fp_.get(), 0, SEEK_END) != 0)
        return false;
    auto len = ftell(fp_.get());
    if (len == -1L)
        return false;
    if (fseek(fp_.get(), 0, SEEK_SET) != 0)
        return false;

    data_.resize(len, '\0');
    if (fread(&data_[0], data_.size(), 1, fp_.get()) != 1)
        return false;

    name_ = file_name;
    return true;
}

bool
SourceFile::Read(unsigned char* target, int maxchars)
{
    if (pos_ == data_.size())
        return false;

    char* outptr = (char*)target;
    char* outend = outptr + maxchars;
    while (outptr < outend && pos_ < data_.size()) {
        char c = data_[pos_++];
        *outptr++ = c;

        if (c == '\n')
            break;
        if (c == '\r') {
            // Handle CRLF.
            if (pos_ < data_.size() && data_[pos_] == '\n') {
                pos_++;
                if (outptr < outend)
                    *outptr++ = '\n';
            } else {
                // Replace with \n.
                *(outptr - 1) = '\n';
            }
            break;
        }
    }

    // Caller passes in a buffer of size >= maxchars+1.
    *outptr = '\0';
    return true;
}

int64_t
SourceFile::Pos()
{
    return pos_;
}

void
SourceFile::Reset(int64_t pos)
{
    assert(pos >= 0);
    assert((size_t)pos <= data_.size());
    pos_ = (size_t)pos;
}

int
SourceFile::Eof()
{
    return pos_ == data_.size();
}
