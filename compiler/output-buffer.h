// vim: set ts=8 sts=4 sw=4 tw=99 et:
/*
 *  Copyright (c) AlliedModders 2021
 *
 *  This software is provided "as-is", without any express or implied warranty.
 *  In no event will the authors be held liable for any damages arising from
 *  the use of this software.
 *
 *  Permission is granted to anyone to use this software for any purpose,
 *  including commercial applications, and to alter it and redistribute it
 *  freely, subject to the following restrictions:
 *
 *  1.  The origin of this software must not be misrepresented; you must not
 *      claim that you wrote the original software. If you use this software in
 *      a product, an acknowledgment in the product documentation would be
 *      appreciated but is not required.
 *  2.  Altered source versions must be plainly marked as such, and must not be
 *      misrepresented as being the original software.
 *  3.  This notice may not be removed or altered from any source distribution.
 */

#pragma once

#include <string>

#include "scvars.h"

class AsmBuffer
{
  public:
    AsmBuffer& operator <<(const char* str) {
        size_t added = strlen(str);
        char* at = Append(added);
        memcpy(at, str, added);
        return *this;
    }
    AsmBuffer& operator <<(const std::string& str) {
        char* at = Append(str.size());
        memcpy(at, str.c_str(), str.size());
        return *this;
    }
    AsmBuffer& operator <<(int n) {
        return *this << std::to_string(n);
    }

    std::string str() const {
        return std::string(&bytes_[0], bytes_.size());
    }

    size_t pos() const { return bytes_.size(); }
    void rewind(size_t pos) {
        assert(pos <= bytes_.size());
        bytes_.resize(pos);
    }

    void dump();

  private:
    char* Append(size_t to_add) {
        size_t len = bytes_.size();
        bytes_.resize(bytes_.size() + to_add);
        return &bytes_[len];
    }

  private:
    std::vector<char> bytes_;
};

extern AsmBuffer gAsmBuffer;

class AutoStage
{
  public:
    AutoStage()
      : pos_(gAsmBuffer.pos()),
        code_idx_(code_idx)
    {}

    void Rewind() {
        gAsmBuffer.rewind(pos_);
        code_idx = code_idx_;
    }

  private:
    size_t pos_;
    cell code_idx_;
};
