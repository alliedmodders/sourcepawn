// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
//  Copyright (c) ITB CompuPhase, 1997-2006
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

#include <vector>

#include "code-generator.h"
#include "compile-context.h"
#include "libsmx/data-pool.h"
#include "libsmx/smx-builder.h"
#include "libsmx/smx-encoding.h"
#include "sc.h"
#include "shared/byte-buffer.h"
#include "shared/string-pool.h"

void assemble(CompileContext& cc, CodegenContext& cg, const char* outname,
              int compression_level);

struct BackpatchEntry {
    size_t index;
    cell target;
};

class AsmReader;

class Assembler
{
  public:
    explicit Assembler(CompileContext& cc, CodegenContext& cg);

    void Assemble(sp::SmxByteBuffer* buffer);

    std::vector<BackpatchEntry>& backpatch_list() { return backpatch_list_; }
    std::vector<cell>& label_table() { return label_table_; }

  private:
    void InitOpcodeLookup();
    void GenerateSegment(AsmReader& reader);

    int FindOpcode(const char* instr, size_t maxlen);

  private:
    CompileContext& cc_;
    CodegenContext& cg_;
    std::vector<cell> code_buffer_;
    std::vector<cell> data_buffer_;
    std::vector<BackpatchEntry> backpatch_list_;
    std::vector<cell> label_table_;
    ke::HashMap<sp::CharsAndLength, int, KeywordTablePolicy> opcode_lookup_;
};
