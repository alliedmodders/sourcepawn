// vim: set sts=4 ts=8 sw=4 tw=99 et:
/*  Pawn compiler - Binary code generation (the "assembler")
 *
 *  Copyright (c) ITB CompuPhase, 1997-2006
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
 *
 *  Version: $Id$
 */
#include <assert.h>
#include <ctype.h>
#include <stddef.h> /* for macro offsetof() */
#include <stdio.h>
#include <stdlib.h> /* for macro max() */
#include <string.h>
#include "amxdbg.h"
#include "errors.h"
#include "sc.h"
#include "sclist.h"
#include "scvars.h"
#include <amtl/am-hashmap.h>
#include <amtl/am-string.h>
#include <smx/smx-v1-opcodes.h>
#include <smx/smx-v1.h>
#include <zlib/zlib.h>
#include "lexer.h"
#include "libpawnc.h"
#include "libsmx/data-pool.h"
#include "libsmx/smx-builder.h"
#include "libsmx/smx-encoding.h"
#include "memfile.h"
#include "sctracker.h"
#include "shared/byte-buffer.h"
#include "sp_symhash.h"
#include "types.h"

using namespace sp;
using namespace ke;

class AsmReader;
class CellWriter;

typedef void (*OPCODE_PROC)(CellWriter* writer, AsmReader* reader, cell opcode);

typedef struct {
    cell opcode;
    const char* name;
    int segment; /* sIN_CSEG=parse in cseg, sIN_DSEG=parse in dseg */
    OPCODE_PROC func;
} OPCODEC;

struct BackpatchEntry {
    size_t index;
    cell target;
};

static ke::Vector<cell> sLabelTable;
static ke::Vector<BackpatchEntry> sBackpatchList;

class CellWriter
{
  public:
    explicit CellWriter(Vector<cell>& buffer)
     : buffer_(buffer),
       current_address_(0)
    {}

    void append(cell value) {
        buffer_.append(value);
        current_address_ += sizeof(value);
    }
    void write_label(int index) {
        assert(index >= 0 && index < sc_labnum);
        if (sLabelTable[index] < 0) {
            BackpatchEntry entry = {current_index(), index};
            sBackpatchList.append(entry);
            append(-1);
        } else {
            append(sLabelTable[index]);
        }
    }

    cell current_address() const {
        return current_address_;
    }
    size_t current_index() const {
        return buffer_.length();
    }

  private:
    Vector<cell>& buffer_;
    cell current_address_;
};

/* apparently, strtol() does not work correctly on very large (unsigned)
 * hexadecimal values */
static ucell
hex2long(const char* s, const char** n)
{
    ucell result = 0L;
    int negate = FALSE;
    int digit;

    /* ignore leading whitespace */
    while (*s == ' ' || *s == '\t')
        s++;

    /* allow a negation sign to create the two's complement of numbers */
    if (*s == '-') {
        negate = TRUE;
        s++;
    }

    assert((*s >= '0' && *s <= '9') || (*s >= 'a' && *s <= 'f') || (*s >= 'a' && *s <= 'f'));
    for (;;) {
        if (*s >= '0' && *s <= '9')
            digit = *s - '0';
        else if (*s >= 'a' && *s <= 'f')
            digit = *s - 'a' + 10;
        else if (*s >= 'A' && *s <= 'F')
            digit = *s - 'A' + 10;
        else
            break; /* probably whitespace */
        result = (result << 4) | digit;
        s++;
    }
    if (n != NULL)
        *n = s;
    if (negate)
        result = (~result) + 1; /* take two's complement of the result */
    return (ucell)result;
}

static ucell
getparam(const char* s, const char** n)
{
    ucell result = 0;
    for (;;) {
        result += hex2long(s, &s);
        if (*s != '+')
            break;
        s++;
    }
    if (n != NULL)
        *n = s;
    return result;
}

static const char*
skipwhitespace(const char* str)
{
    while (isspace(*str))
        str++;
    return str;
}

class AsmReader final
{
  public:
    explicit AsmReader(memfile_t* fp)
     : fp_(fp)
    {
        pc_resetasm(fp_);
        pos_ = fp_->pos();
        end_ = fp_->end();
    }

    // Find the next token that is immediately proceeded by a newline.
    const char* next_line();

    // Process characters until non-whitespace is encountered, up to the next
    // newline. If the position is currently not whitespace then the position
    // will remain unchanged.
    const char* next_token_on_line();

    // Advance the stream to the end of the current token, and return the new
    // position (which should only be used for pointer arithmetic, since it may
    // point beyond the end of the stream).
    const char* end_of_token();

    ucell getparam() {
        return ::getparam(pos_, &pos_);
    }
    ucell hex2long() {
        return ::hex2long(pos_, &pos_);
    }
    const char* pos() const {
        assert(pos_ < end_);
        return pos_;
    }
    Vector<symbol*>& native_list() {
        return native_list_;
    }
    symbol* extract_call_target();

  private:
    template <bool StopAtLine>
    inline const char* advance();

  private:
    memfile_t* fp_;
    const char* pos_;
    const char* end_;
    Vector<symbol*> native_list_;
};

const char*
AsmReader::next_line()
{
    while (true) {
        if (pos_ >= end_)
            return nullptr;
        if (*pos_ == '\n') {
            pos_++;
            break;
        }
        pos_++;
    }
    if (pos_ >= end_)
        return nullptr;
    return pos_;
}

const char*
AsmReader::next_token_on_line()
{
    for (; pos_ < end_; pos_++) {
        // Ignore spaces/tabs, stop on newline.
        if (isspace(*pos_)) {
            if (*pos_ == '\n')
                return nullptr;
            continue;
        }
        // Eat comments until the newline.
        if (*pos_ == ';') {
            while (pos_ < end_ && *pos_ != '\n')
                pos_++;
            return nullptr;
        }
        // Probably a token, stop and return.
        return pos_;
    }
    return nullptr;
}

const char*
AsmReader::end_of_token()
{
    assert(!isspace(*pos_) && *pos_ != ';');
    for (; pos_ < end_; pos_++) {
        if (isspace(*pos_) || *pos_ == ';')
            return pos_;
    }
    return pos_;
}

symbol*
AsmReader::extract_call_target()
{
    char name[METHOD_NAMEMAX];

    const char* params = pos();

    int i;
    for (i = 0; !isspace(*params); i++, params++) {
        assert(*params != '\0');
        assert(i < METHOD_NAMEMAX);
        name[i] = *params;
    }
    name[i] = '\0';
    pos_ += i;

    symbol* sym = findglb(name);
    if (!sym) {
        return nullptr;
    }

    assert(sym->ident == iFUNCTN);
    assert(sym->vclass == sGLOBAL);
    return sym;
}

static void
noop(CellWriter* writer, AsmReader* reader, cell opcode)
{
}

static void
set_currentfile(CellWriter* writer, AsmReader* reader, cell opcode)
{
    fcurrent = (short)reader->getparam();
}

static void
parm0(CellWriter* writer, AsmReader* reader, cell opcode)
{
    writer->append(opcode);
}

static void
parm1(CellWriter* writer, AsmReader* reader, cell opcode)
{
    ucell p = reader->getparam();
    writer->append(opcode);
    writer->append(p);
}

static void
parm2(CellWriter* writer, AsmReader* reader, cell opcode)
{
    ucell p1 = reader->getparam();
    ucell p2 = reader->getparam();
    writer->append(opcode);
    writer->append(p1);
    writer->append(p2);
}

static void
parm3(CellWriter* writer, AsmReader* reader, cell opcode)
{
    ucell p1 = reader->getparam();
    ucell p2 = reader->getparam();
    ucell p3 = reader->getparam();
    writer->append(opcode);
    writer->append(p1);
    writer->append(p2);
    writer->append(p3);
}

static void
parm4(CellWriter* writer, AsmReader* reader, cell opcode)
{
    ucell p1 = reader->getparam();
    ucell p2 = reader->getparam();
    ucell p3 = reader->getparam();
    ucell p4 = reader->getparam();
    writer->append(opcode);
    writer->append(p1);
    writer->append(p2);
    writer->append(p3);
    writer->append(p4);
}

static void
parm5(CellWriter* writer, AsmReader* reader, cell opcode)
{
    ucell p1 = reader->getparam();
    ucell p2 = reader->getparam();
    ucell p3 = reader->getparam();
    ucell p4 = reader->getparam();
    ucell p5 = reader->getparam();
    writer->append(opcode);
    writer->append(p1);
    writer->append(p2);
    writer->append(p3);
    writer->append(p4);
    writer->append(p5);
}

static void
do_dump(CellWriter* writer, AsmReader* reader, cell opcode)
{
    int num = 0;

    while (reader->next_token_on_line()) {
        ucell p = reader->getparam();
        writer->append(p);
        num++;
    }
}

static void
do_dumpfill(CellWriter* writer, AsmReader* reader, cell opcode)
{
    ucell value = reader->getparam();
    ucell times = reader->getparam();
    while (times-- > 0) {
        writer->append(value);
    }
}

static void
do_ldgfen(CellWriter* writer, AsmReader* reader, cell opcode)
{
    symbol* sym = reader->extract_call_target();
    assert(sym->ident == iFUNCTN);
    assert(!sym->native);
    assert((sym->function()->funcid & 1) == 1);
    assert(sym->usage & uREAD);
    assert(!sym->skipped);

    // Note: we emit const.pri for backward compatibility.
    assert(opcode == sp::OP_UNGEN_LDGFN_PRI);
    writer->append(sp::OP_CONST_PRI);
    writer->append(sym->function()->funcid);
}

static void
do_call(CellWriter* writer, AsmReader* reader, cell opcode)
{
    symbol* sym = reader->extract_call_target();
    assert(sym->usage & uREAD);
    assert(!sym->skipped);

    writer->append(opcode);
    writer->append(sym->addr());
}

static void
do_sysreq(CellWriter* writer, AsmReader* reader, cell opcode)
{
    symbol* sym = reader->extract_call_target();
    ucell nargs = reader->getparam();

    assert(sym->native);
    if (sym->addr() < 0) {
      sym->setAddr(reader->native_list().length());
      reader->native_list().append(sym);
    }

    writer->append(opcode);
    writer->append(sym->addr());
    writer->append(nargs);
}

static void
do_jump(CellWriter* writer, AsmReader* reader, cell opcode)
{
    int i = reader->hex2long();

    writer->append(opcode);
    writer->write_label(i);
}

static void
do_switch(CellWriter* writer, AsmReader* reader, cell opcode)
{
    int i = reader->hex2long();

    writer->append(opcode);
    writer->write_label(i);
}

static void
do_case(CellWriter* writer, AsmReader* reader, cell opcode)
{
    cell v = reader->hex2long();
    int i = reader->hex2long();

    writer->append(v);
    writer->write_label(i);
}

// clang-format off
static OPCODEC opcodelist[] = {
  /* node for "invalid instruction" */
  {  0, NULL,         0,        noop },
  {  0, "CODE",       sIN_CSEG, set_currentfile },
  {  0, "DATA",       sIN_DSEG, set_currentfile },
  {  0, "STKSIZE",    0,        noop },
  /* opcodes in sorted order */
  { 78, "add",        sIN_CSEG, parm0 },
  { 87, "add.c",      sIN_CSEG, parm1 },
  { 14, "addr.alt",   sIN_CSEG, parm1 },
  { 13, "addr.pri",   sIN_CSEG, parm1 },
  { 81, "and",        sIN_CSEG, parm0 },
  {121, "bounds",     sIN_CSEG, parm1 },
  {137, "break",      sIN_CSEG, parm0 },  /* version 8 */
  { 49, "call",       sIN_CSEG, do_call },
  {  0, "case",       sIN_CSEG, do_case },
  {130, "casetbl",    sIN_CSEG, parm0 },  /* version 1 */
  {156, "const",      sIN_CSEG, parm2 },  /* version 9 */
  { 12, "const.alt",  sIN_CSEG, parm1 },
  { 11, "const.pri",  sIN_CSEG, parm1 },
  {157, "const.s",    sIN_CSEG, parm2 },  /* version 9 */
  {114, "dec",        sIN_CSEG, parm1 },
  {113, "dec.alt",    sIN_CSEG, parm0 },
  {116, "dec.i",      sIN_CSEG, parm0 },
  {112, "dec.pri",    sIN_CSEG, parm0 },
  {115, "dec.s",      sIN_CSEG, parm1 },
  {  0, "dump",       sIN_DSEG, do_dump },
  {  0, "dumpfill",   sIN_DSEG, do_dumpfill },
  {166, "endproc",    sIN_CSEG, parm0 },
  { 95, "eq",         sIN_CSEG, parm0 },
  {106, "eq.c.alt",   sIN_CSEG, parm1 },
  {105, "eq.c.pri",   sIN_CSEG, parm1 },
  {119, "fill",       sIN_CSEG, parm1 },
  {162, "genarray",   sIN_CSEG, parm1 },
  {163, "genarray.z", sIN_CSEG, parm1 },
  {120, "halt",       sIN_CSEG, parm1 },
  { 45, "heap",       sIN_CSEG, parm1 },
  { 27, "idxaddr",    sIN_CSEG, parm0 },
  { 28, "idxaddr.b",  sIN_CSEG, parm1 },
  {109, "inc",        sIN_CSEG, parm1 },
  {108, "inc.alt",    sIN_CSEG, parm0 },
  {111, "inc.i",      sIN_CSEG, parm0 },
  {107, "inc.pri",    sIN_CSEG, parm0 },
  {110, "inc.s",      sIN_CSEG, parm1 },
  { 86, "invert",     sIN_CSEG, parm0 },
  { 55, "jeq",        sIN_CSEG, do_jump },
  { 56, "jneq",       sIN_CSEG, do_jump },
  { 54, "jnz",        sIN_CSEG, do_jump },
  { 64, "jsgeq",      sIN_CSEG, do_jump },
  { 63, "jsgrtr",     sIN_CSEG, do_jump },
  { 62, "jsleq",      sIN_CSEG, do_jump },
  { 61, "jsless",     sIN_CSEG, do_jump },
  { 51, "jump",       sIN_CSEG, do_jump },
  { 53, "jzer",       sIN_CSEG, do_jump },
  {167, "ldgfn.pri",  sIN_CSEG, do_ldgfen },
  { 25, "lidx",       sIN_CSEG, parm0 },
  { 26, "lidx.b",     sIN_CSEG, parm1 },
  {  2, "load.alt",   sIN_CSEG, parm1 },
  {154, "load.both",  sIN_CSEG, parm2 },  /* version 9 */
  {  9, "load.i",     sIN_CSEG, parm0 },
  {  1, "load.pri",   sIN_CSEG, parm1 },
  {  4, "load.s.alt", sIN_CSEG, parm1 },
  {155, "load.s.both",sIN_CSEG, parm2 },  /* version 9 */
  {  3, "load.s.pri", sIN_CSEG, parm1 },
  { 10, "lodb.i",     sIN_CSEG, parm1 },
  {  8, "lref.s.alt", sIN_CSEG, parm1 },
  {  7, "lref.s.pri", sIN_CSEG, parm1 },
  { 34, "move.alt",   sIN_CSEG, parm0 },
  { 33, "move.pri",   sIN_CSEG, parm0 },
  {117, "movs",       sIN_CSEG, parm1 },
  { 85, "neg",        sIN_CSEG, parm0 },
  { 96, "neq",        sIN_CSEG, parm0 },
  {134, "nop",        sIN_CSEG, parm0 },  /* version 6 */
  { 84, "not",        sIN_CSEG, parm0 },
  { 82, "or",         sIN_CSEG, parm0 },
  { 43, "pop.alt",    sIN_CSEG, parm0 },
  { 42, "pop.pri",    sIN_CSEG, parm0 },
  { 46, "proc",       sIN_CSEG, parm0 },
  { 40, "push",       sIN_CSEG, parm1 },
  {133, "push.adr",   sIN_CSEG, parm1 },  /* version 4 */
  { 37, "push.alt",   sIN_CSEG, parm0 },
  { 39, "push.c",     sIN_CSEG, parm1 },
  { 36, "push.pri",   sIN_CSEG, parm0 },
  { 41, "push.s",     sIN_CSEG, parm1 },
  {139, "push2",      sIN_CSEG, parm2 },  /* version 9 */
  {141, "push2.adr",  sIN_CSEG, parm2 },  /* version 9 */
  {138, "push2.c",    sIN_CSEG, parm2 },  /* version 9 */
  {140, "push2.s",    sIN_CSEG, parm2 },  /* version 9 */
  {143, "push3",      sIN_CSEG, parm3 },  /* version 9 */
  {145, "push3.adr",  sIN_CSEG, parm3 },  /* version 9 */
  {142, "push3.c",    sIN_CSEG, parm3 },  /* version 9 */
  {144, "push3.s",    sIN_CSEG, parm3 },  /* version 9 */
  {147, "push4",      sIN_CSEG, parm4 },  /* version 9 */
  {149, "push4.adr",  sIN_CSEG, parm4 },  /* version 9 */
  {146, "push4.c",    sIN_CSEG, parm4 },  /* version 9 */
  {148, "push4.s",    sIN_CSEG, parm4 },  /* version 9 */
  {151, "push5",      sIN_CSEG, parm5 },  /* version 9 */
  {153, "push5.adr",  sIN_CSEG, parm5 },  /* version 9 */
  {150, "push5.c",    sIN_CSEG, parm5 },  /* version 9 */
  {152, "push5.s",    sIN_CSEG, parm5 },  /* version 9 */
  { 48, "retn",       sIN_CSEG, parm0 },
  { 74, "sdiv.alt",   sIN_CSEG, parm0 },
  {104, "sgeq",       sIN_CSEG, parm0 },
  {103, "sgrtr",      sIN_CSEG, parm0 },
  { 65, "shl",        sIN_CSEG, parm0 },
  { 69, "shl.c.alt",  sIN_CSEG, parm1 },
  { 68, "shl.c.pri",  sIN_CSEG, parm1 },
  { 66, "shr",        sIN_CSEG, parm0 },
  { 71, "shr.c.alt",  sIN_CSEG, parm1 },
  { 70, "shr.c.pri",  sIN_CSEG, parm1 },
  {102, "sleq",       sIN_CSEG, parm0 },
  {101, "sless",      sIN_CSEG, parm0 },
  { 72, "smul",       sIN_CSEG, parm0 },
  { 88, "smul.c",     sIN_CSEG, parm1 },
  { 22, "sref.s.alt", sIN_CSEG, parm1 },
  { 21, "sref.s.pri", sIN_CSEG, parm1 },
  { 67, "sshr",       sIN_CSEG, parm0 },
  { 44, "stack",      sIN_CSEG, parm1 },
  { 16, "stor.alt",   sIN_CSEG, parm1 },
  { 23, "stor.i",     sIN_CSEG, parm0 },
  { 15, "stor.pri",   sIN_CSEG, parm1 },
  { 18, "stor.s.alt", sIN_CSEG, parm1 },
  { 17, "stor.s.pri", sIN_CSEG, parm1 },
  {164, "stradjust.pri", sIN_CSEG, parm0 },
  { 24, "strb.i",     sIN_CSEG, parm1 },
  { 79, "sub",        sIN_CSEG, parm0 },
  { 80, "sub.alt",    sIN_CSEG, parm0 },
  {132, "swap.alt",   sIN_CSEG, parm0 },
  {131, "swap.pri",   sIN_CSEG, parm0 },
  {129, "switch",     sIN_CSEG, do_switch },
  {135, "sysreq.n",   sIN_CSEG, do_sysreq },
  {161, "tracker.pop.setheap", sIN_CSEG, parm0 },
  {160, "tracker.push.c", sIN_CSEG, parm1 },
  { 35, "xchg",       sIN_CSEG, parm0 },
  { 83, "xor",        sIN_CSEG, parm0 },
  { 91, "zero",       sIN_CSEG, parm1 },
  { 90, "zero.alt",   sIN_CSEG, parm0 },
  { 89, "zero.pri",   sIN_CSEG, parm0 },
  { 92, "zero.s",     sIN_CSEG, parm1 },
};
// clang-format on

static ke::HashMap<CharsAndLength, int, KeywordTablePolicy> sOpcodeLookup;

static void
init_opcode_lookup()
{
    if (sOpcodeLookup.elements())
        return;

    sOpcodeLookup.init(512);

    static const int kNumOpcodes = size_t(sizeof(opcodelist) / sizeof(*opcodelist));
    for (int i = 1; i < kNumOpcodes; i++) {
        const auto& entry = opcodelist[i];
        CharsAndLength key(entry.name, strlen(entry.name));
        auto p = sOpcodeLookup.findForAdd(key);
        assert(!p.found());
        sOpcodeLookup.add(p, key, i);
    }
}

static int
findopcode(const char* instr, size_t maxlen)
{
    CharsAndLength key(instr, maxlen);
    auto p = sOpcodeLookup.find(key);
    if (!p.found())
        return 0;
    return p->value;
}

// Generate code or data into a buffer.
static void
generate_segment(AsmReader& reader, Vector<cell>* code_buffer, Vector<cell>* data_buffer)
{
    CellWriter code_writer(*code_buffer);
    CellWriter data_writer(*data_buffer);

    do {
        const char* instr = reader.next_token_on_line();

        // Ignore empty lines.
        if (!instr)
            continue;

        if (tolower(instr[0]) == 'l' && instr[1] == '.') {
            int lindex = (int)hex2long(instr + 2, nullptr);
            assert(lindex >= 0 && lindex < sc_labnum);
            assert(sLabelTable[lindex] == -1);
            sLabelTable[lindex] = code_writer.current_address();
            continue;
        }

        const char* pos = reader.end_of_token();
        int op_index = findopcode(instr, (pos - instr));
        OPCODEC& op = opcodelist[op_index];
        assert(op.name != nullptr);

        reader.next_token_on_line();
        if (op.segment == sIN_CSEG)
            op.func(&code_writer, &reader, op.opcode);
        else if (op.segment == sIN_DSEG)
            op.func(&data_writer, &reader, op.opcode);
    } while (reader.next_line());

    // Fix up backpatches.
    for (const auto& patch : sBackpatchList) {
        assert(patch.index < code_buffer->length());
        assert(patch.target >= 0 && patch.target < sc_labnum);
        assert(sLabelTable[patch.target] >= 0);
        code_buffer->at(patch.index) = sLabelTable[patch.target];
    }
}

#if !defined NDEBUG
// The opcode list should be sorted by name.
class VerifyOpcodeSorting
{
  public:
    VerifyOpcodeSorting() {
        assert(opcodelist[1].name != NULL);
        for (size_t i = 2; i < (sizeof opcodelist / sizeof opcodelist[0]); i++) {
            assert(opcodelist[i].name != NULL);
            assert(strcmp(opcodelist[i].name, opcodelist[i - 1].name) > 0);
        }
    }
} sVerifyOpcodeSorting;
#endif

static int
sort_by_name(const void* a1, const void* a2)
{
    symbol* s1 = *(symbol**)a1;
    symbol* s2 = *(symbol**)a2;
    return strcmp(s1->name(), s2->name());
}

struct function_entry {
    symbol* sym;
    AString name;
};

static int
sort_functions(const void* a1, const void* a2)
{
    function_entry& f1 = *(function_entry*)a1;
    function_entry& f2 = *(function_entry*)a2;
    return strcmp(f1.name.chars(), f2.name.chars());
}

// Helper for parsing a debug string. Debug strings look like this:
//  L:40 10
class DebugString
{
  public:
    DebugString()
     : kind_('\0'),
       str_(nullptr)
    {}
    DebugString(char* str)
     : kind_(str[0]),
       str_(str)
    {
        assert(str_[1] == ':');
        str_ += 2;
    }
    char kind() const {
        return kind_;
    }
    ucell parse() {
        return hex2long(str_, &str_);
    }
    const char* skipspaces() {
        str_ = ::skipwhitespace(str_);
        return str_;
    }
    void expect(char c) {
        assert(*str_ == c);
        str_++;
    }
    const char* skipto(char c) {
        str_ = strchr(str_, c);
        return str_;
    }
    char getc() {
        return *str_++;
    }

  private:
    char kind_;
    const char* str_;
};

typedef SmxBlobSection<sp_fdbg_info_t> SmxDebugInfoSection;
typedef SmxListSection<sp_fdbg_line_t> SmxDebugLineSection;
typedef SmxListSection<sp_fdbg_file_t> SmxDebugFileSection;

struct variable_type_t {
    int tag;
    const int* dims;
    int dimcount;
    bool is_const;
};

class RttiBuilder
{
  public:
    explicit RttiBuilder(SmxNameTable* names);

    void finish(SmxBuilder& builder);
    void add_method(symbol* sym);
    void add_native(symbol* sym);

  private:
    uint32_t add_enum(Type* type);
    uint32_t add_funcenum(Type* type, funcenum_t* fe);
    uint32_t add_typeset(Type* type, funcenum_t* fe);
    uint32_t add_struct(Type* type);
    uint32_t add_enumstruct(Type* type);
    uint32_t encode_signature(symbol* sym);
    void encode_signature_into(Vector<uint8_t>& bytes, functag_t* ft);
    void encode_enum_into(Vector<uint8_t>& bytes, Type* type);
    void encode_tag_into(Vector<uint8_t>& bytes, int tag);
    void encode_ret_array_into(Vector<uint8_t>& bytes, symbol* sym);
    void encode_funcenum_into(Vector<uint8_t>& bytes, Type* type, funcenum_t* fe);
    void encode_var_type(Vector<uint8_t>& bytes, const variable_type_t& info);
    void encode_struct_into(Vector<uint8_t>& bytes, Type* type);
    void encode_enumstruct_into(Vector<uint8_t>& bytes, Type* type);

    uint32_t to_typeid(const Vector<uint8_t>& bytes);

    void add_debug_var(SmxRttiTable<smx_rtti_debug_var>* table, DebugString& str);
    void build_debuginfo();

  private:
    RefPtr<SmxNameTable> names_;
    DataPool type_pool_;
    RefPtr<SmxBlobSection<void>> data_;
    RefPtr<SmxRttiTable<smx_rtti_method>> methods_;
    RefPtr<SmxRttiTable<smx_rtti_native>> natives_;
    RefPtr<SmxRttiTable<smx_rtti_enum>> enums_;
    RefPtr<SmxRttiTable<smx_rtti_typedef>> typedefs_;
    RefPtr<SmxRttiTable<smx_rtti_typeset>> typesets_;
    RefPtr<SmxRttiTable<smx_rtti_classdef>> classdefs_;
    RefPtr<SmxRttiTable<smx_rtti_field>> fields_;
    RefPtr<SmxRttiTable<smx_rtti_enumstruct>> enumstructs_;
    RefPtr<SmxRttiTable<smx_rtti_es_field>> es_fields_;
    RefPtr<SmxDebugInfoSection> dbg_info_;
    RefPtr<SmxDebugLineSection> dbg_lines_;
    RefPtr<SmxDebugFileSection> dbg_files_;
    RefPtr<SmxRttiTable<smx_rtti_debug_method>> dbg_methods_;
    RefPtr<SmxRttiTable<smx_rtti_debug_var>> dbg_globals_;
    RefPtr<SmxRttiTable<smx_rtti_debug_var>> dbg_locals_;

    typedef ke::HashMap<Type*, uint32_t, ke::PointerPolicy<Type>> TypeIdCache;
    TypeIdCache typeid_cache_;
};

RttiBuilder::RttiBuilder(SmxNameTable* names)
 : names_(names)
{
    typeid_cache_.init(128);
    data_ = new SmxBlobSection<void>("rtti.data");
    methods_ = new SmxRttiTable<smx_rtti_method>("rtti.methods");
    natives_ = new SmxRttiTable<smx_rtti_native>("rtti.natives");
    enums_ = new SmxRttiTable<smx_rtti_enum>("rtti.enums");
    typedefs_ = new SmxRttiTable<smx_rtti_typedef>("rtti.typedefs");
    typesets_ = new SmxRttiTable<smx_rtti_typeset>("rtti.typesets");
    classdefs_ = new SmxRttiTable<smx_rtti_classdef>("rtti.classdefs");
    fields_ = new SmxRttiTable<smx_rtti_field>("rtti.fields");
    enumstructs_ = new SmxRttiTable<smx_rtti_enumstruct>("rtti.enumstructs");
    es_fields_ = new SmxRttiTable<smx_rtti_es_field>("rtti.enumstruct_fields");
    dbg_info_ = new SmxDebugInfoSection(".dbg.info");
    dbg_lines_ = new SmxDebugLineSection(".dbg.lines");
    dbg_files_ = new SmxDebugFileSection(".dbg.files");
    dbg_methods_ = new SmxRttiTable<smx_rtti_debug_method>(".dbg.methods");
    dbg_globals_ = new SmxRttiTable<smx_rtti_debug_var>(".dbg.globals");
    dbg_locals_ = new SmxRttiTable<smx_rtti_debug_var>(".dbg.locals");
}

void
RttiBuilder::finish(SmxBuilder& builder)
{
    build_debuginfo();

    const ByteBuffer& buffer = type_pool_.buffer();
    data_->add(buffer.bytes(), buffer.size());

    builder.add(data_);
    builder.add(methods_);
    builder.add(natives_);
    builder.addIfNotEmpty(enums_);
    builder.addIfNotEmpty(typedefs_);
    builder.addIfNotEmpty(typesets_);
    builder.addIfNotEmpty(classdefs_);
    builder.addIfNotEmpty(fields_);
    builder.addIfNotEmpty(enumstructs_);
    builder.addIfNotEmpty(es_fields_);
    builder.add(dbg_files_);
    builder.add(dbg_lines_);
    builder.add(dbg_info_);
    builder.add(dbg_methods_);
    builder.add(dbg_globals_);
    builder.add(dbg_locals_);
}

void
RttiBuilder::build_debuginfo()
{
    stringlist* dbgstrs = get_dbgstrings();

    // State for tracking which file we're on. We replicate the original AMXDBG
    // behavior here which excludes duplicate addresses.
    ucell prev_file_addr = 0;
    const char* prev_file_name = nullptr;

    // Add debug data.
    for (stringlist* iter = dbgstrs; iter; iter = iter->next) {
        if (iter->line[0] == '\0')
            continue;

        DebugString str(iter->line);
        switch (str.kind()) {
            case 'F': {
                ucell codeidx = str.parse();
                if (codeidx != prev_file_addr) {
                    if (prev_file_name) {
                        sp_fdbg_file_t& entry = dbg_files_->add();
                        entry.addr = prev_file_addr;
                        entry.name = names_->add(gAtoms, prev_file_name);
                    }
                    prev_file_addr = codeidx;
                }
                prev_file_name = str.skipspaces();
                break;
            }

            case 'L': {
                sp_fdbg_line_t& entry = dbg_lines_->add();
                entry.addr = str.parse();
                entry.line = str.parse();
                break;
            }

            case 'S':
                add_debug_var(dbg_globals_, str);
                break;
        }
    }

    // Add the last file.
    if (prev_file_name) {
        sp_fdbg_file_t& entry = dbg_files_->add();
        entry.addr = prev_file_addr;
        entry.name = names_->add(gAtoms, prev_file_name);
    }

    // Finish up debug header statistics.
    dbg_info_->header().num_files = dbg_files_->count();
    dbg_info_->header().num_lines = dbg_lines_->count();
    dbg_info_->header().num_syms = 0;
    dbg_info_->header().num_arrays = 0;
}

void
RttiBuilder::add_debug_var(SmxRttiTable<smx_rtti_debug_var>* table, DebugString& str)
{
    int address = str.parse();
    int tag = str.parse();
    str.skipspaces();
    str.expect(':');
    const char* name_start = str.skipspaces();
    const char* name_end = str.skipto(' ');
    uint32_t code_start = str.parse();
    uint32_t code_end = str.parse();
    int ident = str.parse();
    int vclass = str.parse();
    bool is_const = !!str.parse();

    // We don't care about the ident type, we derive it from the tag.
    (void)ident;

    str.skipspaces();

    int dims[sDIMEN_MAX];
    int dimcount = 0;
    int last_tag = 0;
    if (str.getc() == '[') {
        for (const char* ptr = str.skipspaces(); *ptr != ']'; ptr = str.skipspaces()) {
            last_tag = str.parse();
            str.skipspaces();
            str.expect(':');
            dims[dimcount++] = str.parse();
        }
    }

    // Rewrite enum structs to look less like arrays.
    if (gTypes.find(last_tag)->asEnumStruct()) {
        assert(dimcount > 0);
        dimcount--;
        tag = last_tag;
    }

    // Encode the type.
    uint32_t type_id;
    {
        variable_type_t type = {tag, dims, dimcount, is_const};
        Vector<uint8_t> encoding;
        encode_var_type(encoding, type);

        type_id = to_typeid(encoding);
    }

    smx_rtti_debug_var& var = table->add();
    var.address = address;
    switch (vclass) {
        case sLOCAL:
            var.vclass = address < 0 ? kVarClass_Local : kVarClass_Arg;
            break;
        case sGLOBAL:
            var.vclass = kVarClass_Global;
            break;
        case sSTATIC:
            var.vclass = kVarClass_Static;
            break;
        default:
            var.vclass = 0;
            assert(false);
    }
    var.name = names_->add(gAtoms.add(name_start, name_end - name_start));
    var.code_start = code_start;
    var.code_end = code_end;
    var.type_id = type_id;
}

void
RttiBuilder::add_method(symbol* sym)
{
    assert(!sym->skipped);

    uint32_t index = methods_->count();
    smx_rtti_method& method = methods_->add();
    method.name = names_->add(sym->nameAtom());
    method.pcode_start = sym->addr();
    method.pcode_end = sym->codeaddr;
    method.signature = encode_signature(sym);

    if (!sym->function()->dbgstrs)
        return;

    smx_rtti_debug_method debug;
    debug.method_index = index;
    debug.first_local = dbg_locals_->count();

    for (stringlist* iter = sym->function()->dbgstrs; iter; iter = iter->next) {
        if (iter->line[0] == '\0')
            continue;

        DebugString str(iter->line);
        if (str.kind() == 'S')
            add_debug_var(dbg_locals_, str);
    }

    // Only add a method table entry if we actually had locals.
    if (debug.first_local != dbg_locals_->count())
        dbg_methods_->add(debug);
}

void
RttiBuilder::add_native(symbol* sym)
{
    smx_rtti_native& native = natives_->add();
    native.name = names_->add(sym->nameAtom());
    native.signature = encode_signature(sym);
}

uint32_t
RttiBuilder::add_enumstruct(Type* type)
{
    TypeIdCache::Insert p = typeid_cache_.findForAdd(type);
    if (p.found())
        return p->value;

    symbol* sym = type->asEnumStruct();
    uint32_t es_index = enumstructs_->count();
    typeid_cache_.add(p, type, es_index);

    smx_rtti_enumstruct es = {};
    es.name = names_->add(gAtoms, type->name());
    es.first_field = es_fields_->count();
    es.size = sym->addr();
    enumstructs_->add(es);

    // Pre-allocate storage in case of nested types.
    constvalue* table = sym->dim.enumlist;
    for (auto iter = table->next; iter; iter = iter->next)
        es_fields_->add() = smx_rtti_es_field{};

    // Add all fields.
    size_t index = 0;
    for (auto iter = table->next; iter; iter = iter->next, index++) {
        symbol* field = find_enumstruct_field(type, iter->name);
        if (!field) {
            error(105, type->name(), iter->name);
            continue;
        }

        int dims[1], dimcount = 0;
        if (field->dim.array.length)
            dims[dimcount++] = field->dim.array.length;

        variable_type_t type = {field->x.tags.index, dims, dimcount, false};
        Vector<uint8_t> encoding;
        encode_var_type(encoding, type);

        smx_rtti_es_field info;
        info.name = names_->add(gAtoms, iter->name);
        info.type_id = to_typeid(encoding);
        info.offset = field->addr();
        es_fields_->at(es.first_field + index) = info;
    }

    return es_index;
}

uint32_t
RttiBuilder::add_struct(Type* type)
{
    TypeIdCache::Insert p = typeid_cache_.findForAdd(type);
    if (p.found())
        return p->value;

    uint32_t struct_index = classdefs_->count();
    typeid_cache_.add(p, type, struct_index);

    pstruct_t* ps = type->asStruct();

    smx_rtti_classdef classdef;
    memset(&classdef, 0, sizeof(classdef));
    classdef.flags = kClassDefType_Struct;
    classdef.name = names_->add(gAtoms, ps->name);
    classdef.first_field = fields_->count();
    classdefs_->add(classdef);

    // Pre-reserve space in case we recursively add structs.
    for (size_t i = 0; i < ps->args.length(); i++)
        fields_->add();

    for (size_t i = 0; i < ps->args.length(); i++) {
        const structarg_t* arg = ps->args[i].get();

        int dims[1] = {0};
        int dimcount = arg->ident == iREFARRAY ? 1 : 0;

        variable_type_t type = {arg->tag, dims, dimcount, !!arg->fconst};
        Vector<uint8_t> encoding;
        encode_var_type(encoding, type);

        smx_rtti_field field;
        field.flags = 0;
        field.name = names_->add(arg->name);
        field.type_id = to_typeid(encoding);
        fields_->at(classdef.first_field + i) = field;
    }
    return struct_index;
}

uint32_t
RttiBuilder::to_typeid(const Vector<uint8_t>& bytes)
{
    if (bytes.length() <= 4) {
        uint32_t payload = 0;
        for (size_t i = 0; i < bytes.length(); i++)
            payload |= bytes[i] << (i * 8);
        if (payload <= kMaxTypeIdPayload)
            return MakeTypeId(payload, kTypeId_Inline);
    }

    uint32_t offset = type_pool_.add(bytes);
    return MakeTypeId(offset, kTypeId_Complex);
}

uint32_t
RttiBuilder::encode_signature(symbol* sym)
{
    Vector<uint8_t> bytes;

    uint32_t argc = 0;
    bool is_variadic = false;
    for (arginfo* arg = &sym->function()->args[0]; arg->ident; arg++) {
        if (arg->ident == iVARARGS)
            is_variadic = true;
        argc++;
    }
    if (argc > UCHAR_MAX)
        error(45);

    bytes.append((uint8_t)argc);
    if (is_variadic)
        bytes.append(cb::kVariadic);

    symbol* child = sym->array_return();
    if (child && child->dim.array.length) {
        encode_ret_array_into(bytes, child);
    } else if (sym->tag == pc_tag_void) {
        bytes.append(cb::kVoid);
    } else {
        encode_tag_into(bytes, sym->tag);
    }

    for (arginfo* arg = &sym->function()->args[0]; arg->ident; arg++) {
        int tag = arg->tag;
        int numdim = arg->numdim;
        if (arg->numdim && arg->idxtag[numdim - 1]) {
            int last_tag = arg->idxtag[numdim - 1];
            Type* last_type = gTypes.find(last_tag);
            if (last_type->isEnumStruct()) {
                tag = last_tag;
                numdim--;
            }
        }

        if (arg->ident == iREFERENCE)
            bytes.append(cb::kByRef);
        variable_type_t info = {tag, arg->dim, numdim, arg->is_const};
        encode_var_type(bytes, info);
    }

    return type_pool_.add(bytes);
}

uint32_t
RttiBuilder::add_enum(Type* type)
{
    TypeIdCache::Insert p = typeid_cache_.findForAdd(type);
    if (p.found())
        return p->value;

    uint32_t index = enums_->count();
    typeid_cache_.add(p, type, index);

    smx_rtti_enum entry;
    memset(&entry, 0, sizeof(entry));
    entry.name = names_->add(gAtoms, type->name());
    enums_->add(entry);
    return index;
}

uint32_t
RttiBuilder::add_funcenum(Type* type, funcenum_t* fe)
{
    TypeIdCache::Insert p = typeid_cache_.findForAdd(type);
    if (p.found())
        return p->value;

    // Reserve slot beforehand in case the type is recursive.
    uint32_t index = typedefs_->count();
    typeid_cache_.add(p, type, index);
    typedefs_->add();

    Vector<uint8_t> bytes;
    encode_signature_into(bytes, fe->entries.back());
    uint32_t signature = type_pool_.add(bytes);

    smx_rtti_typedef& def = typedefs_->at(index);
    def.name = names_->add(gAtoms, type->name());
    def.type_id = MakeTypeId(signature, kTypeId_Complex);
    return index;
}

uint32_t
RttiBuilder::add_typeset(Type* type, funcenum_t* fe)
{
    TypeIdCache::Insert p = typeid_cache_.findForAdd(type);
    if (p.found())
        return p->value;

    // Reserve slot beforehand in case the type is recursive.
    uint32_t index = typesets_->count();
    typeid_cache_.add(p, type, index);
    typesets_->add();

    uint32_t typecount = (uint32_t)fe->entries.length();

    Vector<uint8_t> bytes;
    CompactEncodeUint32(bytes, typecount);
    for (const auto& iter : fe->entries)
        encode_signature_into(bytes, iter);

    smx_rtti_typeset& entry = typesets_->at(index);
    entry.name = names_->add(gAtoms, type->name());
    entry.signature = type_pool_.add(bytes);
    return index;
}

void
RttiBuilder::encode_struct_into(Vector<uint8_t>& bytes, Type* type)
{
    bytes.append(cb::kClassdef);
    CompactEncodeUint32(bytes, add_struct(type));
}

void
RttiBuilder::encode_enum_into(Vector<uint8_t>& bytes, Type* type)
{
    bytes.append(cb::kEnum);
    CompactEncodeUint32(bytes, add_enum(type));
}

void
RttiBuilder::encode_enumstruct_into(Vector<uint8_t>& bytes, Type* type)
{
    bytes.append(cb::kEnumStruct);
    CompactEncodeUint32(bytes, add_enumstruct(type));
}

void
RttiBuilder::encode_ret_array_into(Vector<uint8_t>& bytes, symbol* sym)
{
    bytes.append(cb::kFixedArray);
    if (sym->tag == pc_tag_string)
        CompactEncodeUint32(bytes, sym->dim.array.length * 4);
    else
        CompactEncodeUint32(bytes, sym->dim.array.length);
    encode_tag_into(bytes, sym->tag);
}

static inline uint8_t
TagToRttiBytecode(int tag)
{
    if (tag == pc_tag_bool)
        return cb::kBool;
    if (tag == pc_anytag)
        return cb::kAny;
    if (tag == pc_tag_string)
        return cb::kChar8;
    if (tag == sc_rationaltag)
        return cb::kFloat32;
    if (tag == 0)
        return cb::kInt32;
    return 0;
}

void
RttiBuilder::encode_tag_into(Vector<uint8_t>& bytes, int tag)
{
    if (uint8_t b = TagToRttiBytecode(tag)) {
        bytes.append(b);
        return;
    }

    Type* type = gTypes.find(tag);
    assert(!type->isObject());

    if (type->isStruct()) {
        encode_struct_into(bytes, type);
        return;
    }

    if (type->isFunction()) {
        if (funcenum_t* fe = type->toFunction())
            encode_funcenum_into(bytes, type, fe);
        else
            bytes.append(cb::kTopFunction);
        return;
    }

    if (type->isEnumStruct()) {
        encode_enumstruct_into(bytes, type);
        return;
    }

    encode_enum_into(bytes, type);
}

void
RttiBuilder::encode_funcenum_into(Vector<uint8_t>& bytes, Type* type, funcenum_t* fe)
{
    if (fe->entries.length() == 1) {
        uint32_t index = add_funcenum(type, fe);
        bytes.append(cb::kTypedef);
        CompactEncodeUint32(bytes, index);
    } else {
        uint32_t index = add_typeset(type, fe);
        bytes.append(cb::kTypeset);
        CompactEncodeUint32(bytes, index);
    }
}

void
RttiBuilder::encode_signature_into(Vector<uint8_t>& bytes, functag_t* ft)
{
    bytes.append(cb::kFunction);
    bytes.append((uint8_t)ft->args.length());
    if (!ft->args.empty() && ft->args[ft->args.length() - 1].ident == iVARARGS)
        bytes.append(cb::kVariadic);
    if (ft->ret_tag == pc_tag_void)
        bytes.append(cb::kVoid);
    else
        encode_tag_into(bytes, ft->ret_tag);

    for (const auto& arg : ft->args) {
        if (arg.ident == iREFERENCE)
            bytes.append(cb::kByRef);

        variable_type_t info = {arg.tag, arg.dims, arg.dimcount, !!arg.fconst};
        encode_var_type(bytes, info);
    }
}

void
RttiBuilder::encode_var_type(Vector<uint8_t>& bytes, const variable_type_t& info)
{
    for (int i = 0; i < info.dimcount; i++) {
        if (info.dims[i] == 0) {
            bytes.append(cb::kArray);
        } else {
            bytes.append(cb::kFixedArray);
            if (i == info.dimcount - 1 && info.tag == pc_tag_string)
                CompactEncodeUint32(bytes, info.dims[i] * 4);
            else
                CompactEncodeUint32(bytes, info.dims[i]);
        }

        if (i != info.dimcount - 1 && info.is_const)
            bytes.append(cb::kConst);
    }
    if (info.is_const)
        bytes.append(cb::kConst);
    encode_tag_into(bytes, info.tag);
}

typedef SmxListSection<sp_file_natives_t> SmxNativeSection;
typedef SmxListSection<sp_file_publics_t> SmxPublicSection;
typedef SmxListSection<sp_file_pubvars_t> SmxPubvarSection;
typedef SmxBlobSection<sp_file_data_t> SmxDataSection;
typedef SmxBlobSection<sp_file_code_t> SmxCodeSection;

static void
assemble_to_buffer(SmxByteBuffer* buffer, memfile_t* fin)
{
    SmxBuilder builder;
    RefPtr<SmxNativeSection> natives = new SmxNativeSection(".natives");
    RefPtr<SmxPublicSection> publics = new SmxPublicSection(".publics");
    RefPtr<SmxPubvarSection> pubvars = new SmxPubvarSection(".pubvars");
    RefPtr<SmxDataSection> data = new SmxDataSection(".data");
    RefPtr<SmxCodeSection> code = new SmxCodeSection(".code");
    RefPtr<SmxNameTable> names = new SmxNameTable(".names");

    RttiBuilder rtti(names);

    Vector<function_entry> functions;

    // Sort globals.
    Vector<symbol*> global_symbols;
    for (symbol* sym = glbtab.next; sym; sym = sym->next)
        global_symbols.append(sym);
    qsort(global_symbols.buffer(), global_symbols.length(), sizeof(symbol*), sort_by_name);

    // Build the easy symbol tables.
    for (const auto& sym : global_symbols) {
        if (sym->ident == iFUNCTN) {
            if (sym->native) {
                // Set native addresses to -1 to indicate whether we've seen
                // them in the assembly yet.
                sym->setAddr(-1);
                continue;
            }

            // If a function is marked as missing it should not be a public function
            // with a declaration.
            if (sym->missing) {
                assert(!(sym->is_public && sym->defined));
                continue;
            }

            if (sym->is_public || (sym->usage & uREAD)) {
                function_entry entry;
                entry.sym = sym;
                if (sym->is_public) {
                    entry.name = sym->name();
                } else {
                    // Create a private name.
                    char private_name[sNAMEMAX * 3 + 1];
                    snprintf(private_name, sizeof(private_name), ".%d.%s", sym->addr(),
                             sym->name());

                    entry.name = private_name;
                }

                functions.append(entry);
                continue;
            }
        } else if (sym->ident == iVARIABLE || sym->ident == iARRAY || sym->ident == iREFARRAY) {
            if (sym->is_public || (sym->usage & (uREAD | uWRITTEN)) != 0) {
                sp_file_pubvars_t& pubvar = pubvars->add();
                pubvar.address = sym->addr();
                pubvar.name = names->add(sym->nameAtom());
            }
        }
    }

    // The public list must be sorted.
    qsort(functions.buffer(), functions.length(), sizeof(function_entry), sort_functions);
    for (size_t i = 0; i < functions.length(); i++) {
        function_entry& f = functions[i];
        symbol* sym = f.sym;

        assert(sym->addr() > 0);
        assert(sym->defined);
        assert(sym->codeaddr > sym->addr());

        sp_file_publics_t& pubfunc = publics->add();
        pubfunc.address = sym->addr();
        pubfunc.name = names->add(gAtoms, f.name.chars());

        sym->function()->funcid = (uint32_t(i) << 1) | 1;

        rtti.add_method(sym);
    }

    for (int i = 1; i <= sc_labnum; i++)
        sLabelTable.append(-1);
    assert(sLabelTable.length() == size_t(sc_labnum));

    // Generate buffers.
    AsmReader reader(fin);
    Vector<cell> code_buffer, data_buffer;
    generate_segment(reader, &code_buffer, &data_buffer);

    // Populate the native table.
    for (size_t i = 0; i < reader.native_list().length(); i++) {
        symbol* sym = reader.native_list()[i];
        assert(size_t(sym->addr()) == i);

        sp_file_natives_t& entry = natives->add();

        char testalias[sNAMEMAX + 1];
        if (lookup_alias(testalias, sym->name()))
            entry.name = names->add(gAtoms, "@");
        else
            entry.name = names->add(sym->nameAtom());

        rtti.add_native(sym);
    }

    // Set up the code section.
    code->header().codesize = code_buffer.length() * sizeof(cell);
    code->header().cellsize = sizeof(cell);
    code->header().codeversion =
        (pc_code_version) ? pc_code_version : SmxConsts::CODE_VERSION_SM_LEGACY;
    code->header().flags = CODEFLAG_DEBUG;
    code->header().main = 0;
    code->header().code = sizeof(sp_file_code_t);
    code->header().features = 0;
    code->setBlob((uint8_t*)code_buffer.buffer(), code_buffer.length() * sizeof(cell));

    // Set up the data section. Note pre-SourceMod 1.7, the |memsize| was
    // computed as AMX::stp, which included the entire memory size needed to
    // store the file. Here (in 1.7+), we allocate what is actually needed
    // by the plugin.
    data->header().datasize = data_buffer.length() * sizeof(cell);
    data->header().memsize =
        data->header().datasize + glb_declared * sizeof(cell) + pc_stksize * sizeof(cell);
    data->header().data = sizeof(sp_file_data_t);
    data->setBlob((uint8_t*)data_buffer.buffer(), data_buffer.length() * sizeof(cell));

    // Add tables in the same order SourceMod 1.6 added them.
    builder.add(code);
    builder.add(data);
    builder.add(publics);
    builder.add(pubvars);
    builder.add(natives);
    builder.add(names);
    rtti.finish(builder);

    builder.write(buffer);
}

static void
splat_to_binary(const char* binfname, const void* bytes, size_t size)
{
    // Note: error 161 will setjmp(), which skips destructors :(
    FILE* fp = fopen(binfname, "wb");
    if (!fp) {
        error(FATAL_ERROR_WRITE, binfname);
        return;
    }
    if (fwrite(bytes, 1, size, fp) != size) {
        fclose(fp);
        error(FATAL_ERROR_WRITE, binfname);
        return;
    }
    fclose(fp);
}

void
assemble(const char* binfname, memfile_t* fin)
{
    init_opcode_lookup();

    SmxByteBuffer buffer;
    assemble_to_buffer(&buffer, fin);

    // Buffer compression logic.
    sp_file_hdr_t* header = (sp_file_hdr_t*)buffer.bytes();

    if (sc_compression_level) {
        size_t region_size = header->imagesize - header->dataoffs;
        size_t zbuf_max = compressBound(region_size);
        std::unique_ptr<Bytef[]> zbuf = std::make_unique<Bytef[]>(zbuf_max);

        uLong new_disksize = zbuf_max;
        int err = compress2(zbuf.get(), &new_disksize, (Bytef*)(buffer.bytes() + header->dataoffs),
                            region_size, sc_compression_level);
        if (err == Z_OK) {
            header->disksize = new_disksize + header->dataoffs;
            header->compression = SmxConsts::FILE_COMPRESSION_GZ;

            ByteBuffer new_buffer;
            new_buffer.writeBytes(buffer.bytes(), header->dataoffs);
            new_buffer.writeBytes(zbuf.get(), new_disksize);

            splat_to_binary(binfname, new_buffer.bytes(), new_buffer.size());
            return;
        }

        pc_printf("Unable to compress, error %d\n", err);
        pc_printf("Falling back to no compression.\n");
    }

    header->disksize = 0;
    header->compression = SmxConsts::FILE_COMPRESSION_NONE;

    splat_to_binary(binfname, buffer.bytes(), buffer.size());
}
