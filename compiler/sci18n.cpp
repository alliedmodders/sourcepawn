/*  vim: set sts=4 sw=4 tw=99 ts=8 et:
 *
 *  Codepage translation to Unicode, and UTF-8 support
 *
 *  The translation is based on codepage mapping files that are distributed
 *  by the Unicode consortium, see ftp://ftp.unicode.org/Public/MAPPINGS/.
 *
 *  Character sets with a maximum of 256 codes are translated via a lookup
 *  table (these are Single-Byte Character Sets). Character sets like Shift-JIS
 *  with single-byte characters and multi-byte characters (introduced by a
 *  leader byte) are split into two tables: the 256-entry lookup table for
 *  the single-byte characters and an extended table for the multi-byte
 *  characters. The extended table is allocated dynamically; the lookup table
 *  is allocated statically, so loading SBCS tables cannot fail (if the tables
 *  themselves are valid, of course).
 *
 *  Copyright (c) ITB CompuPhase, 2004-2006
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
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <locale>
#include <codecvt>

#include <amtl/am-bits.h>

#include "errors.h"
#include "sc.h"
#include "scvars.h"

cell
get_utf8_char(const unsigned char* string, const unsigned char** endptr)
{
    const unsigned char* p = string;

    unsigned char ch = *p++;
    if (ch <= 0x7f) {
        *endptr = p;
        return ch;
    }

    // First byte starts with 11, then up to 4 additional 1s, and then a zero.
    // By inverting we can find the position of the zero.
    unsigned char inverted = (~ch) & 0xff;

    if (!inverted)
        return -1;

    unsigned int indicator_bit = ke::FindLeftmostBit32(inverted);
    if (indicator_bit == 0 || indicator_bit > 5)
        return -1;

    unsigned int mask = (1 << indicator_bit) - 1;
    cell result = ch & mask;

    unsigned int extra_bytes = 6 - indicator_bit;
    for (unsigned int i = 1; i <= extra_bytes; i++) {
        if ((*p & 0xc0) != 0x80) {
            result = -1;
            break;
        }

        result <<= 6;
        result |= (*p & 0x3f);
        p++;
    }

    *endptr = p;
    return result;
}

void
skip_utf8_bom(SourceFile* fp)
{
    auto resetpos = fp->Pos();

    static const size_t kBomSize = 3;
    unsigned char bom[kBomSize + 1];
    if (!fp->Read(bom, kBomSize))
        return;

    if (bom[0] == 0xef && bom[1] == 0xbb && bom[2] == 0xbf)
        return;

    fp->Reset(resetpos);
}

void UnicodeCodepointToUtf8(ucell codepoint, std::string* out) {
    std::wstring_convert<std::codecvt_utf8<char32_t>, char32_t> convert;

    char32_t cp = codepoint;
    *out += convert.to_bytes(&cp, &cp + 1);
}
