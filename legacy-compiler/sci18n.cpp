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

#include <amtl/am-bits.h>
#include "errors.h"
#include "sc.h"

void UnicodeCodepointToUtf8(ucell codepoint, std::string* out) {
    if (codepoint <= 0x7F) {
        *out += static_cast<char>(codepoint);
    } else if (codepoint <= 0x7FF) {
        *out += static_cast<char>(0xC0 | ((codepoint >> 6) & 0x1F));
        *out += static_cast<char>(0x80 | (codepoint & 0x3F));
    } else if (codepoint <= 0xFFFF) {
        *out += static_cast<char>(0xE0 | ((codepoint >> 12) & 0x0F));
        *out += static_cast<char>(0x80 | ((codepoint >> 6) & 0x3F));
        *out += static_cast<char>(0x80 | (codepoint & 0x3F));
    } else if (codepoint <= 0x10FFFF) {
        *out += static_cast<char>(0xF0 | ((codepoint >> 18) & 0x07));
        *out += static_cast<char>(0x80 | ((codepoint >> 12) & 0x3F));
        *out += static_cast<char>(0x80 | ((codepoint >> 6) & 0x3F));
        *out += static_cast<char>(0x80 | (codepoint & 0x3F));
    }
}
