/*  Codepage translation to Unicode, and UTF-8 support
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
#include "errors.h"
#include "libpawnc.h"
#include "sc.h"
#include "scvars.h"

#if !defined TRUE
#    define FALSE 0
#    define TRUE 1
#endif
#if !defined _MAX_PATH
#    define _MAX_PATH 250
#endif
#if !defined DIRSEP_CHAR
#    if defined __linux__ || defined __FreeBSD__ || defined __OpenBSD__
#        define DIRSEP_CHAR '/'
#    elif defined macintosh
#        define DIRSEP_CHAR ':'
#    else
#        define DIRSEP_CHAR '\\'
#    endif
#endif

#if !defined ELEMENTS
#    define ELEMENTS(array) (sizeof(array) / sizeof(array[0]))
#endif

cell
get_utf8_char(const unsigned char* string, const unsigned char** endptr)
{
    int follow = 0;
    long lowmark = 0;
    unsigned char ch;
    cell result = 0;

    if (endptr != NULL)
        *endptr = string;

    for (;;) {
        ch = *string++;

        if (follow > 0 && (ch & 0xc0) == 0x80) {
            /* leader code is active, combine with earlier code */
            result = (result << 6) | (ch & 0x3f);
            if (--follow == 0) {
                /* encoding a character in more bytes than is strictly needed,
                 * is not really valid UTF-8; we are strict here to increase
                 * the chance of heuristic dectection of non-UTF-8 text
                 * (JAVA writes zero bytes as a 2-byte code UTF-8, which is invalid)
                 */
                if (result < lowmark)
                    return -1;
                /* the code positions 0xd800--0xdfff and 0xfffe & 0xffff do not
                 * exist in UCS-4 (and hence, they do not exist in Unicode)
                 */
                if ((result >= 0xd800 && result <= 0xdfff) || result == 0xfffe || result == 0xffff)
                    return -1;
            }
            break;
        } else if (follow == 0 && (ch & 0x80) == 0x80) {
            /* UTF-8 leader code */
            if ((ch & 0xe0) == 0xc0) {
                /* 110xxxxx 10xxxxxx */
                follow = 1;
                lowmark = 0x80L;
                result = ch & 0x1f;
            } else if ((ch & 0xf0) == 0xe0) {
                /* 1110xxxx 10xxxxxx 10xxxxxx (16 bits, BMP plane) */
                follow = 2;
                lowmark = 0x800L;
                result = ch & 0x0f;
            } else if ((ch & 0xf8) == 0xf0) {
                /* 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx */
                follow = 3;
                lowmark = 0x10000L;
                result = ch & 0x07;
            } else if ((ch & 0xfc) == 0xf8) {
                /* 111110xx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx */
                follow = 4;
                lowmark = 0x200000L;
                result = ch & 0x03;
            } else if ((ch & 0xfe) == 0xfc) {
                /* 1111110x 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx (32 bits) */
                follow = 5;
                lowmark = 0x4000000L;
                result = ch & 0x01;
            } else {
                /* this is invalid UTF-8 */
                return -1;
            }
        } else if (follow == 0 && (ch & 0x80) == 0x00) {
            /* 0xxxxxxx (US-ASCII) */
            result = ch;
            break;
        } else {
            /* this is invalid UTF-8 */
            return -1;
        }
    }

    if (endptr != NULL)
        *endptr = string;
    return result;
}

void
skip_utf8_bom(void* fp)
{
    void* resetpos = pc_getpossrc(fp);

    static const size_t kBomSize = 3;
    unsigned char bom[kBomSize + 1];
    if (!pc_readsrc(fp, bom, kBomSize))
        return;

    if (bom[0] == 0xef && bom[1] == 0xbb && bom[2] == 0xbf)
        return;

    pc_resetsrc(fp, resetpos);
}
