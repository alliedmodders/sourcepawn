/*  Pawn compiler - Staging buffer and optimizer
 *
 *  The staging buffer
 *  ------------------
 *  The staging buffer allows buffered output of generated code, deletion
 *  of redundant code, optimization by a tinkering process and reversing
 *  the ouput of evaluated expressions (which is used for the reversed
 *  evaluation of arguments in functions).
 *  Initially, stgwrite() writes to the file directly, but after a call to
 *  stgset(TRUE), output is redirected to the buffer. After a call to
 *  stgset(FALSE), stgwrite()'s output is directed to the file again. Thus
 *  only one routine is used for writing to the output, which can be
 *  buffered output or direct output.
 *
 *  staging buffer variables:   stgbuf  - the buffer
 *                              stgidx  - current index in the staging buffer
 *                              staging - if true, write to the staging buffer;
 *                                        if false, write to file directly.
 *
 * The peephole optimizer uses a dual "pipeline". The staging buffer (described
 * above) gets optimized for each expression or sub-expression in a function
 * call. The peephole optimizer is recursive, but it does not span multiple
 * sub-expressions. However, the data gets written to a second buffer that
 * behaves much like the staging buffer. This second buffer gathers all
 * optimized strings from the staging buffer for a complete expression. The
 * peephole optmizer then runs over this second buffer to find optimzations
 * across function parameter boundaries.
 *
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
#include <stdio.h>
#include <stdlib.h> /* for atoi() */
#include <string.h>
#include "emitter.h"
#include "errors.h"
#include "lexer.h"
#include "libpawnc.h"
#include "sc.h"
#include "scvars.h"

#define sSTG_GROW 512
#define sSTG_MAX 20480

static char* stgbuf = NULL;
static int stgmax = 0; /* current size of the staging buffer */

static char* stgpipe = NULL;
static int pipemax = 0; /* current size of the stage pipe, a second staging buffer */
static int pipeidx = 0;

#define CHECK_STGBUFFER(index)  \
    if ((int)(index) >= stgmax) \
    grow_stgbuffer(&stgbuf, &stgmax, (index) + 1)
#define CHECK_STGPIPE(index)     \
    if ((int)(index) >= pipemax) \
    grow_stgbuffer(&stgpipe, &pipemax, (index) + 1)

static void
grow_stgbuffer(char** buffer, int* curmax, int requiredsize)
{
    char* p;
    int clear = (*buffer == NULL); /* if previously none, empty buffer explicitly */

    assert(*curmax < requiredsize);
    /* if the staging buffer (holding intermediate code for one line) grows
     * over a few kBytes, there is probably a run-away expression
     */
    if (requiredsize > sSTG_MAX)
        error(FATAL_ERROR_OOM);
    *curmax = requiredsize + sSTG_GROW;
    if (*buffer != NULL)
        p = (char*)realloc(*buffer, *curmax * sizeof(char));
    else
        p = (char*)malloc(*curmax * sizeof(char));
    if (p == NULL)
        error(FATAL_ERROR_OOM);
    *buffer = p;
    if (clear)
        **buffer = '\0';
}

void
stgbuffer_cleanup(void)
{
    if (stgbuf != NULL) {
        free(stgbuf);
        stgbuf = NULL;
        stgmax = 0;
    }
    if (stgpipe != NULL) {
        free(stgpipe);
        stgpipe = NULL;
        pipemax = 0;
        pipeidx = 0;
    }
}

/* the variables "stgidx" and "staging" are declared in "scvars.c" */

static int
filewrite(char* str)
{
    if (sc_status == statWRITE)
        return pc_writeasm(outf, str);
    return TRUE;
}

/*  stgwrite
 *
 *  Writes the string "st" to the staging buffer or to the output file. In the
 *  case of writing to the staging buffer, the terminating byte of zero is
 *  copied too, but... the optimizer can only work on complete lines (not on
 *  fractions of it. Therefore if the string is staged, if the last character
 *  written to the buffer is a '\0' and the previous-to-last is not a '\n',
 *  the string is concatenated to the last string in the buffer (the '\0' is
 *  overwritten). This also means an '\n' used in the middle of a string isn't
 *  recognized and could give wrong results with the optimizer.
 *  Even when writing to the output file directly, all strings are buffered
 *  until a whole line is complete.
 *
 *  Global references: stgidx  (altered)
 *                     stgbuf  (altered)
 *                     staging (referred to only)
 */
void
stgwrite(const char* st)
{
    int len;
    int st_len = strlen(st);

    if (staging) {
        assert(
            stgidx == 0 ||
            stgbuf !=
                NULL); /* staging buffer must be valid if there is (apparently) something in it */
        if (stgidx >= 2 && stgbuf[stgidx - 1] == '\0' && stgbuf[stgidx - 2] != '\n')
            stgidx -= 1; /* overwrite last '\0' */

        CHECK_STGBUFFER(stgidx + st_len + 1);
        memcpy(stgbuf + stgidx, st, st_len + 1);
        stgidx += st_len + 1;
    } else {
        len = (stgbuf != NULL) ? strlen(stgbuf) : 0;
        CHECK_STGBUFFER(len + st_len + 1);
        memcpy(stgbuf + len, st, st_len + 1);
        len += st_len;
        if (len > 0 && stgbuf[len - 1] == '\n') {
            filewrite(stgbuf);
            stgbuf[0] = '\0';
        }
    }
}

/*  stgout
 *
 *  Writes the staging buffer to the output file via stgstring() (for
 *  reversing expressions in the buffer) and stgopt() (for optimizing). It
 *  resets "stgidx".
 *
 *  Global references: stgidx  (altered)
 *                     stgbuf  (referred to only)
 *                     staging (referred to only)
 */
void
stgout(int index)
{
    int idx;

    if (!staging)
        return;
    assert(pipeidx == 0);

    /* first pass: sub-expressions */
    stgidx = index;

    /* second pass: optimize the buffer created in the first pass */
    if (sc_status == statWRITE) {
        /* there is no sense in re-optimizing if the order of the sub-expressions
         * did not change; so output directly
         */
        for (idx = 0; idx < pipeidx; idx += strlen(stgpipe + idx) + 1)
            filewrite(stgpipe + idx);
    }
    pipeidx = 0; /* reset second pipe */
}

typedef struct {
    char *start, *end;
} argstack;

/*  stgdel
 *
 *  Scraps code from the staging buffer by resetting "stgidx" to "index".
 *
 *  Global references: stgidx (altered)
 *                     staging (reffered to only)
 */
void
stgdel(int index, cell code_index)
{
    if (staging) {
        stgidx = index;
        code_idx = code_index;
    }
}

int
stgget(int* index, cell* code_index)
{
    if (staging) {
        *index = stgidx;
        *code_index = code_idx;
    }
    return staging;
}

/*  stgset
 *
 *  Sets staging on or off. If it's turned off, the staging buffer must be
 *  initialized to an empty string. If it's turned on, the routine makes sure
 *  the index ("stgidx") is set to 0 (it should already be 0).
 *
 *  Global references: staging  (altered)
 *                     stgidx   (altered)
 *                     stgbuf   (contents altered)
 */
void
stgset(int onoff)
{
    staging = onoff;
    if (staging) {
        assert(stgidx == 0);
        stgidx = 0;
        CHECK_STGBUFFER(stgidx);
        /* write any contents that may be put in the buffer by stgwrite()
         * when "staging" was 0
         */
        if (strlen(stgbuf) > 0)
            filewrite(stgbuf);
    }
    stgbuf[0] = '\0';
}
