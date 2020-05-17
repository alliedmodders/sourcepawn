// vim: set ts=8 sts=4 sw=4 tw=99 et:
/*  Pawn compiler - File input, preprocessing and lexical analysis functions
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
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <utility>

#include "lexer.h"
#include <amtl/am-hashmap.h>
#include <amtl/am-platform.h>
#include <amtl/am-string.h>
#include <sp_typeutil.h>
#include "emitter.h"
#include "errors.h"
#include "libpawnc.h"
#include "optimizer.h"
#include "sc.h"
#include "sci18n.h"
#include "sclist.h"
#include "sctracker.h"
#include "scvars.h"
#include "sp_symhash.h"
#include "types.h"

using namespace sp;

/* flags for litchar() */
#define UTF8MODE 0x1
static cell litchar(const unsigned char** lptr, int flags);

static void substallpatterns(unsigned char* line, int buffersize);
static int alpha(char c);

#define SKIPMODE 1     /* bit field in "#if" stack */
#define PARSEMODE 2    /* bit field in "#if" stack */
#define HANDLED_ELSE 4 /* bit field in "#if" stack */
#define SKIPPING (skiplevel > 0 && (ifstack[skiplevel - 1] & SKIPMODE) == SKIPMODE)

static short icomment; /* currently in multiline comment? */
static ke::Vector<short> sCommentStack;
static ke::Vector<short> sPreprocIfStack;
static char ifstack[sCOMP_STACK]; /* "#if" stack */
static short iflevel;             /* nesting level if #if/#else/#endif */
static short skiplevel; /* level at which we started skipping (including nested #if .. #endif) */
static unsigned char term_expr[] = "";
static int listline = -1; /* "current line" for the list file */

ke::HashMap<CharsAndLength, int, KeywordTablePolicy> sKeywords;

int
plungequalifiedfile(char* name)
{
    static const char* extensions[] = {".inc", ".p", ".pawn"};

    void* fp;
    char* ext;
    size_t ext_idx;

    ext_idx = 0;
    do {
        fp = pc_opensrc(name);
        ext = strchr(name, '\0'); /* save position */
        if (fp == NULL) {
            /* try to append an extension */
            strcpy(ext, extensions[ext_idx]);
            fp = pc_opensrc(name);
            if (fp == NULL)
                *ext = '\0'; /* on failure, restore filename */
        }
        ext_idx++;
    } while (fp == NULL && ext_idx < (sizeof extensions / sizeof extensions[0]));
    if (fp == NULL) {
        *ext = '\0'; /* restore filename */
        return FALSE;
    }
    if (sc_showincludes && sc_status == statFIRST) {
        fprintf(stdout, "Note: including file: %s\n", name);
    }
    gInputFileStack.append(inpf);
    gInputFilenameStack.append(inpfname);
    sPreprocIfStack.append(iflevel);
    assert(!SKIPPING);
    assert(skiplevel == iflevel); /* these two are always the same when "parsing" */
    sCommentStack.append(icomment);
    gCurrentFileStack.append(fcurrent);
    gCurrentLineStack.append(fline);
    inpfname = strdup(name); /* set name of include file */
    if (inpfname == NULL)
        error(FATAL_ERROR_OOM);
    inpf = fp; /* set input file pointer to include file */
    fnumber++;
    fline = 0; /* set current line number to 0 */
    fcurrent = fnumber;
    icomment = 0;               /* not in a comment */
    insert_dbgfile(inpfname);   /* attach to debug information */
    insert_inputfile(inpfname); /* save for the error system */
    assert(sc_status == statFIRST || strcmp(get_inputfile(fcurrent), inpfname) == 0);
    setfiledirect(inpfname); /* (optionally) set in the list file */
    listline = -1;           /* force a #line directive when changing the file */
    skip_utf8_bom(inpf);
    return TRUE;
}

int
plungefile(char* name, int try_currentpath, int try_includepaths)
{
    int result = FALSE;

    if (try_currentpath) {
        result = plungequalifiedfile(name);
        if (!result) {
            /* failed to open the file in the active directory, try to open the file
             * in the same directory as the current file --but first check whether
             * there is a (relative) path for the current file
             */
            char* ptr;
            if ((ptr = strrchr(inpfname, DIRSEP_CHAR)) != 0) {
                int len = (int)(ptr - inpfname) + 1;
                if (len + strlen(name) < _MAX_PATH) {
                    char path[_MAX_PATH];
                    SafeStrcpyN(path, sizeof(path), inpfname, len);
                    SafeStrcat(path, sizeof(path), name);
                    result = plungequalifiedfile(path);
                }
            }
        }
    }

    if (try_includepaths && name[0] != DIRSEP_CHAR) {
        int i;
        char* ptr;
        for (i = 0; !result && (ptr = get_path(i)) != NULL; i++) {
            char path[_MAX_PATH];
            SafeSprintf(path, sizeof(path), "%s%s", ptr, name);
            result = plungequalifiedfile(path);
        }
    }
    return result;
}

static void
check_empty(const unsigned char* lptr)
{
    /* verifies that the string contains only whitespace */
    while (*lptr <= ' ' && *lptr != '\0')
        lptr++;
    if (*lptr != '\0')
        error(38); /* extra characters on line */
}

/*  doinclude
 *
 *  Gets the name of an include file, pushes the old file on the stack and
 *  sets some options. This routine doesn't use lex(), since lex() doesn't
 *  recognize file names (and directories).
 *
 *  Global references: inpf     (altered)
 *                     inpfname (altered)
 *                     fline    (altered)
 *                     lptr     (altered)
 */
static void
doinclude(int silent)
{
    char name[_MAX_PATH];
    char c;
    size_t i;
    int result;

    while (*lptr <= ' ' && *lptr != '\0') /* skip leading whitespace */
        lptr++;
    if (*lptr == '<' || *lptr == '\"') {
        c = (char)((*lptr == '\"') ? '\"' : '>'); /* termination character */
        lptr++;
        while (*lptr <= ' ' && *lptr != '\0') /* skip whitespace after quote */
            lptr++;
    } else {
        c = '\0';
    }

    i = 0;
    while (*lptr != c && *lptr != '\0' && i < sizeof name - 1) /* find the end of the string */
        name[i++] = *lptr++;
    while (i > 0 && name[i - 1] <= ' ')
        i--; /* strip trailing whitespace */
    assert(i < sizeof name);
    name[i] = '\0'; /* zero-terminate the string */

    if (*lptr != c) { /* verify correct string termination */
        error(37);    /* invalid string */
        return;
    }
    if (c != '\0')
        check_empty(lptr + 1); /* verify that the rest of the line is whitespace */

    result = plungefile(name, (c != '>'), TRUE);
    if (!result && !silent)
        error(FATAL_ERROR_READ, name);
}

/*  readline
 *
 *  Reads in a new line from the input file pointed to by "inpf". readline()
 *  concatenates lines that end with a \ with the next line. If no more data
 *  can be read from the file, readline() attempts to pop off the previous file
 *  from the stack. If that fails too, it sets "freading" to 0.
 *
 *  Global references: inpf,fline,inpfname,freading,icomment (altered)
 */
static void
readline(unsigned char* line)
{
    int num, cont;
    unsigned char* ptr;
    symbol* sym;

    if (lptr == term_expr)
        return;
    num = sLINEMAX;
    cont = FALSE;
    do {
        if (inpf == NULL || pc_eofsrc(inpf)) {
            if (cont)
                error(49); /* invalid line continuation */
            if (inpf != NULL && inpf != inpf_org)
                pc_closesrc(inpf);
            if (gCurrentLineStack.empty()) {
                freading = FALSE;
                *line = '\0';
                /* when there is nothing more to read, the #if/#else stack should
                 * be empty and we should not be in a comment
                 */
                assert(iflevel >= 0);
                if (iflevel > 0)
                    error(1, "#endif", "-end of file-");
                else if (icomment != 0)
                    error(1, "*/", "-end of file-");
                return;
            }
            fline = gCurrentLineStack.popCopy();
            fcurrent = gCurrentFileStack.popCopy();
            icomment = sCommentStack.popCopy();
            iflevel = sPreprocIfStack.popCopy();
            skiplevel = iflevel; /* this condition held before including the file */
            assert(!SKIPPING);   /* idem ditto */
            free(inpfname);      /* return memory allocated for the include file name */
            inpfname = gInputFilenameStack.popCopy();
            inpf = gInputFileStack.popCopy();
            insert_dbgfile(inpfname);
            setfiledirect(inpfname);
            assert(sc_status == statFIRST || strcmp(get_inputfile(fcurrent), inpfname) == 0);
            listline = -1; /* force a #line directive when changing the file */
        }

        if (pc_readsrc(inpf, line, num) == NULL) {
            *line = '\0'; /* delete line */
            cont = FALSE;
        } else {
            /* check whether to erase leading spaces */
            if (cont) {
                unsigned char* ptr = line;
                while (*ptr <= ' ' && *ptr != '\0')
                    ptr++;
                if (ptr != line)
                    memmove(line, ptr, strlen((char*)ptr) + 1);
            }
            cont = FALSE;
            /* check whether a full line was read */
            if (strchr((char*)line, '\n') == NULL && !pc_eofsrc(inpf))
                error(75); /* line too long */
            /* check if the next line must be concatenated to this line */
            if ((ptr = (unsigned char*)strchr((char*)line, '\n')) == NULL)
                ptr = (unsigned char*)strchr((char*)line, '\r');
            if (ptr != NULL && ptr > line) {
                assert(*(ptr + 1) == '\0'); /* '\n' or '\r' should be last in the string */
                while (ptr > line && *ptr <= ' ')
                    ptr--; /* skip trailing whitespace */
                if (*ptr == '\\') {
                    cont = TRUE;
                    /* set '\a' at the position of '\\' to make it possible to check
                     * for a line continuation in a single line comment (error 49)
                     */
                    *ptr++ = '\a';
                    *ptr = '\0'; /* erase '\n' (and any trailing whitespace) */
                }
            }
            num -= strlen((char*)line);
            line += strlen((char*)line);
        }
        fline += 1;
        sym = findconst("__LINE__");
        assert(sym != NULL);
        sym->setAddr(fline);
    } while (num >= 0 && cont);
}

/*  stripcom
 *
 *  Replaces all comments from the line by space characters. It updates
 *  a global variable ("icomment") for multiline comments.
 *
 *  This routine also supports the C++ extension for single line comments.
 *  These comments are started with "//" and end at the end of the line.
 *
 *  The function also detects (and manages) "documentation comments". The
 *  global variable "icomment" is set to 2 for documentation comments.
 *
 *  Global references: icomment  (private to "stripcom")
 */
static void
stripcom(unsigned char* line)
{
    char c;
#define COMMENT_LIMIT 100
#define COMMENT_MARGIN 40 /* length of the longest word */
    char comment[COMMENT_LIMIT + COMMENT_MARGIN];
    int commentidx = 0;
    int skipstar = TRUE;

    while (*line) {
        if (icomment != 0) {
            if (*line == '*' && *(line + 1) == '/') {
                if (icomment == 2) {
                    assert(commentidx < COMMENT_LIMIT + COMMENT_MARGIN);
                    comment[commentidx] = '\0';
                }
                icomment = 0; /* comment has ended */
                *line = ' ';  /* replace '*' and '/' characters by spaces */
                *(line + 1) = ' ';
                line += 2;
            } else {
                if (*line == '/' && *(line + 1) == '*')
                    error(216); /* nested comment */
                /* collect the comment characters in a string */
                if (icomment == 2) {
                    if (skipstar && ((*line != '\0' && *line <= ' ') || *line == '*')) {
                        /* ignore leading whitespace and '*' characters */
                    } else if (commentidx < COMMENT_LIMIT + COMMENT_MARGIN - 1) {
                        comment[commentidx++] = (char)((*line != '\n') ? *line : ' ');
                        if (commentidx > COMMENT_LIMIT && *line != '\0' && *line <= ' ') {
                            comment[commentidx] = '\0';
                            commentidx = 0;
                        }
                        skipstar = FALSE;
                    }
                }
                *line = ' '; /* replace comments by spaces */
                line += 1;
            }
        } else {
            if (*line == '/' && *(line + 1) == '*') {
                icomment = 1; /* start comment */
                /* there must be two "*" behind the slash and then white space */
                if (*(line + 2) == '*' && *(line + 3) <= ' ') {
                    icomment = 2; /* documentation comment */
                }
                commentidx = 0;
                skipstar = TRUE;
                *line = ' '; /* replace '/' and '*' characters by spaces */
                *(line + 1) = ' ';
                line += 2;
                if (icomment == 2)
                    *line++ = ' ';
            } else if (*line == '/' && *(line + 1) == '/') { /* comment to end of line */
                if (strchr((char*)line, '\a') != NULL)
                    error(49); /* invalid line continuation */
                if (*(line + 2) == '/' && *(line + 3) <= ' ') {
                    /* documentation comment */
                    char* str = (char*)line + 3;
                    char* end;
                    while (*str <= ' ' && *str != '\0')
                        str++; /* skip leading whitespace */
                    if ((end = strrchr(str, '\n')) != NULL)
                        *end = '\0'; /* erase trailing '\n' */
                }
                *line++ = '\n'; /* put "newline" at first slash */
                *line = '\0';   /* put "zero-terminator" at second slash */
            } else {
                if (*line == '\"' || *line == '\'') { /* leave literals unaltered */
                    c = *line;                        /* ending quote, single or double */
                    line += 1;
                    while (*line != c && *line != '\0') {
                        if (*line == sc_ctrlchar && *(line + 1) != '\0')
                            line += 1; /* skip escape character (but avoid skipping past '\0' */
                        line += 1;
                    }
                    line += 1; /* skip final quote */
                } else {
                    line += 1;
                }
            }
        }
    }
    if (icomment == 2) {
        assert(commentidx < COMMENT_LIMIT + COMMENT_MARGIN);
        comment[commentidx] = '\0';
    }
}

/*  btoi
 *
 *  Attempts to interpret a numeric symbol as a binary value. On success
 *  it returns the number of characters processed (so the line pointer can be
 *  adjusted) and the value is stored in "val". Otherwise it returns 0 and
 *  "val" is garbage.
 *
 *  A binary value must start with "0b"
 */
static int
btoi(cell* val, const unsigned char* curptr)
{
    const unsigned char* ptr;

    *val = 0;
    ptr = curptr;
    if (*ptr == '0' && *(ptr + 1) == 'b') {
        ptr += 2;
        while (*ptr == '0' || *ptr == '1' || *ptr == '_') {
            if (*ptr != '_')
                *val = (*val << 1) | (*ptr - '0');
            ptr++;
        }
    } else {
        return 0;
    }
    if (alphanum(*ptr)) /* number must be delimited by non-alphanumeric char */
        return 0;
    else
        return (int)(ptr - curptr);
}

/*  otoi
 *
 *  Attempts to interpret a numeric symbol as a octal value. On
 *  success it returns the number of characters processed and the value is
 *  stored in "val". Otherwise it return 0 and "val" is garbage.
 *
 *  An octal value must start with "0o"
 */
static int
otoi(cell* val, const unsigned char* curptr)
{
    const unsigned char* ptr;

    *val = 0;
    ptr = curptr;
    if (!isdigit(*ptr)) /* should start with digit */
        return 0;
    if (*ptr == '0' && *(ptr + 1) == 'o') {
        ptr += 2;
        while (isoctal(*ptr) || *ptr == '_') {
            if (*ptr != '_') {
                assert(isoctal(*ptr));
                *val = (*val << 3) + (*ptr - '0');
            }
            ptr++;
        }
    } else {
        return 0;
    }
    if (alphanum(*ptr)) /* number must be delimited by non-alphanumeric char */
        return 0;
    else
        return (int)(ptr - curptr);
}

/*  dtoi
 *
 *  Attempts to interpret a numeric symbol as a decimal value. On success
 *  it returns the number of characters processed and the value is stored in
 *  "val". Otherwise it returns 0 and "val" is garbage.
 */
static int
dtoi(cell* val, const unsigned char* curptr)
{
    const unsigned char* ptr;

    *val = 0;
    ptr = curptr;
    if (!isdigit(*ptr)) /* should start with digit */
        return 0;
    while (isdigit(*ptr) || *ptr == '_') {
        if (*ptr != '_')
            *val = (*val * 10) + (*ptr - '0');
        ptr++;
    }
    if (alphanum(*ptr)) /* number must be delimited by non-alphanumerical */
        return 0;
    if (*ptr == '.' && isdigit(*(ptr + 1)))
        return 0; /* but a fractional part must not be present */
    return (int)(ptr - curptr);
}

/*  htoi
 *
 *  Attempts to interpret a numeric symbol as a hexadecimal value. On
 *  success it returns the number of characters processed and the value is
 *  stored in "val". Otherwise it return 0 and "val" is garbage.
 */
static int
htoi(cell* val, const unsigned char* curptr)
{
    const unsigned char* ptr;

    *val = 0;
    ptr = curptr;
    if (!isdigit(*ptr)) /* should start with digit */
        return 0;
    if (*ptr == '0' && *(ptr + 1) == 'x') { /* C style hexadecimal notation */
        ptr += 2;
        while (ishex(*ptr) || *ptr == '_') {
            if (*ptr != '_') {
                assert(ishex(*ptr));
                *val = *val << 4;
                if (isdigit(*ptr))
                    *val += (*ptr - '0');
                else
                    *val += (tolower(*ptr) - 'a' + 10);
            }
            ptr++;
        }
    } else {
        return 0;
    }
    if (alphanum(*ptr))
        return 0;
    else
        return (int)(ptr - curptr);
}

/*  ftoi
 *
 *  Attempts to interpret a numeric symbol as a rational number, either as
 *  IEEE 754 single/double precision floating point or as a fixed point integer.
 *  On success it returns the number of characters processed and the value is
 *  stored in "val". Otherwise it returns 0 and "val" is unchanged.
 *
 *  Pawn has stricter definition for rational numbers than most:
 *  o  the value must start with a digit; ".5" is not a valid number, you
 *     should write "0.5"
 *  o  a period must appear in the value, even if an exponent is given; "2e3"
 *     is not a valid number, you should write "2.0e3"
 *  o  at least one digit must follow the period; "6." is not a valid number,
 *     you should write "6.0"
 */
static int
ftoi(cell* val, const unsigned char* curptr)
{
    const unsigned char* ptr;
    double fnum, ffrac, fmult;
    unsigned long dnum, dbase = 1;
    int ignore;

    fnum = 0.0;
    dnum = 0L;
    ptr = curptr;
    if (!isdigit(*ptr)) /* should start with digit */
        return 0;
    while (isdigit(*ptr) || *ptr == '_') {
        if (*ptr != '_') {
            fnum = (fnum * 10.0) + (*ptr - '0');
            dnum = (dnum * 10L) + (*ptr - '0') * dbase;
        }
        ptr++;
    }
    if (*ptr != '.')
        return 0; /* there must be a period */
    ptr++;
    if (!isdigit(*ptr)) /* there must be at least one digit after the dot */
        return 0;
    ffrac = 0.0;
    fmult = 1.0;
    ignore = FALSE;
    while (isdigit(*ptr) || *ptr == '_') {
        if (*ptr != '_') {
            ffrac = (ffrac * 10.0) + (*ptr - '0');
            fmult = fmult / 10.0;
            dbase /= 10L;
            dnum += (*ptr - '0') * dbase;
        }
        ptr++;
    }
    fnum += ffrac * fmult; /* form the number so far */
    if (*ptr == 'e') {     /* optional fractional part */
        int exp, sign;
        ptr++;
        if (*ptr == '-') {
            sign = -1;
            ptr++;
        } else {
            sign = 1;
        }
        if (!isdigit(*ptr)) /* 'e' should be followed by a digit */
            return 0;
        exp = 0;
        while (isdigit(*ptr)) {
            exp = (exp * 10) + (*ptr - '0');
            ptr++;
        }
        fmult = pow(10.0, exp * sign);
        fnum *= fmult;
        dnum *= (unsigned long)(fmult + 0.5);
    }

    /* floating point */
    float value = (float)fnum;
    *val = sp::FloatCellUnion(value).cell;
    return (int)(ptr - curptr);
}

/*  number
 *
 *  Reads in a number (binary, decimal or hexadecimal). It returns the number
 *  of characters processed or 0 if the symbol couldn't be interpreted as a
 *  number (in this case the argument "val" remains unchanged). This routine
 *  relies on the 'early dropout' implementation of the logical or (||)
 *  operator.
 *
 *  Note: the routine doesn't check for a sign (+ or -). The - is checked
 *        for at "hier2()" (in fact, it is viewed as an operator, not as a
 *        sign) and the + is invalid (as in K&R C, and unlike ANSI C).
 */
static int
number(cell* val, const unsigned char* curptr)
{
    int i;
    cell value;

    if ((i = btoi(&value, curptr)) != 0     /* binary? */
        || (i = htoi(&value, curptr)) != 0  /* hexadecimal? */
        || (i = dtoi(&value, curptr)) != 0  /* decimal? */
        || (i = otoi(&value, curptr)) != 0) /* octal? */
    {
        *val = value;
        return i;
    } else {
        return 0; /* else not a number */
    }
}

static void
chrcat(char* str, char chr)
{
    str = strchr(str, '\0');
    *str++ = chr;
    *str = '\0';
}

static int
preproc_expr(cell* val, int* tag)
{
    int result;
    int index;
    cell code_index;
    char* term;

    /* Disable staging; it should be disabled already because
     * expressions may not be cut off half-way between conditional
     * compilations. Reset the staging index, but keep the code
     * index.
     */
    if (stgget(&index, &code_index)) {
        error(57); /* unfinished expression */
        stgdel(0, code_index);
        stgset(FALSE);
    }
    assert((lptr - pline) < (int)strlen((char*)pline)); /* lptr must point inside the string */
    /* preprocess the string */
    substallpatterns(pline, sLINEMAX);
    assert((lptr - pline) <
           (int)strlen((char*)pline)); /* lptr must STILL point inside the string */
    /* append a special symbol to the string, so the expression
     * analyzer won't try to read a next line when it encounters
     * an end-of-line
     */
    assert(strlen((char*)pline) < sLINEMAX);
    term = strchr((char*)pline, '\0');
    assert(term != NULL);
    chrcat((char*)pline, PREPROC_TERM); /* the "DEL" code (see SC.H) */
    result = exprconst(val, tag, NULL); /* get value (or 0 on error) */
    *term = '\0';                       /* erase the token (if still present) */
    lexclr(FALSE);                      /* clear any "pushed" tokens */
    return result;
}

/* getstring
 * Returns returns a pointer behind the closing quote or to the other
 * character that caused the input to be ended.
 */
static const unsigned char*
getstring(unsigned char* dest, int max, const unsigned char* line)
{
    assert(dest != NULL && line != NULL);
    *dest = '\0';
    while (*line <= ' ' && *line != '\0')
        line++; /* skip whitespace */
    if (*line == '"') {
        int len = 0;
        line++; /* skip " */
        while (*line != '"' && *line != '\0') {
            if (len < max - 1)
                dest[len++] = *line;
            line++;
        }
        dest[len] = '\0';
        if (*line == '"')
            line++; /* skip closing " */
        else
            error(37); /* invalid string */
    } else {
        error(37); /* invalid string */
    }
    return line;
}

enum {
    CMD_NONE,
    CMD_TERM,
    CMD_EMPTYLINE,
    CMD_CONDFALSE,
    CMD_INCLUDE,
    CMD_DEFINE,
    CMD_IF,
    CMD_DIRECTIVE,
};

/*  command
 *
 *  Recognizes the compiler directives. The function returns:
 *     CMD_NONE         the line must be processed
 *     CMD_TERM         a pending expression must be completed before processing further lines
 *     Other value: the line must be skipped, because:
 *     CMD_CONDFALSE    false "#if.." code
 *     CMD_EMPTYLINE    line is empty
 *     CMD_INCLUDE      the line contains a #include directive
 *     CMD_DEFINE       the line contains a #subst directive
 *     CMD_IF           the line contains a #if/#else/#endif directive
 *     CMD_DIRECTIVE    the line contains some other compiler directive
 *
 *  Global variables: iflevel, ifstack (altered)
 *                    lptr      (altered)
 */
static int
command(void)
{
    int tok, ret;
    cell val;
    char* str;
    int index;
    cell code_index;

    while (*lptr <= ' ' && *lptr != '\0')
        lptr += 1;
    if (*lptr == '\0')
        return CMD_EMPTYLINE; /* empty line */
    if (*lptr != '#')
        return SKIPPING ? CMD_CONDFALSE : CMD_NONE; /* it is not a compiler directive */
    /* compiler directive found */
    indent_nowarn = TRUE; /* allow loose indentation" */
    lexclr(FALSE);        /* clear any "pushed" tokens */
    /* on a pending expression, force to return a silent ';' token and force to
     * re-read the line
     */
    if (!sc_needsemicolon && stgget(&index, &code_index)) {
        lptr = term_expr;
        return CMD_TERM;
    }
    tok = lex(&val, &str);
    ret = SKIPPING ? CMD_CONDFALSE
                   : CMD_DIRECTIVE; /* preset 'ret' to CMD_DIRECTIVE (most common case) */
    switch (tok) {
        case tpIF: /* conditional compilation */
            ret = CMD_IF;
            assert(iflevel >= 0);
            if (iflevel >= sCOMP_STACK)
                error(FATAL_ERROR_ALLOC_OVERFLOW, "Conditional compilation stack");
            iflevel++;
            if (SKIPPING)
                break; /* break out of switch */
            skiplevel = iflevel;
            preproc_expr(&val, NULL); /* get value (or 0 on error) */
            ifstack[iflevel - 1] = (char)(val ? PARSEMODE : SKIPMODE);
            check_empty(lptr);
            break;
        case tpELSE:
        case tpELSEIF:
            ret = CMD_IF;
            assert(iflevel >= 0);
            if (iflevel == 0) {
                error(26); /* no matching #if */
                errorset(sRESET, 0);
            } else {
                /* check for earlier #else */
                if ((ifstack[iflevel - 1] & HANDLED_ELSE) == HANDLED_ELSE) {
                    if (tok == tpELSEIF)
                        error(61); /* #elseif directive may not follow an #else */
                    else
                        error(60); /* multiple #else directives between #if ... #endif */
                    errorset(sRESET, 0);
                } else {
                    assert(iflevel > 0);
                    /* if there has been a "parse mode" on this level, set "skip mode",
                     * otherwise, clear "skip mode"
                     */
                    if ((ifstack[iflevel - 1] & PARSEMODE) == PARSEMODE) {
                        /* there has been a parse mode already on this level, so skip the rest */
                        ifstack[iflevel - 1] |= (char)SKIPMODE;
                        /* if we were already skipping this section, allow expressions with
                         * undefined symbols; otherwise check the expression to catch errors
                         */
                        if (tok == tpELSEIF) {
                            if (skiplevel == iflevel)
                                preproc_expr(&val, NULL); /* get, but ignore the expression */
                            else
                                lptr = (unsigned char*)strchr((char*)lptr, '\0');
                        }
                    } else {
                        /* previous conditions were all FALSE */
                        if (tok == tpELSEIF) {
                            /* if we were already skipping this section, allow expressions with
                             * undefined symbols; otherwise check the expression to catch errors
                             */
                            if (skiplevel == iflevel) {
                                preproc_expr(&val, NULL); /* get value (or 0 on error) */
                            } else {
                                lptr = (unsigned char*)strchr((char*)lptr, '\0');
                                val = 0;
                            }
                            ifstack[iflevel - 1] = (char)(val ? PARSEMODE : SKIPMODE);
                        } else {
                            /* a simple #else, clear skip mode */
                            ifstack[iflevel - 1] &= (char)~SKIPMODE;
                        }
                    }
                }
            }
            check_empty(lptr);
            break;
        case tpENDIF:
            ret = CMD_IF;
            if (iflevel == 0) {
                error(26); /* no matching "#if" */
                errorset(sRESET, 0);
            } else {
                iflevel--;
                if (iflevel < skiplevel)
                    skiplevel = iflevel;
            }
            check_empty(lptr);
            break;
        case tINCLUDE: /* #include directive */
        case tpTRYINCLUDE:
            ret = CMD_INCLUDE;
            if (!SKIPPING)
                doinclude(tok == tpTRYINCLUDE);
            break;
        case tpFILE:
            if (!SKIPPING) {
                char pathname[_MAX_PATH];
                lptr = getstring((unsigned char*)pathname, sizeof pathname, lptr);
                if (strlen(pathname) > 0) {
                    free(inpfname);
                    inpfname = strdup(pathname);
                    if (inpfname == NULL)
                        error(FATAL_ERROR_OOM);
                    fline = 0;
                }
            }
            check_empty(lptr);
            break;
        case tpLINE:
            if (!SKIPPING) {
                if (lex(&val, &str) != tNUMBER)
                    error(8); /* invalid/non-constant expression */
                fline = (int)val;
            }
            check_empty(lptr);
            break;
        case tpASSERT:
            if (!SKIPPING && (sc_debug & sCHKBOUNDS) != 0) {
                for (str = (char*)lptr; *str <= ' ' && *str != '\0'; str++)
                    /* nothing */;        /* save start of expression */
                preproc_expr(&val, NULL); /* get constant expression (or 0 on error) */
                if (!val)
                    error(FATAL_ERROR_ASSERTION_FAILED, str); /* assertion failed */
                check_empty(lptr);
            }
            break;
        case tpPRAGMA:
            if (!SKIPPING) {
                if (lex(&val, &str) == tSYMBOL) {
                    if (strcmp(str, "ctrlchar") == 0) {
                        while (*lptr <= ' ' && *lptr != '\0')
                            lptr++;
                        if (*lptr == '\0') {
                            sc_ctrlchar = sc_ctrlchar_org;
                        } else {
                            if (lex(&val, &str) != tNUMBER)
                                error(27); /* invalid character constant */
                            sc_ctrlchar = (char)val;
                        }
                    } else if (strcmp(str, "deprecated") == 0) {
                        while (*lptr <= ' ' && *lptr != '\0')
                            lptr++;
                        pc_deprecate = (char*)lptr;
                        lptr = (unsigned char*)strchr(
                            (char*)lptr,
                            '\0'); /* skip to end (ignore "extra characters on line") */
                    } else if (strcmp(str, "dynamic") == 0) {
                        preproc_expr(&pc_stksize, NULL);
                    } else if (strcmp(str, "rational") == 0) {
                        while (*lptr != '\0')
                            lptr++;
                    } else if (strcmp(str, "semicolon") == 0) {
                        cell val;
                        preproc_expr(&val, NULL);
                        sc_needsemicolon = (int)val;
                    } else if (strcmp(str, "newdecls") == 0) {
                        while (*lptr <= ' ' && *lptr != '\0')
                            lptr++;
                        if (strncmp((char*)lptr, "required", 8) == 0)
                            sc_require_newdecls = 1;
                        else if (strncmp((char*)lptr, "optional", 8) == 0)
                            sc_require_newdecls = 0;
                        else
                            error(146);
                        lptr = (unsigned char*)strchr(
                            (char*)lptr,
                            '\0'); /* skip to end (ignore "extra characters on line") */
                    } else if (strcmp(str, "tabsize") == 0) {
                        cell val;
                        preproc_expr(&val, NULL);
                        sc_tabsize = (int)val;
                    } else if (strcmp(str, "unused") == 0) {
                        char name[sNAMEMAX + 1];
                        size_t i;
                        int comma;
                        symbol* sym;
                        do {
                            /* get the name */
                            while (*lptr <= ' ' && *lptr != '\0')
                                lptr++;
                            for (i = 0; i < sizeof name && alphanum(*lptr); i++, lptr++)
                                name[i] = *lptr;
                            name[i] = '\0';
                            /* get the symbol */
                            sym = findloc(name);
                            if (sym == NULL)
                                sym = findglb(name);
                            if (sym != NULL) {
                                sym->usage |= uREAD;
                                if (sym->ident == iVARIABLE || sym->ident == iREFERENCE ||
                                    sym->ident == iARRAY || sym->ident == iREFARRAY)
                                    sym->usage |= uWRITTEN;
                            } else {
                                error(17, name); /* undefined symbol */
                            }
                            /* see if a comma follows the name */
                            while (*lptr <= ' ' && *lptr != '\0')
                                lptr++;
                            comma = (*lptr == ',');
                            if (comma)
                                lptr++;
                        } while (comma);
                    } else {
                        error(207); /* unknown #pragma */
                    }
                } else {
                    error(207); /* unknown #pragma */
                }
                check_empty(lptr);
            }
            break;
        case tpENDINPUT:
        case tpENDSCRPT:
            if (!SKIPPING) {
                check_empty(lptr);
                assert(inpf != NULL);
                if (inpf != inpf_org)
                    pc_closesrc(inpf);
                inpf = NULL;
            }
            break;
        case tpDEFINE: {
            ret = CMD_DEFINE;
            if (!SKIPPING) {
                char *pattern, *substitution;
                const unsigned char *start, *end;
                int count, prefixlen;
                /* find the pattern to match */
                while (*lptr <= ' ' && *lptr != '\0')
                    lptr++;
                start = lptr; /* save starting point of the match pattern */
                count = 0;
                while (*lptr > ' ' && *lptr != '\0') {
                    litchar(&lptr, 0); /* litchar() advances "lptr" and handles escape characters */
                    count++;
                }
                end = lptr;
                /* check pattern to match */
                if (!alpha(*start)) {
                    error(74); /* pattern must start with an alphabetic character */
                    break;
                }
                /* store matched pattern */
                pattern = (char*)malloc(count + 1);
                if (pattern == NULL)
                    error(FATAL_ERROR_OOM);
                lptr = start;
                count = 0;
                while (lptr != end) {
                    assert(lptr < end);
                    assert(*lptr != '\0');
                    pattern[count++] = (char)litchar(&lptr, 0);
                }
                pattern[count] = '\0';
                /* special case, erase trailing variable, because it could match anything */
                if (count >= 2 && isdigit(pattern[count - 1]) && pattern[count - 2] == '%')
                    pattern[count - 2] = '\0';
                /* find substitution string */
                while (*lptr <= ' ' && *lptr != '\0')
                    lptr++;
                start = lptr; /* save starting point of the match pattern */
                count = 0;
                end = NULL;
                while (*lptr != '\0') {
                    /* keep position of the start of trailing whitespace */
                    if (*lptr <= ' ') {
                        if (end == NULL)
                            end = lptr;
                    } else {
                        end = NULL;
                    }
                    count++;
                    lptr++;
                }
                if (end == NULL)
                    end = lptr;
                /* store matched substitution */
                substitution = (char*)malloc(count + 1); /* +1 for '\0' */
                if (substitution == NULL)
                    error(FATAL_ERROR_OOM);
                lptr = start;
                count = 0;
                while (lptr != end) {
                    assert(lptr < end);
                    assert(*lptr != '\0');
                    substitution[count++] = *lptr++;
                }
                substitution[count] = '\0';
                /* check whether the definition already exists */
                for (prefixlen = 0, start = (unsigned char*)pattern; alphanum(*start);
                     prefixlen++, start++)
                    /* nothing */;
                assert(prefixlen > 0);

                macro_t def;
                if (find_subst(pattern, prefixlen, &def)) {
                    if (strcmp(def.first, pattern) != 0 || strcmp(def.second, substitution) != 0)
                        error(201, pattern); /* redefinition of macro (non-identical) */
                    delete_subst(pattern, prefixlen);
                }
                /* add the pattern/substitution pair to the list */
                assert(strlen(pattern) > 0);
                insert_subst(pattern, prefixlen, substitution);
                free(pattern);
                free(substitution);
            }
            break;
        } /* case */
        case tpUNDEF:
            if (!SKIPPING) {
                if (lex(&val, &str) == tSYMBOL) {
                    if (delete_subst(str, strlen(str))) {
                        /* also undefine normal constants */
                        symbol* sym = findconst(str);
                        if (sym != NULL && !(sym->enumroot && sym->enumfield)) {
                            delete_symbol(&glbtab, sym);
                        }
                    }
                    if (!ret)
                        error(17, str); /* undefined symbol */
                } else {
                    error(20, str); /* invalid symbol name */
                }
                check_empty(lptr);
            }
            break;
        case tpERROR:
            while (*lptr <= ' ' && *lptr != '\0')
                lptr++;
            if (!SKIPPING)
                error(FATAL_ERROR_USER_ERROR, lptr); /* user error */
            break;
        case tpWARNING:
            while (*lptr <= ' ' && *lptr != '\0')
                lptr++;
            if (!SKIPPING)
                error(224, lptr); /* user warning */
            break;
        default:
            error(31);                                 /* unknown compiler directive */
            ret = SKIPPING ? CMD_CONDFALSE : CMD_NONE; /* process as normal line */
    }
    return ret;
}

static int
is_startstring(const unsigned char* string)
{
    if (*string == '\"' || *string == '\'')
        return TRUE; /* "..." */
    return FALSE;
}

static const unsigned char*
skipstring(const unsigned char* string)
{
    char endquote = *string;
    assert(endquote == '"' || endquote == '\'');
    string++; /* skip open quote */
    while (*string != endquote && *string != '\0')
        litchar(&string, 0);
    return string;
}

static const unsigned char*
skippgroup(const unsigned char* string)
{
    int nest = 0;
    char open = *string;
    char close;

    switch (open) {
        case '(':
            close = ')';
            break;
        case '{':
            close = '}';
            break;
        case '[':
            close = ']';
            break;
        case '<':
            close = '>';
            break;
        default:
            assert(0);
            close = '\0'; /* only to avoid a compiler warning */
    }

    string++;
    while (*string != close || nest > 0) {
        if (*string == open)
            nest++;
        else if (*string == close)
            nest--;
        else if (is_startstring(string))
            string = skipstring(string);
        if (*string == '\0')
            break;
        string++;
    }
    return string;
}

static char*
strdel(char* str, size_t len)
{
    size_t length = strlen(str);
    if (len > length)
        len = length;
    memmove(str, str + len, length - len + 1); /* include EOS byte */
    return str;
}

static char*
strins(char* dest, const char* src, size_t srclen)
{
    size_t destlen = strlen(dest);
    assert(srclen <= strlen(src));
    memmove(dest + srclen, dest, destlen + 1); /* include EOS byte */
    memcpy(dest, src, srclen);
    return dest;
}

static int
substpattern(unsigned char* line, size_t buffersize, const char* pattern,
             const char* substitution)
{
    int prefixlen;
    const unsigned char *p, *s, *e;
    unsigned char* args[10];
    int match, arg, len, argsnum = 0;
    int stringize;

    memset(args, 0, sizeof args);

    /* check the length of the prefix */
    for (prefixlen = 0, s = (unsigned char*)pattern; alphanum(*s); prefixlen++, s++)
        /* nothing */;
    assert(prefixlen > 0);
    assert(strncmp((char*)line, pattern, prefixlen) == 0);

    /* pattern prefix matches; match the rest of the pattern, gather
     * the parameters
     */
    s = line + prefixlen;
    p = (unsigned char*)pattern + prefixlen;
    match = TRUE; /* so far, pattern matches */
    while (match && *s != '\0' && *p != '\0') {
        if (*p == '%') {
            p++; /* skip '%' */
            if (isdigit(*p)) {
                arg = *p - '0';
                assert(arg >= 0 && arg <= 9);
                p++; /* skip parameter id */
                assert(*p != '\0');
                /* match the source string up to the character after the digit
                 * (skipping strings in the process
                 */
                e = s;
                while (*e != *p && *e != '\0' && *e != '\n') {
                    if (is_startstring(e)) /* skip strings */
                        e = skipstring(e);
                    else if (strchr("({[", *e) != NULL) /* skip parenthized groups */
                        e = skippgroup(e);
                    if (*e != '\0')
                        e++; /* skip non-alphapetic character (or closing quote of
                              * a string, or the closing paranthese of a group) */
                }
                /* store the parameter (overrule any earlier) */
                if (args[arg] != NULL)
                    free(args[arg]);
                else
                    argsnum++;
                len = (int)(e - s);
                args[arg] = (unsigned char*)malloc(len + 1);
                if (args[arg] == NULL)
                    error(FATAL_ERROR_OOM);
                SafeStrcpy((char*)args[arg], len + 1, (char*)s);
                /* character behind the pattern was matched too */
                if (*e == *p) {
                    s = e + 1;
                } else if (*e == '\n' && *p == ';' && *(p + 1) == '\0' && !sc_needsemicolon) {
                    s = e; /* allow a trailing ; in the pattern match to end of line */
                } else {
                    assert(*e == '\0' || *e == '\n');
                    match = FALSE;
                    s = e;
                }
                p++;
            } else {
                match = FALSE;
            }
        } else if (*p == ';' && *(p + 1) == '\0' && !sc_needsemicolon) {
            /* source may be ';' or end of the line */
            while (*s <= ' ' && *s != '\0')
                s++; /* skip white space */
            if (*s != ';' && *s != '\0')
                match = FALSE;
            p++; /* skip the semicolon in the pattern */
        } else {
            cell ch;
            /* skip whitespace between two non-alphanumeric characters, except
             * for two identical symbols
             */
            assert((char*)p > pattern);
            if (!alphanum(*p) && *(p - 1) != *p)
                while (*s <= ' ' && *s != '\0')
                    s++;         /* skip white space */
            ch = litchar(&p, 0); /* this increments "p" */
            if (*s != ch)
                match = FALSE;
            else
                s++; /* this character matches */
        }
    }

    if (match && *p == '\0') {
        /* if the last character to match is an alphanumeric character, the
         * current character in the source may not be alphanumeric
         */
        assert(p > (unsigned char*)pattern);
        if (alphanum(*(p - 1)) && alphanum(*s))
            match = FALSE;
    }

    if (match) {
        /* calculate the length of the substituted string */
        for (e = (unsigned char*)substitution, len = 0; *e != '\0'; e++) {
            if (*e == '#' && *(e + 1) == '%' && isdigit(*(e + 2)) && argsnum) {
                stringize = 1;
                e++; /* skip '#' */
            } else {
                stringize = 0;
            }
            if (*e == '%' && isdigit(*(e + 1)) && argsnum) {
                arg = *(e + 1) - '0';
                assert(arg >= 0 && arg <= 9);
                assert(stringize == 0 || stringize == 1);
                if (args[arg] != NULL) {
                    len += strlen((char*)args[arg]) + 2 * stringize;
                    e++;
                } else {
                    len++;
                }
            } else {
                len++;
            }
        }
        /* check length of the string after substitution */
        if (strlen((char*)line) + len - (int)(s - line) > buffersize) {
            error(75); /* line too long */
        } else {
            /* substitute pattern */
            strdel((char*)line, (int)(s - line));
            for (e = (unsigned char*)substitution, s = line; *e != '\0'; e++) {
                if (*e == '#' && *(e + 1) == '%' && isdigit(*(e + 2))) {
                    stringize = 1;
                    e++; /* skip '#' */
                } else {
                    stringize = 0;
                }
                if (*e == '%' && isdigit(*(e + 1))) {
                    arg = *(e + 1) - '0';
                    assert(arg >= 0 && arg <= 9);
                    if (args[arg] != NULL) {
                        if (stringize)
                            strins((char*)s++, "\"", 1);
                        strins((char*)s, (char*)args[arg], strlen((char*)args[arg]));
                        s += strlen((char*)args[arg]);
                        if (stringize)
                            strins((char*)s++, "\"", 1);
                    } else {
                        error(236); /* parameter does not exist, incorrect #define pattern */
                        strins((char*)s, (char*)e, 2);
                        s += 2;
                    }
                    e++; /* skip %, digit is skipped later */
                } else if (*e == '"') {
                    p = e;
                    if (is_startstring(e)) {
                        e = skipstring(e);
                        strins((char*)s, (char*)p, (e - p + 1));
                        s += (e - p + 1);
                    } else {
                        strins((char*)s, (char*)e, 1);
                        s++;
                    }
                } else {
                    strins((char*)s, (char*)e, 1);
                    s++;
                }
            }
        }
    }

    for (arg = 0; arg < 10; arg++)
        if (args[arg] != NULL)
            free(args[arg]);

    return match;
}

static void
substallpatterns(unsigned char* line, int buffersize)
{
    unsigned char *start, *end;
    int prefixlen;

    start = line;
    while (*start != '\0') {
        /* find the start of a prefix (skip all non-alphabetic characters),
         * also skip strings
         */
        while (!alpha(*start) && *start != '\0') {
            /* skip strings */
            if (is_startstring(start)) {
                start = (unsigned char*)skipstring(start);
                if (*start == '\0')
                    break; /* abort loop on error */
            }
            start++; /* skip non-alphapetic character (or closing quote of a string) */
        }
        if (*start == '\0')
            break; /* abort loop on error */
        /* if matching the operator "defined", skip it plus the symbol behind it */
        if (strncmp((char*)start, "defined", 7) == 0 && !isalpha((char)*(start + 7))) {
            start += 7; /* skip "defined" */
            /* skip white space & parantheses */
            while ((*start <= ' ' && *start != '\0') || *start == '(')
                start++;
            /* skip the symbol behind it */
            while (alphanum(*start))
                start++;
            /* drop back into the main loop */
            continue;
        }
        /* get the prefix (length), look for a matching definition */
        prefixlen = 0;
        end = start;
        while (alphanum(*end)) {
            prefixlen++;
            end++;
        }
        assert(prefixlen > 0);

        macro_t subst;
        if (find_subst((const char*)start, prefixlen, &subst)) {
            /* properly match the pattern and substitute */
            if (!substpattern(start, buffersize - (int)(start - line), subst.first, subst.second))
                start = end; /* match failed, skip this prefix */
            /* match succeeded: do not update "start", because the substitution text
             * may be matched by other macros
             */
        } else {
            start = end; /* no macro with this prefix, skip this prefix */
        }
    }
}

/*  scanellipsis
 *  Look for ... in the string and (if not there) in the remainder of the file,
 *  but restore (or keep intact):
 *  - the current position in the file
 *  - the comment parsing state
 *  - the line buffer used by the lexical analyser
 *  - the active line number and the active file
 *
 *  The function returns 1 if an ellipsis was found and 0 if not
 */
static int
scanellipsis(const unsigned char* lptr)
{
    static void* inpfmark = NULL;
    unsigned char* localbuf;
    short localcomment, found;

    /* first look for the ellipsis in the remainder of the string */
    while (*lptr <= ' ' && *lptr != '\0')
        lptr++;
    if (lptr[0] == '.' && lptr[1] == '.' && lptr[2] == '.')
        return 1;
    if (*lptr != '\0')
        return 0; /* stumbled on something that is not an ellipsis and not white-space */

    /* the ellipsis was not on the active line, read more lines from the current
     * file (but save its position first)
     */
    if (inpf == NULL || pc_eofsrc(inpf))
        return 0; /* quick exit: cannot read after EOF */
    if ((localbuf = (unsigned char*)malloc((sLINEMAX + 1) * sizeof(unsigned char))) == NULL)
        return 0;
    inpfmark = pc_getpossrc(inpf);
    localcomment = icomment;

    found = 0;
    /* read from the file, skip preprocessing, but strip off comments */
    while (!found && pc_readsrc(inpf, localbuf, sLINEMAX) != NULL) {
        stripcom(localbuf);
        lptr = localbuf;
        /* skip white space */
        while (*lptr <= ' ' && *lptr != '\0')
            lptr++;
        if (lptr[0] == '.' && lptr[1] == '.' && lptr[2] == '.')
            found = 1;
        else if (*lptr != '\0')
            break; /* stumbled on something that is not an ellipsis and not white-space */
    }

    /* clean up & reset */
    free(localbuf);
    pc_resetsrc(inpf, inpfmark);
    icomment = localcomment;
    return found;
}

/*  preprocess
 *
 *  Reads a line by readline() into "pline" and performs basic preprocessing:
 *  deleting comments, skipping lines with false "#if.." code and recognizing
 *  other compiler directives. There is an indirect recursion: lex() calls
 *  preprocess() if a new line must be read, preprocess() calls command(),
 *  which at his turn calls lex() to identify the token.
 *
 *  Global references: lptr     (altered)
 *                     pline    (altered)
 *                     freading (referred to only)
 */
void
preprocess(void)
{
    int iscommand;

    if (!freading)
        return;
    do {
        readline(pline);
        stripcom(
            pline); /* ??? no need for this when reading back from list file (in the second pass) */
        lptr = pline; /* set "line pointer" to start of the parsing buffer */
        iscommand = command();
        if (iscommand != CMD_NONE)
            errorset(sRESET, 0); /* reset error flag ("panic mode") on empty line or directive */
        if (iscommand == CMD_NONE) {
            assert(lptr != term_expr);
            substallpatterns(pline, sLINEMAX);
            lptr = pline; /* reset "line pointer" to start of the parsing buffer */
        }
        if (sc_status == statFIRST && sc_listing && freading &&
            (iscommand == CMD_NONE || iscommand == CMD_EMPTYLINE || iscommand == CMD_DIRECTIVE))
        {
            listline++;
            if (fline != listline) {
                listline = fline;
                setlinedirect(fline);
            }
            if (iscommand == CMD_EMPTYLINE)
                pc_writeasm(outf, "\n");
            else
                pc_writeasm(outf, (char*)pline);
        }
    } while (iscommand != CMD_NONE && iscommand != CMD_TERM && freading); /* enddo */
}

static void
packedstring(const unsigned char* lptr, int flags, full_token_t* tok)
{
    while (*lptr != '\0') {
        if (*lptr == '\a') { // ignore '\a' (which was inserted at a line concatenation)
            lptr++;
            continue;
        }
        ucell c = litchar(&lptr, flags); // litchar() alters "lptr"
        if (c >= (ucell)(1 << sCHARBITS))
            error(43); // character constant exceeds range
        glbstringread++;
        if (tok->len >= sizeof(tok->str) - 1) {
            error(75); // line too long
            continue;
        }
        tok->str[tok->len++] = (char)c;
    }
    assert(tok->len < sizeof(tok->str));
    tok->str[tok->len] = '\0';
}

/*  lex(lexvalue,lexsym)        Lexical Analysis
 *
 *  lex() first deletes leading white space, then checks for multi-character
 *  operators, keywords (including most compiler directives), numbers,
 *  labels, symbols and literals (literal characters are converted to a number
 *  and are returned as such). If every check fails, the line must contain
 *  a single-character operator. So, lex() returns this character. In the other
 *  case (something did match), lex() returns the number of the token. All
 *  these tokens have been assigned numbers above 255.
 *
 *  Some tokens have "attributes":
 *     tNUMBER        the value of the number is return in "lexvalue".
 *     tRATIONAL      the value is in IEEE 754 encoding or in fixed point
 *                    encoding in "lexvalue".
 *     tSYMBOL        the first sNAMEMAX characters of the symbol are
 *                    stored in a buffer, a pointer to this buffer is
 *                    returned in "lexsym".
 *     tLABEL         the first sNAMEMAX characters of the label are
 *                    stored in a buffer, a pointer to this buffer is
 *                    returned in "lexsym".
 *     tSTRING        the string is stored in the literal pool, the index
 *                    in the literal pool to this string is stored in
 *                    "lexvalue".
 *
 *  lex() stores all information (the token found and possibly its attribute)
 *  in global variables. This allows a token to be examined twice. If "_pushed"
 *  is true, this information is returned.
 *
 *  Global references: lptr          (altered)
 *                     fline         (referred to only)
 *                     litidx        (referred to only)
 *                     _pushed
 */

static int _lexnewline;

// lex() is called recursively, which messes up the lookahead buffer. To get
// around this we use two separate token buffers.
token_buffer_t sNormalBuffer;
token_buffer_t sPreprocessBuffer;
token_buffer_t* sTokenBuffer;

full_token_t*
current_token()
{
    return &sTokenBuffer->tokens[sTokenBuffer->cursor];
}

static full_token_t*
next_token()
{
    assert(sTokenBuffer->depth > 0);
    int cursor = sTokenBuffer->cursor + 1;
    if (cursor == MAX_TOKEN_DEPTH)
        cursor = 0;
    return &sTokenBuffer->tokens[cursor];
}

const char* sc_tokens[] = {"*=",
                           "/=",
                           "%=",
                           "+=",
                           "-=",
                           "<<=",
                           ">>>=",
                           ">>=",
                           "&=",
                           "^=",
                           "|=",
                           "||",
                           "&&",
                           "==",
                           "!=",
                           "<=",
                           ">=",
                           "<<",
                           ">>>",
                           ">>",
                           "++",
                           "--",
                           "...",
                           "..",
                           "::",
                           "acquire",
                           "as",
                           "assert",
                           "break",
                           "builtin",
                           "catch",
                           "case",
                           "cast_to",
                           "char",
                           "const",
                           "continue",
                           "decl",
                           "default",
                           "defined",
                           "delete",
                           "do",
                           "double",
                           "else",
                           "enum",
                           "exit",
                           "explicit",
                           "finally",
                           "for",
                           "foreach",
                           "forward",
                           "funcenum",
                           "functag",
                           "function",
                           "goto",
                           "if",
                           "implicit",
                           "import",
                           "in",
                           "int",
                           "int8",
                           "int16",
                           "int32",
                           "int64",
                           "interface",
                           "intn",
                           "let",
                           "methodmap",
                           "namespace",
                           "native",
                           "new",
                           "null",
                           "__nullable__",
                           "object",
                           "operator",
                           "package",
                           "private",
                           "protected",
                           "public",
                           "readonly",
                           "return",
                           "sealed",
                           "sizeof",
                           "static",
                           "stock",
                           "struct",
                           "switch",
                           "this",
                           "throw",
                           "try",
                           "typedef",
                           "typeof",
                           "typeset",
                           "uint8",
                           "uint16",
                           "uint32",
                           "uint64",
                           "uintn",
                           "union",
                           "using",
                           "var",
                           "variant",
                           "view_as",
                           "virtual",
                           "void",
                           "volatile",
                           "while",
                           "with",
                           "#assert",
                           "#define",
                           "#else",
                           "#elseif",
                           "#endif",
                           "#endinput",
                           "#endscript",
                           "#error",
                           "#warning",
                           "#file",
                           "#if",
                           "#include",
                           "#line",
                           "#pragma",
                           "#tryinclude",
                           "#undef",
                           ";",
                           ";",
                           "-integer value-",
                           "-rational value-",
                           "-identifier-",
                           "-label-",
                           "-string-",
                           "-string-"};

void
lexinit()
{
    iflevel = 0;   /* preprocessor: nesting of "#if" is currently 0 */
    skiplevel = 0; /* preprocessor: not currently skipping */
    icomment = 0;  /* currently not in a multiline comment */
    _lexnewline = FALSE;
    memset(&sNormalBuffer, 0, sizeof(sNormalBuffer));
    memset(&sPreprocessBuffer, 0, sizeof(sPreprocessBuffer));
    sTokenBuffer = &sNormalBuffer;

    if (!sKeywords.elements()) {
        sKeywords.init(128);

        const int kStart = tMIDDLE + 1;
        const char** tokptr = &sc_tokens[kStart - tFIRST];
        for (int i = kStart; i <= tLAST; i++, tokptr++) {
            CharsAndLength key(*tokptr, strlen(*tokptr));
            auto p = sKeywords.findForAdd(key);
            assert(!p.found());
            sKeywords.add(p, key, i);
        }
    }
}

ke::AString
get_token_string(int tok_id)
{
    ke::AString str;
    if (tok_id < 256) {
        str.format("%c", tok_id);
    } else if (tok_id == tEOL) {
        str.format("<newline>");
    } else {
        assert(tok_id >= tFIRST && tok_id <= tLAST);
        str.format("%s", sc_tokens[tok_id - tFIRST]);
    }
    return str;
}

static int
lex_keyword_impl(const char* match, size_t length)
{
    CharsAndLength key(match, length);
    auto p = sKeywords.find(key);
    if (!p.found())
        return 0;
    return p->value;
}

static inline bool
IsUnimplementedKeyword(int token)
{
    switch (token) {
        case tACQUIRE:
        case tAS:
        case tCATCH:
        case tCAST_TO:
        case tDOUBLE:
        case tEXPLICIT:
        case tFINALLY:
        case tFOREACH:
        case tIMPLICIT:
        case tIMPORT:
        case tIN:
        case tINT8:
        case tINT16:
        case tINT32:
        case tINT64:
        case tINTERFACE:
        case tINTN:
        case tLET:
        case tNAMESPACE:
        case tPACKAGE:
        case tPRIVATE:
        case tPROTECTED:
        case tREADONLY:
        case tSEALED:
        case tTHROW:
        case tTRY:
        case tTYPEOF:
        case tUINT8:
        case tUINT16:
        case tUINT32:
        case tUINT64:
        case tUINTN:
        case tUNION:
        case tVAR:
        case tVARIANT:
        case tVIRTUAL:
        case tVOLATILE:
        case tWITH:
            return true;
        default:
            return false;
    }
}

static full_token_t*
advance_token_ptr()
{
    assert(sTokenBuffer->depth == 0);
    sTokenBuffer->num_tokens++;
    sTokenBuffer->cursor++;
    if (sTokenBuffer->cursor == MAX_TOKEN_DEPTH)
        sTokenBuffer->cursor = 0;

    return current_token();
}

static void
preprocess_in_lex()
{
    sTokenBuffer = &sPreprocessBuffer;
    preprocess();
    sTokenBuffer = &sNormalBuffer;
}

// Pops a token off the token buffer, making it the current token.
static void
lexpop()
{
    assert(sTokenBuffer->depth > 0);

    sTokenBuffer->depth--;
    sTokenBuffer->cursor++;
    if (sTokenBuffer->cursor == MAX_TOKEN_DEPTH)
        sTokenBuffer->cursor = 0;
}

static void lex_once(full_token_t* tok, cell* lexvalue);
static bool lex_match_char(char c);
static void lex_string_literal(full_token_t* tok, cell* lexvalue);
static bool lex_number(full_token_t* tok, cell* lexvalue);
static bool lex_keyword(full_token_t* tok, const char* token_start);
static void lex_symbol(full_token_t* tok, const char* token_start);
static bool lex_symbol_or_keyword(full_token_t* tok);

int
lex(cell* lexvalue, char** lexsym)
{
    int newline;

    if (sTokenBuffer->depth > 0) {
        lexpop();
        *lexvalue = current_token()->value;
        *lexsym = current_token()->str;
        return current_token()->id;
    }

    full_token_t* tok = advance_token_ptr();
    tok->id = 0;
    tok->value = 0;
    tok->str[0] = '\0';
    tok->len = 0;

    *lexvalue = tok->value;
    *lexsym = tok->str;

    _lexnewline = FALSE;
    if (!freading)
        return 0;

    newline = (lptr == pline); /* does lptr point to start of line buffer */
    while (*lptr <= ' ') {     /* delete leading white space */
        if (*lptr == '\0') {
            preprocess_in_lex();
            if (!freading)
                return 0;
            if (lptr == term_expr) /* special sequence to terminate a pending expression */
                return (tok->id = tENDEXPR);
            _lexnewline = TRUE; /* set this after preprocess(), because
                                 * preprocess() calls lex() recursively */
            newline = TRUE;
        } else {
            lptr += 1;
        }
    }
    if (newline) {
        stmtindent = 0;
        for (int i = 0; i < (int)(lptr - pline); i++)
            if (pline[i] == '\t' && sc_tabsize > 0)
                stmtindent += (int)(sc_tabsize - (stmtindent + sc_tabsize) % sc_tabsize);
            else
                stmtindent++;
    }

    tok->start.line = fline;
    tok->start.col = (int)(lptr - pline);
    tok->start.file = fcurrent;

    lex_once(tok, lexvalue);

    tok->end.line = fline;
    tok->end.col = (int)(lptr - pline);
    tok->end.file = tok->start.file;
    return tok->id;
}

static void
lex_once(full_token_t* tok, cell* lexvalue)
{
    switch (*lptr) {
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9': {
            if (lex_number(tok, lexvalue))
                return;
            break;
        }

        case '*':
            lptr++;
            if (lex_match_char('='))
                tok->id = taMULT;
            else
                tok->id = '*';
            return;

        case '/':
            lptr++;
            if (lex_match_char('='))
                tok->id = taDIV;
            else
                tok->id = '/';
            return;

        case '%':
            lptr++;
            if (lex_match_char('='))
                tok->id = taMOD;
            else
                tok->id = '%';
            return;

        case '+':
            lptr++;
            if (lex_match_char('='))
                tok->id = taADD;
            else if (lex_match_char('+'))
                tok->id = tINC;
            else
                tok->id = '+';
            return;

        case '-':
            lptr++;
            if (lex_match_char('='))
                tok->id = taSUB;
            else if (lex_match_char('-'))
                tok->id = tDEC;
            else
                tok->id = '-';
            return;

        case '<':
            lptr++;
            if (lex_match_char('<')) {
                if (lex_match_char('='))
                    tok->id = taSHL;
                else
                    tok->id = tSHL;
            } else if (lex_match_char('=')) {
                tok->id = tlLE;
            } else {
                tok->id = '<';
            }
            return;

        case '>':
            lptr++;
            if (lex_match_char('>')) {
                if (lex_match_char('>')) {
                    if (lex_match_char('='))
                        tok->id = taSHRU;
                    else
                        tok->id = tSHRU;
                } else if (lex_match_char('=')) {
                    tok->id = taSHR;
                } else {
                    tok->id = tSHR;
                }
            } else if (lex_match_char('=')) {
                tok->id = tlGE;
            } else {
                tok->id = '>';
            }
            return;

        case '&':
            lptr++;
            if (lex_match_char('='))
                tok->id = taAND;
            else if (lex_match_char('&'))
                tok->id = tlAND;
            else
                tok->id = '&';
            return;

        case '^':
            lptr++;
            if (lex_match_char('='))
                tok->id = taXOR;
            else
                tok->id = '^';
            return;

        case '|':
            lptr++;
            if (lex_match_char('='))
                tok->id = taOR;
            else if (lex_match_char('|'))
                tok->id = tlOR;
            else
                tok->id = '|';
            return;

        case '=':
            lptr++;
            if (lex_match_char('='))
                tok->id = tlEQ;
            else
                tok->id = '=';
            return;

        case '!':
            lptr++;
            if (lex_match_char('='))
                tok->id = tlNE;
            else
                tok->id = '!';
            return;

        case '.':
            lptr++;
            if (lex_match_char('.')) {
                if (lex_match_char('.'))
                    tok->id = tELLIPS;
                else
                    tok->id = tDBLDOT;
            } else {
                tok->id = '.';
            }
            return;

        case ':':
            lptr++;
            if (lex_match_char(':'))
                tok->id = tDBLCOLON;
            else
                tok->id = ':';
            return;

        case '"':
            lex_string_literal(tok, lexvalue);
            return;

        case '\'':
            lptr += 1; /* skip quote */
            tok->id = tNUMBER;
            *lexvalue = tok->value = litchar(&lptr, UTF8MODE);
            if (*lptr == '\'')
                lptr += 1; /* skip final quote */
            else
                error(27); /* invalid character constant (must be one character) */
            return;

        case ';':
            // semicolon resets the error state.
            tok->id = ';';
            lptr++;
            errorset(sRESET, 0);
            return;
    }

    if (alpha(*lptr) || *lptr == '#') {
        if (lex_symbol_or_keyword(tok))
            return;
    }

    // Unmatched, return the next character.
    tok->id = *lptr++;
}

static bool
lex_match_char(char c)
{
    if (*lptr != c)
        return false;
    lptr++;
    return true;
}

static bool
lex_number(full_token_t* tok, cell* lexvalue)
{
    if (int i = number(&tok->value, lptr)) {
        tok->id = tNUMBER;
        *lexvalue = tok->value;
        lptr += i;
        return true;
    }
    if (int i = ftoi(&tok->value, lptr)) {
        tok->id = tRATIONAL;
        *lexvalue = tok->value;
        lptr += i;
        return true;
    }
    return false;
}

static void
lex_string_literal(full_token_t* tok, cell* lexvalue)
{
    tok->id = tSTRING;
    tok->str[0] = '\0';
    tok->len = 0;
    tok->value = -1;  // Catch consumers expecting automatic litadd().

    glbstringread = 1;

    for (;;) {
        assert(*lptr == '\"');
        lptr += 1;

        static char buffer[sLINEMAX + 1];
        char* cat = buffer;
        while (*lptr != '\"' && *lptr != '\0' && (cat - buffer) < sLINEMAX) {
            if (*lptr != '\a') { /* ignore '\a' (which was inserted at a line concatenation) */
                *cat++ = *lptr;
                if (*lptr == sc_ctrlchar && *(lptr + 1) != '\0')
                    *cat++ = *++lptr; /* skip escape character plus the escaped character */
            }
            lptr++;
        }
        *cat = '\0'; /* terminate string */

        packedstring((unsigned char*)buffer, 0, tok);

        if (*lptr == '\"')
            lptr += 1; /* skip final quote */
        else
            error(37); /* invalid (non-terminated) string */
        /* see whether an ellipsis is following the string */
        if (!scanellipsis(lptr))
            break; /* no concatenation of string literals */
        /* there is an ellipses, go on parsing (this time with full preprocessing) */
        while (*lptr <= ' ') {
            if (*lptr == '\0') {
                preprocess_in_lex();
                assert(freading && lptr != term_expr);
            } else {
                lptr++;
            }
        }
        assert(freading && lptr[0] == '.' && lptr[1] == '.' && lptr[2] == '.');
        lptr += 3;
        while (*lptr <= ' ') {
            if (*lptr == '\0') {
                preprocess_in_lex();
                assert(freading && lptr != term_expr);
            } else {
                lptr++;
            }
        }
        if (!freading || !(*lptr == '\"')) {
            error(37); /* invalid string concatenation */
            break;
        }
    }
}

static bool
lex_keyword(full_token_t* tok, const char* token_start)
{
    int tok_id = lex_keyword_impl(token_start, tok->len);
    if (!tok_id)
        return false;

    if (IsUnimplementedKeyword(tok_id)) {
        // Try to gracefully error.
        error(173, get_token_string(tok_id).chars());
        tok->id = tSYMBOL;
        strcpy(tok->str, get_token_string(tok_id).chars());
        tok->len = strlen(tok->str);
    } else if (*lptr == ':' && (tok_id == tINT || tok_id == tVOID)) {
        // Special case 'int:' to its old behavior: an implicit view_as<> cast
        // with Pawn's awful lowercase coercion semantics.
        ke::AString token_str = get_token_string(tok_id);
        const char* token = token_str.chars();
        switch (tok_id) {
            case tINT:
                error(238, token, token);
                break;
            case tVOID:
                error(239, token, token);
                break;
        }
        lptr++;
        tok->id = tLABEL;
        strcpy(tok->str, token);
        tok->len = strlen(tok->str);
    } else {
        tok->id = tok_id;
        errorset(sRESET, 0); /* reset error flag (clear the "panic mode")*/
    }
    return true;
}

static bool
lex_symbol_or_keyword(full_token_t* tok)
{
    unsigned char const* token_start = lptr;
    char first_char = *lptr;
    assert(alpha(first_char) || first_char == '#');

    bool maybe_keyword = (first_char != PUBLIC_CHAR);
    while (true) {
        char c = *++lptr;
        if (isdigit(c)) {
            // Only symbols have numbers, so this terminates a keyword if we
            // started with '#".
            if (first_char == '#')
                break;
            maybe_keyword = false;
        } else if (!isalpha(c) && c != '_') {
            break;
        }
    }

    tok->len = lptr - token_start;
    if (tok->len == 1 && first_char == PUBLIC_CHAR) {
        tok->id = PUBLIC_CHAR;
        return true;
    }
    if (maybe_keyword) {
        if (lex_keyword(tok, (const char*)token_start))
            return true;
    }
    if (first_char != '#') {
        lex_symbol(tok, (const char*)token_start);
        return true;
    }

    // Failed to find anything, reset lptr.
    lptr = token_start;
    return false;
}

static void
lex_symbol(full_token_t* tok, const char* token_start)
{
    ke::SafeStrcpyN(tok->str, sizeof(tok->str), token_start, tok->len);
    if (tok->len > sNAMEMAX) {
        static_assert(sNAMEMAX < sizeof(tok->str), "sLINEMAX should be > sNAMEMAX");
        tok->str[sNAMEMAX] = '\0';
        tok->len = sNAMEMAX;
        error(200, tok->str, sNAMEMAX);
    }

    tok->id = tSYMBOL;

    if (*lptr == ':' && *(lptr + 1) != ':') {
        if (sc_allowtags) {
            tok->id = tLABEL;
            lptr++;
        } else if (gTypes.find(tok->str)) {
            // This looks like a tag override (a tag with this name exists), but
            // tags are not allowed right now, so it is probably an error.
            error(220);
        }
    } else if (tok->len == 1 && *token_start == '_') {
        // By itself, '_' is not a symbol but a placeholder. However, '_:' is
        // a label which is why we handle this after the label check.
        tok->id = '_';
    }
}

/*  lexpush
 *
 *  Pushes a token back, so the next call to lex() will return the token
 *  last examined, instead of a new token.
 *
 *  Only one token can be pushed back.
 *
 *  In fact, lex() already stores the information it finds into global
 *  variables, so all that is to be done is set a flag that informs lex()
 *  to read and return the information from these variables, rather than
 *  to read in a new token from the input file.
 */
void
lexpush(void)
{
    assert(sTokenBuffer->depth < MAX_TOKEN_DEPTH);
    sTokenBuffer->depth++;
    if (sTokenBuffer->cursor == 0)
        sTokenBuffer->cursor = MAX_TOKEN_DEPTH - 1;
    else
        sTokenBuffer->cursor--;
    assert(sTokenBuffer->depth <= sTokenBuffer->num_tokens);
}

/*  lexclr
 *
 *  Sets the variable "_pushed" to 0 to make sure lex() will read in a new
 *  symbol (a not continue with some old one). This is required upon return
 *  from Assembler mode, and in a few cases after detecting an syntax error.
 */
void
lexclr(int clreol)
{
    sTokenBuffer->depth = 0;
    if (clreol) {
        lptr = (unsigned char*)strchr((char*)pline, '\0');
        assert(lptr != NULL);
    }
}

// Return true if the symbol is ahead, false otherwise.
int
lexpeek(int id)
{
    if (matchtoken(id)) {
        lexpush();
        return TRUE;
    }
    return FALSE;
}

const token_pos_t& current_pos()
{
    return current_token()->start;
}

/*  matchtoken
 *
 *  This routine is useful if only a simple check is needed. If the token
 *  differs from the one expected, it is pushed back.
 *  This function returns 1 for "token found" and 2 for "implied statement
 *  termination token" found --the statement termination is an end of line in
 *  an expression where there is no pending operation. Such an implied token
 *  (i.e. not present in the source code) should not be pushed back, which is
 *  why it is sometimes important to distinguish the two.
 */
int
matchtoken(int token)
{
    cell val;
    char* str;
    int tok;

    tok = lex(&val, &str);

    if (token == tok)
        return 1;
    if (token == tTERM && (tok == ';' || tok == tENDEXPR))
        return 1;

    if (!sc_needsemicolon && token == tTERM && (_lexnewline || !freading)) {
        /* Push "tok" back, because it is the token following the implicit statement
         * termination (newline) token.
         */
        lexpush();
        return 2;
    }

    lexpush();
    return 0;
}

/*  tokeninfo
 *
 *  Returns additional information of a token after using "matchtoken()"
 *  or needtoken(). It does no harm using this routine after a call to
 *  "lex()", but lex() already returns the same information.
 *
 *  The token itself is the return value. Normally, this one is already known.
 */
int
tokeninfo(cell* val, char** str)
{
    *val = current_token()->value;
    *str = current_token()->str;
    return current_token()->id;
}

/*  needtoken
 *
 *  This routine checks for a required token and gives an error message if
 *  it isn't there (and returns 0/FALSE in that case). Like function matchtoken(),
 *  this function returns 1 for "token found" and 2 for "statement termination
 *  token" found; see function matchtoken() for details.
 */
int
needtoken(int token)
{
    char s1[20], s2[20];
    int t;

    if ((t = matchtoken(token)) != 0) {
        return t;
    } else {
        /* token already pushed back */
        assert(sTokenBuffer->depth > 0);
        if (token < 256)
            sprintf(s1, "%c", (char)token); /* single character token */
        else
            strcpy(s1, sc_tokens[token - tFIRST]); /* multi-character symbol */
        if (!freading)
            strcpy(s2, "-end of file-");
        else if (next_token()->id < 256)
            sprintf(s2, "%c", (char)next_token()->id);
        else
            strcpy(s2, sc_tokens[next_token()->id - tFIRST]);
        error(1, s1, s2); /* expected ..., but found ... */
        return FALSE;
    }
}

// If the next token is on the current line, return that token. Otherwise,
// return tNEWLINE.
int
peek_same_line()
{
    // We should not call this without having parsed at least one token.
    assert(sTokenBuffer->num_tokens > 0);

    // If there's tokens pushed back, then |fline| is the line of the furthest
    // token parsed. If fline == current token's line, we are guaranteed any
    // buffered token is still on the same line.
    if (sTokenBuffer->depth > 0 && current_token()->end.line == fline)
        return next_token()->id;

    // Make sure the next token is lexed by lexing, and then buffering it.
    full_token_t* next;
    {
        token_t tmp;
        lextok(&tmp);
        next = current_token();
        lexpush();
    }

    // If the next token starts on the line the last token ends, then the next
    // token is considered on the same line.
    if (next->start.line == current_token()->end.line)
        return next->id;

    return tEOL;
}

int
require_newline(TerminatorPolicy policy)
{
    if (policy != TerminatorPolicy::Newline) {
        // Semicolon must be on the same line.
        auto pos = current_token()->start;
        int next_tok_id = peek_same_line();
        if (next_tok_id == ';') {
            lexpop();
        } else if (policy == TerminatorPolicy::Semicolon && sc_needsemicolon) {
            error(pos, 1, ";", get_token_string(next_tok_id).chars());
        }
    }

    int tokid = peek_same_line();
    if (tokid == tEOL || tokid == 0)
        return TRUE;

    char s[20];
    if (tokid < 256)
        sprintf(s, "%c", (char)tokid);
    else
        strcpy(s, sc_tokens[tokid - tFIRST]);
    error(155, s);
    return FALSE;
}

void
litadd(const char* str, size_t len)
{
    ucell val = 0;
    int byte = 0;
    for (size_t i = 0; i < len; i++) {
        val |= str[i] << (8 * byte);
        if (byte == sizeof(ucell) - 1) {
            litadd(val);
            val = 0;
            byte = 0;
        } else {
            byte++;
        }
    }
    if (byte != 0) {
        // There are zeroes to terminate |val|.
        litadd(val);
    } else {
        // Add a full cell of zeroes.
        litadd(0);
    }
}

/*  litchar
 *
 *  Return current literal character and increase the pointer to point
 *  just behind this literal character.
 *
 *  Note: standard "escape sequences" are suported, but the backslash may be
 *        replaced by another character; the syntax '\ddd' is supported,
 *        but ddd must be decimal!
 */
static cell
litchar(const unsigned char** lptr, int flags)
{
    cell c = 0;
    const unsigned char* cptr;

    cptr = *lptr;
    if (*cptr != sc_ctrlchar) { /* no escape character */
        if ((flags & UTF8MODE) != 0) {
            c = get_utf8_char(cptr, &cptr);
            assert(c >= 0); /* file was already scanned for conformance to UTF-8 */
        } else {
            c = *cptr;
            cptr += 1;
        }
    } else {
        cptr += 1;
        if (*cptr == sc_ctrlchar) {
            c = *cptr; /* \\ == \ (the escape character itself) */
            cptr += 1;
        } else {
            switch (*cptr) {
                case 'a': /* \a == audible alarm */
                    c = 7;
                    cptr += 1;
                    break;
                case 'b': /* \b == backspace */
                    c = 8;
                    cptr += 1;
                    break;
                case 'e': /* \e == escape */
                    c = 27;
                    cptr += 1;
                    break;
                case 'f': /* \f == form feed */
                    c = 12;
                    cptr += 1;
                    break;
                case 'n': /* \n == NewLine character */
                    c = 10;
                    cptr += 1;
                    break;
                case 'r': /* \r == carriage return */
                    c = 13;
                    cptr += 1;
                    break;
                case 't': /* \t == horizontal TAB */
                    c = 9;
                    cptr += 1;
                    break;
                case 'v': /* \v == vertical TAB */
                    c = 11;
                    cptr += 1;
                    break;
                case 'x': {
                    int digits = 0;
                    cptr += 1;
                    c = 0;
                    while (ishex(*cptr) && digits < 2) {
                        if (isdigit(*cptr))
                            c = (c << 4) + (*cptr - '0');
                        else
                            c = (c << 4) + (tolower(*cptr) - 'a' + 10);
                        cptr++;
                        digits++;
                    }
                    if (*cptr == ';')
                        cptr++; /* swallow a trailing ';' */
                    break;
                }
                case '\'': /* \' == ' (single quote) */
                case '"':  /* \" == " (single quote) */
                case '%':  /* \% == % (percent) */
                    c = *cptr;
                    cptr += 1;
                    break;
                default:
                    if (isdigit(*cptr)) { /* \ddd */
                        c = 0;
                        while (*cptr >= '0' && *cptr <= '9') /* decimal! */
                            c = c * 10 + *cptr++ - '0';
                        if (*cptr == ';')
                            cptr++; /* swallow a trailing ';' */
                    } else {
                        error(27); /* invalid character constant */
                    }
            }
        }
    }
    *lptr = cptr;
    assert(c >= 0);
    return c;
}

/*  alpha
 *
 *  Test if character "c" is alphabetic ("a".."z"), an underscore ("_")
 *  or an "at" sign ("@"). The "@" is an extension to standard C.
 */
static int
alpha(char c)
{
    return (isalpha(c) || c == '_' || c == PUBLIC_CHAR);
}

/*  alphanum
 *
 *  Test if character "c" is alphanumeric ("a".."z", "0".."9", "_" or "@")
 */
int
alphanum(char c)
{
    return (alpha(c) || isdigit(c));
}

/*  ishex
 *
 *  Test if character "c" is a hexadecimal digit ("0".."9" or "a".."f").
 */
int
ishex(char c)
{
    return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
}

/*  isoctal
 *
 *  Test if character "c" is an octal digit ("0".."7").
 */
int
isoctal(char c)
{
    return (c >= '0' && c <= '7');
}

/* The local variable table must be searched backwards, so that the deepest
 * nesting of local variables is searched first. The simplest way to do
 * this is to insert all new items at the head of the list.
 * In the global list, the symbols are kept in sorted order, so that the
 * public functions are written in sorted order.
 */
static symbol*
add_symbol(symbol* root, symbol* entry)
{
    entry->next = root->next;
    root->next = entry;
    if (root == &glbtab)
        AddToHashTable(sp_Globals, entry);
    return entry;
}

static void
free_symbol(symbol* sym)
{
    delete sym;
}

void
delete_symbol(symbol* root, symbol* sym)
{
    symbol* origRoot = root;
    /* find the symbol and its predecessor
     * (this function assumes that you will never delete a symbol that is not
     * in the table pointed at by "root")
     */
    assert(root != sym);
    while (root->next != sym) {
        root = root->next;
        assert(root != NULL);
    }

    if (origRoot == &glbtab)
        RemoveFromHashTable(sp_Globals, sym);

    /* unlink it, then free it */
    root->next = sym->next;
    free_symbol(sym);
}

int
get_actual_compound(symbol* sym)
{
    if (sym->ident == iARRAY || sym->ident == iREFARRAY) {
        while (sym->parent())
            sym = sym->parent();
    }

    return sym->compound;
}

void
delete_symbols(symbol* root, int level, int delete_functions)
{
    symbol* origRoot = root;
    symbol *sym, *parent_sym;
    int mustdelete;

    /* erase only the symbols with a deeper nesting level than the
     * specified nesting level */
    while (root->next != NULL) {
        sym = root->next;
        if (get_actual_compound(sym) < level)
            break;
        switch (sym->ident) {
            case iVARIABLE:
            case iARRAY:
                /* do not delete global variables if functions are preserved */
                mustdelete = delete_functions;
                break;
            case iREFERENCE:
                /* always delete references (only exist as function parameters) */
                mustdelete = TRUE;
                break;
            case iREFARRAY:
                /* a global iREFARRAY symbol is the return value of a function: delete
                 * this only if "globals" must be deleted; other iREFARRAY instances
                 * (locals) are also deleted
                 */
                mustdelete = delete_functions;
                for (parent_sym = sym->parent(); parent_sym != NULL && parent_sym->ident != iFUNCTN;
                     parent_sym = parent_sym->parent())
                    assert(parent_sym->ident == iREFARRAY);
                assert(parent_sym == NULL ||
                       (parent_sym->ident == iFUNCTN && parent_sym->parent() == NULL));
                if (parent_sym == NULL || parent_sym->ident != iFUNCTN)
                    mustdelete = TRUE;
                break;
            case iCONSTEXPR:
            case iENUMSTRUCT:
                /* delete constants, except predefined constants */
                mustdelete = delete_functions || !sym->predefined;
                break;
            case iFUNCTN:
                /* optionally preserve globals (variables & functions), but
                 * NOT native functions
                 */
                mustdelete = delete_functions || sym->native;
                assert(sym->parent() == NULL);
                break;
            case iMETHODMAP:
                // We delete methodmap symbols at the end, but since methodmaps
                // themselves get wiped, we null the pointer.
                sym->methodmap = nullptr;
                mustdelete = delete_functions;
                assert(!sym->parent());
                break;
            case iARRAYCELL:
            case iARRAYCHAR:
            case iEXPRESSION:
            case iVARARGS:
            case iACCESSOR:
            default:
                assert(0);
                break;
        }
        if (mustdelete) {
            if (origRoot == &glbtab)
                RemoveFromHashTable(sp_Globals, sym);
            root->next = sym->next;
            free_symbol(sym);
        } else {
            /* if the function was prototyped, but not implemented in this source,
             * mark it as such, so that its use can be flagged
             */
            if (sym->ident == iFUNCTN && !sym->defined)
                sym->missing = true;
            if (sym->ident == iFUNCTN || sym->ident == iVARIABLE || sym->ident == iARRAY)
                sym->defined = false;
            /* for user defined operators, also remove the "prototyped" flag, as
             * user-defined operators *must* be declared before use
             */
            if (sym->ident == iFUNCTN && !alpha(*sym->name()))
                sym->prototyped = false;
            if (origRoot == &glbtab)
                sym->clear_refers();
            root = sym; /* skip the symbol */
        }
    }
}


void
markusage(symbol* sym, int usage)
{
    // When compiling a skipped function, do not accumulate liveness information
    // for referenced functions.
    if (sc_status == statSKIP && sym->ident == iFUNCTN)
        return;

    sym->usage |= usage;
    if ((usage & uWRITTEN) != 0)
        sym->lnumber = fline;
    /* check if (global) reference must be added to the symbol */
    if ((usage & (uREAD | uWRITTEN)) != 0) {
        /* only do this for global symbols */
        if (sym->vclass == sGLOBAL && curfunc)
            curfunc->add_reference_to(sym);
    }
}

/*  findglb
 *
 *  Returns a pointer to the global symbol (if found) or NULL (if not found)
 */
symbol*
findglb(const char* name)
{
    return FindInHashTable(sp_Globals, name, fcurrent);
}

/*  findloc
 *
 *  Returns a pointer to the local symbol (if found) or NULL (if not found).
 *  See add_symbol() how the deepest nesting level is searched first.
 */
symbol*
findloc(const char* name)
{
    symbol* sym = loctab.next;
    sp::Atom* atom = gAtoms.add(name);
    while (sym != NULL) {
        if (atom == sym->nameAtom() &&
            (sym->parent() == NULL ||
             sym->ident ==
                 iCONSTEXPR)) /* sub-types (hierarchical types) are skipped, except for enum fields */
        {
            return sym; /* return first match */
        }
        sym = sym->next;
    }
    return nullptr;
}

symbol*
findconst(const char* name)
{
    symbol* sym;

    sym = findloc(name);          /* try local symbols first */
    if (sym == NULL || sym->ident != iCONSTEXPR) { /* not found, or not a constant */
        sym = FindInHashTable(sp_Globals, name, fcurrent);
    }
    if (sym == NULL || sym->ident != iCONSTEXPR)
        return NULL;
    assert(sym->parent() == NULL || sym->enumfield);
    /* ^^^ constants have no hierarchy, but enumeration fields may have a parent */
    return sym;
}

FunctionData::FunctionData()
 : funcid(0)
 , dbgstrs(nullptr)
{
    resizeArgs(0);
}

FunctionData::~FunctionData() {
    if (dbgstrs) {
        delete_stringtable(dbgstrs);
        free(dbgstrs);
    }
}

void
FunctionData::resizeArgs(size_t nargs)
{
    arginfo null_arg;
    memset(&null_arg, 0, sizeof(null_arg));

    args.resize(nargs);
    args.append(null_arg);
}

symbol::symbol()
 : symbol("", 0, 0, 0, 0)
{}

symbol::symbol(const char* symname, cell symaddr, int symident, int symvclass, int symtag)
 : next(nullptr),
   codeaddr(code_idx),
   vclass((char)symvclass),
   ident((char)symident),
   compound(0),
   tag(symtag),
   usage(0),
   defined(false),
   is_const(false),
   stock(false),
   is_public(false),
   is_static(false),
   is_struct(false),
   prototyped(false),
   missing(false),
   callback(false),
   skipped(false),
   retvalue(false),
   forward(false),
   native(false),
   enumroot(false),
   enumfield(false),
   predefined(false),
   deprecated(false),
   queued(false),
   x({}),
   fnumber(fcurrent),
   /* assume global visibility (ignored for local symbols) */
   lnumber(fline),
   documentation(nullptr),
   methodmap(nullptr),
   addr_(symaddr),
   name_(nullptr),
   referred_from_count_(0),
   parent_(nullptr),
   child_(nullptr)
{
    if (symname)
        name_ = gAtoms.add(symname);
    if (symident == iFUNCTN)
        data_.reset(new FunctionData);
    memset(&dim, 0, sizeof(dim));
}

symbol::symbol(const symbol& other)
 : symbol(nullptr, other.addr_, other.ident, other.vclass, other.tag)
{
    name_ = other.name_;

    usage = other.usage;
    defined = other.defined;
    prototyped = other.prototyped;
    missing = other.missing;
    enumroot = other.enumroot;
    enumfield = other.enumfield;
    predefined = other.predefined;
    callback = other.callback;
    skipped = other.skipped;
    retvalue = other.retvalue;
    forward = other.forward;
    native = other.native;
    stock = other.stock;
    is_struct = other.is_struct;
    is_public = other.is_public;
    is_const = other.is_const;
    deprecated = other.deprecated;
    // Note: explicitly don't add queued.

    x = other.x;
}

symbol::~symbol()
{
    if (ident == iFUNCTN) {
        /* run through the argument list; "default array" arguments
         * must be freed explicitly; the tag list must also be freed */
        for (arginfo* arg = &function()->args[0]; arg->ident != 0; arg++) {
            if (arg->ident == iREFARRAY && arg->hasdefault)
                free(arg->defvalue.array.data);
        }
    } else if (ident == iCONSTEXPR && enumroot) {
        /* free the constant list of an enum root */
        assert(dim.enumlist != NULL);
        delete_consttable(dim.enumlist);
        free(dim.enumlist);
    }
}

void
symbol::add_reference_to(symbol* other)
{
    for (symbol* sym : refers_to_) {
        if (sym == other)
            return;
    }
    refers_to_.append(other);
    other->referred_from_.append(this);
    other->referred_from_count_++;
}

void
symbol::drop_reference_from(symbol* from)
{
#if !defined(NDEBUG)
    bool found = false;
    for (size_t i = 0; i < referred_from_.length(); i++) {
        if (referred_from_[i] == from) {
            referred_from_[i] = nullptr;
            found = true;
            break;
        }
    }
    assert(found);
#endif
    referred_from_count_--;
}

/*  addsym
 *
 *  Adds a symbol to the symbol table (either global or local variables,
 *  or global and local constants).
 */
symbol*
addsym(const char* name, cell addr, int ident, int vclass, int tag)
{
    /* first fill in the entry */
    symbol* sym = new symbol(name, addr, ident, vclass, tag);

    /* then insert it in the list */
    if (vclass == sGLOBAL)
        return add_symbol(&glbtab, sym);
    return add_symbol(&loctab, sym);
}

symbol*
addvariable(const char* name, cell addr, int ident, int vclass, int tag, int dim[], int numdim,
            int idxtag[])
{
    return addvariable2(name, addr, ident, vclass, tag, dim, numdim, idxtag, 0);
}

symbol*
addvariable3(declinfo_t* decl, cell addr, int vclass, int slength)
{
    typeinfo_t* type = &decl->type;
    return addvariable2(decl->name, addr, type->ident, vclass, type->tag, type->dim, type->numdim,
                        type->idxtag, slength);
}

symbol*
addvariable2(const char* name, cell addr, int ident, int vclass, int tag, int dim[], int numdim,
             int idxtag[], int slength)
{
    symbol* sym;

    /* global variables may only be defined once
     * One complication is that functions returning arrays declare an array
     * with the same name as the function, so the assertion must allow for
     * this special case. Another complication is that variables may be
     * "redeclared" if they are local to an automaton (and findglb() will find
     * the symbol without states if no symbol with states exists).
     */
    assert(vclass != sGLOBAL || (sym = findglb(name)) == NULL || !sym->defined ||
           (sym->ident == iFUNCTN && sym == curfunc));

    if (ident == iARRAY || ident == iREFARRAY) {
        symbol *parent = NULL, *top;
        int level;
        sym = NULL; /* to avoid a compiler warning */
        for (level = 0; level < numdim; level++) {
            top = addsym(name, addr, ident, vclass, tag);
            top->defined = true;
            top->dim.array.length = dim[level];
            top->dim.array.slength = 0;
            if (level == numdim - 1 && tag == pc_tag_string) {
                if (slength == 0)
                    top->dim.array.length = dim[level] * sizeof(cell);
                else
                    top->dim.array.slength = slength;
            }
            top->dim.array.level = (short)(numdim - level - 1);
            top->x.tags.index = idxtag[level];
            top->set_parent(parent);
            if (parent) {
                parent->set_array_child(top);
            }
            parent = top;
            if (level == 0)
                sym = top;
        }
    } else {
        sym = addsym(name, addr, ident, vclass, tag);
        sym->defined = true;
    }
    return sym;
}

/*  getlabel
 *
 *  Returns te next internal label number. The global variable sc_labnum is
 *  initialized to zero.
 */
int
getlabel(void)
{
    return sc_labnum++;
}

/*  itoh
 *
 *  Converts a number to a hexadecimal string and returns a pointer to that
 *  string. This function is NOT re-entrant.
 */
char*
itoh(ucell val)
{
    static char itohstr[30];
    char* ptr;
    int i, nibble[16]; /* a 64-bit hexadecimal cell has 16 nibbles */
    int max = 8;
    ptr = itohstr;
    for (i = 0; i < max; i += 1) {
        nibble[i] = (int)(val & 0x0f); /* nibble 0 is lowest nibble */
        val >>= 4;
    } /* endfor */
    i = max - 1;
    while (nibble[i] == 0 && i > 0) /* search for highest non-zero nibble */
        i -= 1;
    while (i >= 0) {
        if (nibble[i] >= 10)
            *ptr++ = (char)('a' + (nibble[i] - 10));
        else
            *ptr++ = (char)('0' + nibble[i]);
        i -= 1;
    }
    *ptr = '\0'; /* and a zero-terminator */
    return itohstr;
}

int
lextok(token_t* tok)
{
    tok->id = lex(&tok->val, &tok->str);
    return tok->id;
}

int
expecttoken(int id, token_t* tok)
{
    int rval = needtoken(id);
    if (rval) {
        tok->val = current_token()->value;
        tok->id = current_token()->id;
        tok->str = current_token()->str;
        return rval;
    }
    return FALSE;
}

int
matchtoken2(int id, token_t* tok)
{
    if (matchtoken(id)) {
        tok->id = tokeninfo(&tok->val, &tok->str);
        return TRUE;
    }
    return FALSE;
}

int
matchsymbol(token_ident_t* ident)
{
    if (lextok(&ident->tok) != tSYMBOL) {
        lexpush();
        return FALSE;
    }
    strcpy(ident->name, ident->tok.str);
    ident->tok.str = ident->name;
    return TRUE;
}

int
needsymbol(token_ident_t* ident)
{
    if (!expecttoken(tSYMBOL, &ident->tok))
        return FALSE;
    strcpy(ident->name, ident->tok.str);
    ident->tok.str = ident->name;
    return TRUE;
}

symbol*
find_enumstruct_field(Type* type, const char* name)
{
    assert(type->asEnumStruct());

    char const_name[METHOD_NAMEMAX + 1];
    ke::SafeSprintf(const_name, sizeof(const_name), "%s::%s", type->name(), name);
    if (symbol* sym = findconst(const_name))
        return sym;
    return findglb(const_name);
}

void
declare_methodmap_symbol(methodmap_t* map, bool can_redef)
{
    if (!can_redef)
        return;

    symbol* sym = findglb(map->name);
    if (sym && sym->ident != iMETHODMAP) {
        if (sym->ident == iCONSTEXPR) {
            // We should only hit this on the first pass. Assert really hard that
            // we're about to kill an enum definition and not something random.
            assert(sc_status == statFIRST);
            assert(sym->ident == iCONSTEXPR);
            assert(map->tag == sym->tag);

            sym->ident = iMETHODMAP;

            // Kill previous enumstruct properties, if any.
            if (sym->enumroot) {
                for (constvalue* cv = sym->dim.enumlist; cv; cv = cv->next) {
                    symbol* csym = findglb(cv->name);
                    if (csym && csym->ident == iCONSTEXPR && csym->parent() == sym &&
                        csym->enumfield)
                    {
                        csym->enumfield = false;
                        csym->set_parent(nullptr);
                    }
                }
                delete_consttable(sym->dim.enumlist);
                free(sym->dim.enumlist);
                sym->dim.enumlist = NULL;
            }
        } else {
            error(11, map->name);
            sym = nullptr;
        }
    }

    if (!sym) {
        sym = addsym(map->name,  // name
                     0,          // addr
                     iMETHODMAP, // ident
                     sGLOBAL,    // vclass
                     map->tag);  // tag
        sym->defined = true;
    }
    sym->methodmap = map;
}

void
declare_handle_intrinsics()
{
    // Must not have an existing Handle methodmap.
    if (methodmap_find_by_name("Handle")) {
        error(156);
        return;
    }

    methodmap_t* map = methodmap_add(nullptr, Layout_MethodMap, "Handle");
    map->nullable = true;

    declare_methodmap_symbol(map, true);

    if (symbol* sym = findglb("CloseHandle")) {
        auto dtor = std::make_unique<methodmap_method_t>(map);
        dtor->target = sym;
        strcpy(dtor->name, "~Handle");
        map->dtor = dtor.get();
        map->methods.append(std::move(dtor));

        auto close = std::make_unique<methodmap_method_t>(map);
        close->target = sym;
        strcpy(close->name, "Close");
        map->methods.append(std::move(close));
    }
}
