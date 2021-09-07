// vim: set ts=8 sts=4 sw=4 tw=99 et:
/*  Pawn compiler - Error message system
 *  In fact a very simple system, using only 'panic mode'.
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
#ifdef _WIN32
#    include <io.h>
#endif
#if defined __linux__ || defined __GNUC__
#    include <unistd.h>
#endif
#include <stdarg.h> /* ANSI standardized variable argument list functions */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <sstream>
#include <utility>
#include <vector>

#include "compile-context.h"
#include "errors.h"
#include "lexer.h"
#include "sc.h"
#include "sclist.h"
#include "scvars.h"
#include "symbols.h"
#include "types.h"

#if defined _MSC_VER
#    pragma warning(push)
#    pragma warning(disable : 4125) /* decimal digit terminates octal escape sequence */
#endif

#include "messages.h"

#if defined _MSC_VER
#    pragma warning(pop)
#endif

#define NUM_WARNINGS (int)(sizeof warnmsg / sizeof warnmsg[0])
static unsigned char warndisable[(NUM_WARNINGS + 7) / 8]; /* 8 flags in a char */

static int errflag;
static int errorcount;
static AutoErrorPos* sPosOverride = nullptr;
bool sc_one_error_per_statement = false;
bool sc_shutting_down = false;

std::vector<ErrorReport> sErrorList;

void report_error(ErrorReport&& report);

AutoErrorPos::AutoErrorPos(const token_pos_t& pos)
  : pos_(pos),
    prev_(sPosOverride)
{
    sPosOverride = this;
}

AutoErrorPos::~AutoErrorPos()
{
    assert(sPosOverride == this);
    sPosOverride = prev_;
}

static inline ErrorType
DeduceErrorType(int number)
{
    if (number < 200 || (number < 300 && sc_warnings_are_errors) || number >= 400)
        return ErrorType::Error;
    if (number >= 300)
        return ErrorType::Fatal;

    int index = (number - 200) / 8;
    int mask = 1 << ((number - 200) % 8);
    if ((warndisable[index] & mask) != 0)
        return ErrorType::Suppressed;
    return ErrorType::Warning;
}

static inline const char*
GetErrorTypePrefix(ErrorType type)
{
    switch (type) {
        case ErrorType::Error:
            return "error";
        case ErrorType::Fatal:
            return "fatal error";
        case ErrorType::Warning:
        case ErrorType::Suppressed:
            return "warning";
        default:
            assert(false);
            return "(unknown)";
    }
}


static inline const char*
GetMessageForNumber(int number)
{
    if (number < 200) {
        assert(size_t(number - 1) < sizeof(errmsg) / sizeof(*errmsg));
        return errmsg[number - 1];
    }
    if (number < 300) {
        assert(size_t(number - 200) < sizeof(warnmsg) / sizeof(*warnmsg));
        return warnmsg[number - 200];
    }
    if (number >= 400) {
        assert(size_t(number - 400) < sizeof(errmsg_ex) / sizeof(*errmsg_ex));
        return errmsg_ex[number - 400];
    }
    assert(number >= FIRST_FATAL_ERROR);
    assert(size_t(number - FIRST_FATAL_ERROR) < sizeof(fatalmsg) / sizeof(*fatalmsg));
    return fatalmsg[number - FIRST_FATAL_ERROR];
}

/*  error
 *
 *  Outputs an error message (note: msg is passed optionally).
 *  If an error is found, the variable "errflag" is set and subsequent
 *  errors are ignored until lex() finds a semicolumn or a keyword
 *  (lex() resets "errflag" in that case).
 *
 *  Global references: inpfname   (reffered to only)
 *                     fline      (reffered to only)
 *                     fcurrent   (reffered to only)
 *                     errflag    (altered)
 */
int
error(int number, ...)
{
    if (sPosOverride) {
        va_list ap;
        va_start(ap, number);
        error_va(sPosOverride->pos(), number, ap);
        va_end(ap);
        return 0;
    }

    va_list ap;
    va_start(ap, number);
    ErrorReport report = ErrorReport::infer_va(number, ap);
    va_end(ap);

    report_error(std::move(report));
    return 0;
}

MessageBuilder::MessageBuilder(int number)
  : number_(number)
{
    if (sPosOverride)
        where_ = sPosOverride->pos();
    else
        where_ = CompileContext::get().lexer()->pos();
}

MessageBuilder::MessageBuilder(symbol* sym, int number)
  : number_(number)
{
    where_.file = sym->fnumber;
    where_.line = sym->lnumber;
    where_.col = 0;
}

MessageBuilder::MessageBuilder(MessageBuilder&& other)
  : where_(other.where_),
    number_(other.number_),
    args_(std::move(other.args_)),
    disabled_(false)
{
    other.disabled_ = true;
}

MessageBuilder&
MessageBuilder::operator =(MessageBuilder&& other)
{
    where_ = other.where_;
    number_ = other.number_;
    args_ = std::move(other.args_);
    disabled_ = false;
    other.disabled_ = true;
    return *this;
}

MessageBuilder::~MessageBuilder()
{
    if (disabled_)
        return;

    ErrorReport report;
    report.number = number_;
    report.fileno = where_.file;
    report.lineno = std::max(where_.line, 1);
    if (report.fileno >= 0) {
        report.filename = get_inputfile(report.fileno);
    } else {
        report.fileno = 0;
        report.filename = inpf->name();
    }
    report.type = DeduceErrorType(number_);

    std::ostringstream out;
    if (report.filename)
        out << report.filename << "(" << report.lineno << ") : ";
    out << GetErrorTypePrefix(report.type)
        << " " << ke::StringPrintf("%03d", report.number) << ": ";

    auto iter = args_.begin();
    const char* msg = GetMessageForNumber(number_);
    while (*msg) {
        if (*msg == '%' && (*(msg + 1) == 's' || *(msg + 1) == 'd')) {
            if (iter == args_.end())
                out << "<invalid>";
            else
                out << *iter++;
            msg += 2;
        } else {
            out << *msg++;
        }
    }

    report.message = out.str();
    report_error(std::move(report));
}

int
error(const token_pos_t& where, int number, ...)
{
    va_list ap;
    va_start(ap, number);
    ErrorReport report = ErrorReport::create_va(number, where.file, where.line, ap);
    va_end(ap);

    report.lineno = where.line;
    report_error(std::move(report));
    return 0;
}

int
error_va(const token_pos_t& where, int number, va_list ap)
{
    ErrorReport report = ErrorReport::create_va(number, where.file, where.line, ap);

    report.lineno = where.line;
    report_error(std::move(report));
    return 0;
}

int
error(symbol* sym, int number, ...)
{
    va_list ap;
    va_start(ap, number);
    ErrorReport report = ErrorReport::create_va(number, sym->fnumber, sym->lnumber, ap);
    va_end(ap);

    report_error(std::move(report));
    return 0;
}

static void
abort_compiler()
{
    if (strlen(errfname) == 0) {
        fprintf(stdout, "Compilation aborted.\n");
    }
    longjmp(errbuf, 2); /* fatal error, quit */
}

ErrorReport
ErrorReport::create_va(int number, int fileno, int lineno, va_list ap)
{
    ErrorReport report;
    report.number = number;
    report.fileno = fileno;
    report.lineno = std::max(lineno, 1);
    if (report.fileno >= 0) {
        report.filename = get_inputfile(report.fileno);
    } else {
        report.fileno = 0;
        report.filename = inpf->name();
    }
    report.type = DeduceErrorType(number);

    const char* prefix = GetErrorTypePrefix(report.type);
    const char* format = GetMessageForNumber(report.number);

    char msg[1024];
    ke::SafeVsprintf(msg, sizeof(msg), format, ap);

    char base[1024];
    ke::SafeSprintf(base, sizeof(base), "%s(%d) : %s %03d: ", report.filename, report.lineno,
                    prefix, report.number);

    char full[2048];
    ke::SafeSprintf(full, sizeof(full), "%s%s", base, msg);
    report.message = full;

    return report;
}

ErrorReport
ErrorReport::infer_va(int number, va_list ap)
{
    return create_va(number, -1, fline, ap);
}

void
clear_errors()
{
    sErrorList.clear();
    warnnum = 0;

    // :TODO: What? Combine these variables.
    sc_total_errors = 0;
    errorcount = 0;
    errnum = 0;
}

void
report_error(ErrorReport&& report)
{
    static int lastline;
    static short lastfile;

    /* errflag is reset on each semicolon.
     * In a two-pass compiler, an error should not be reported twice. Therefore
     * the error reporting is enabled only in the second pass (and only when
     * actually producing output). Fatal errors may never be ignored.
     */
    if (report.type != ErrorType::Fatal) {
        // This is needed so Analyze() can return "true" but still propagate errors.
        if (report.type == ErrorType::Error)
            sc_total_errors++;

        if (errflag && sc_one_error_per_statement)
            return;
    }

    break_on_error(report.number);

    sErrorList.emplace_back(std::move(report));

    switch (sErrorList.back().type) {
        case ErrorType::Suppressed:
            return;
        case ErrorType::Warning:
            warnnum++;
            break;
        case ErrorType::Error:
        case ErrorType::Fatal:
            errnum++;
            sc_total_errors++;
            if (sc_one_error_per_statement)
                errflag = TRUE;
            break;
    }

    if (sErrorList.back().type == ErrorType::Fatal || errnum > 25) {
        if (sc_shutting_down)
            dump_error_report(true);
        else
            abort_compiler();
        return;
    }

    // Count messages per line, reset if not the same line.
    if (lastline != sErrorList.back().lineno || sErrorList.back().fileno != lastfile)
        errorcount = 0;

    lastline = sErrorList.back().lineno;
    lastfile = sErrorList.back().fileno;

    if (sErrorList.back().type != ErrorType::Warning)
        errorcount++;
    if (errorcount >= 3)
        error(FATAL_ERROR_OVERWHELMED_BY_BAD);
}

void
dump_error_report(bool clear)
{
    static FILE* stdfp = sc_use_stderr ? stderr : stdout;

    FILE* fp = nullptr;
    if (strlen(errfname) > 0)
        fp = fopen(errfname, "a");
    if (!fp)
        fp = stdfp;

    std::sort(sErrorList.begin(), sErrorList.end(),
              [](const ErrorReport& a, const ErrorReport& b) -> bool {
        if (a.fileno == b.fileno)
            return a.lineno < b.lineno;
        return a.fileno > b.fileno;
    });

    for (const auto& report : sErrorList)
        fprintf(fp, "%s", report.message.c_str());
    fflush(fp);

    if (fp != stdfp)
        fclose(fp);

    if (clear)
        sErrorList.clear();
}

void
errorset(int code, int line)
{
    switch (code) {
        case sRESET:
            errflag = FALSE; /* start reporting errors */
            break;
    }
}

/* sc_enablewarning()
 * Enables or disables a warning (errors cannot be disabled).
 * Initially all warnings are enabled. The compiler does this by setting bits
 * for the *disabled* warnings and relying on the array to be zero-initialized.
 *
 * Parameter enable can be:
 *  o  0 for disable
 *  o  1 for enable
 *  o  2 for toggle
 */
int
pc_enablewarning(int number, int enable)
{
    int index;
    unsigned char mask;

    if (number < 200)
        return FALSE; /* errors and fatal errors cannot be disabled */
    number -= 200;
    if (number >= NUM_WARNINGS)
        return FALSE;

    index = number / 8;
    mask = (unsigned char)(1 << (number % 8));
    switch (enable) {
        case 0:
            warndisable[index] |= mask;
            break;
        case 1:
            warndisable[index] &= (unsigned char)~mask;
            break;
        case 2:
            warndisable[index] ^= mask;
            break;
    }

    return TRUE;
}

#ifndef NDEBUG
void break_on_error(int number)
{
}
#endif

MessageBuilder&
MessageBuilder::operator <<(Type* type)
{
    args_.emplace_back(type->prettyName());
    return *this;
}
