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
#include "compile-options.h"
#include "errors.h"
#include "lexer.h"
#include "parse-node.h"
#include "sc.h"
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

void report_error(ErrorReport&& report);

AutoErrorPos::AutoErrorPos(const token_pos_t& pos)
  : reports_(CompileContext::get().reports()),
    pos_(pos)
{
    prev_ = reports_->pos_override();
    reports_->set_pos_override(this);
}

AutoErrorPos::~AutoErrorPos()
{
    assert(reports_->pos_override() == this);
    reports_->set_pos_override(prev_);
}

static inline ErrorType
DeduceErrorType(int number)
{
    auto& cc = CompileContext::get();
    if (number < 200 || (number < 300 && cc.options()->warnings_are_errors) || number >= 400)
        return ErrorType::Error;

    if (cc.reports()->IsWarningDisabled(number))
        return ErrorType::Suppressed;
    return ErrorType::Warning;
}

static inline const char*
GetErrorTypePrefix(ErrorType type)
{
    switch (type) {
        case ErrorType::Error:
            return "error";
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

    assert(number >= 400);
    assert(size_t(number - 400) < sizeof(errmsg_ex) / sizeof(*errmsg_ex));
    return errmsg_ex[number - 400];
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
error(int number)
{
    auto& cc = CompileContext::get();
    if (auto pos_override = cc.reports()->pos_override()) {
        error(pos_override->pos(), number);
        return 0;
    }

    ErrorReport report = ErrorReport::infer(number);

    report_error(std::move(report));
    return 0;
}

MessageBuilder::MessageBuilder(int number)
  : number_(number)
{
    auto& cc = CompileContext::get();
    if (auto pos_override = cc.reports()->pos_override())
        where_ = pos_override->pos();
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

MessageBuilder::MessageBuilder(ParseNode* node, int number)
{
    where_ = node->pos();
    number_ = number;
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

    auto& cc = CompileContext::get();

    ErrorReport report;
    report.number = number_;
    report.fileno = where_.file;
    report.lineno = std::max(where_.line, 1);
    if (report.fileno < 0)
        report.fileno = cc.lexer()->fcurrent();
    if (report.fileno < cc.sources()->opened_files().size())
        report.filename = cc.sources()->opened_files().at(report.fileno)->name();
    else
        report.filename = cc.options()->source_files[0];
    report.type = DeduceErrorType(number_);

    std::ostringstream out;
    if (!report.filename.empty())
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
error(const token_pos_t& where, int number)
{
    ErrorReport report = ErrorReport::create(number, where.file, where.line);

    report.lineno = where.line;
    report_error(std::move(report));
    return 0;
}

int
error(symbol* sym, int number)
{
    ErrorReport report = ErrorReport::create(number, sym->fnumber, sym->lnumber);

    report_error(std::move(report));
    return 0;
}

ErrorReport
ErrorReport::create(int number, int fileno, int lineno)
{
    auto& cc = CompileContext::get();

    ErrorReport report;
    report.number = number;
    report.fileno = fileno;
    report.lineno = std::max(lineno, 1);
    if (report.fileno < 0)
        report.fileno = cc.lexer()->fcurrent();
    if (report.fileno < cc.sources()->opened_files().size())
        report.filename = cc.sources()->opened_files().at(report.fileno)->name();
    else
        report.filename = cc.options()->source_files[0];
    report.type = DeduceErrorType(number);

    const char* prefix = GetErrorTypePrefix(report.type);
    const char* format = GetMessageForNumber(report.number);

    // Do not format "format" anymore, legacy error() is only allowed for
    // non-formatted messages.
    report.message = ke::StringPrintf("%s(%d) : %s %03d: %s", report.filename.c_str(),
                                      report.lineno, prefix, report.number, format);
    return report;
}

ErrorReport
ErrorReport::infer(int number)
{
    auto& cc = CompileContext::get();
    return create(number, -1, cc.lexer()->fline());
}

void
report_error(ErrorReport&& report)
{
    auto& cc = CompileContext::get();
    cc.reports()->ReportError(std::move(report));
}

void
ReportManager::ReportError(ErrorReport&& report)
{
    /* errflag is reset on each semicolon.
     * In a two-pass compiler, an error should not be reported twice. Therefore
     * the error reporting is enabled only in the second pass (and only when
     * actually producing output).
     */
    // This is needed so Analyze() can return "true" but still propagate errors.
    if (report.type == ErrorType::Error)
        total_errors_++;

    if (errflag_ && cc_.one_error_per_stmt())
        return;

    break_on_error(report.number);

    error_list_.emplace_back(std::move(report));

    switch (error_list_.back().type) {
        case ErrorType::Suppressed:
            return;
        case ErrorType::Warning:
            break;
        case ErrorType::Error:
            total_errors_++;
            total_reported_errors_++;
            if (cc_.one_error_per_stmt())
                errflag_ = true;
            break;
    }

    if (total_reported_errors_ > 25)
        cc_.set_must_abort();

    // Count messages per line, reset if not the same line.
    if (lastline_ != error_list_.back().lineno || error_list_.back().fileno != lastfile_)
        errors_on_line_ = 0;

    lastline_ = error_list_.back().lineno;
    lastfile_ = error_list_.back().fileno;

    if (error_list_.back().type != ErrorType::Warning)
        errors_on_line_++;
    if (errors_on_line_ >= 3)
        cc_.set_must_abort();
}

void
ReportManager::DumpErrorReport(bool clear)
{
    FILE* stdfp = cc_.options()->use_stderr ? stderr : stdout;

    FILE* fp = nullptr;
    if (!cc_.errfname().empty())
        fp = fopen(cc_.errfname().c_str(), "a");
    if (!fp)
        fp = stdfp;

    std::sort(error_list_.begin(), error_list_.end(),
              [](const ErrorReport& a, const ErrorReport& b) -> bool {
        if (a.fileno == b.fileno)
            return a.lineno < b.lineno;
        return a.fileno > b.fileno;
    });

    for (const auto& report : error_list_)
        fprintf(fp, "%s", report.message.c_str());
    fflush(fp);

    if (fp != stdfp)
        fclose(fp);

    if (clear)
        error_list_.clear();
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
void
ReportManager::EnableWarning(int number, int enable)
{
    switch (enable) {
        case 0:
            warn_disable_.emplace(number);
            break;
        case 1: {
            auto iter = warn_disable_.find(number);
            if (iter != warn_disable_.end())
                warn_disable_.erase(iter);
            break;
        }
        case 2: {
            auto iter = warn_disable_.find(number);
            if (iter != warn_disable_.end())
                warn_disable_.erase(iter);
            else
                warn_disable_.emplace(number);
            break;
        }
    }
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

ReportManager::ReportManager(CompileContext& cc)
  : cc_(cc)
{
}

unsigned int
ReportManager::NumErrorMessages() const
{
    unsigned int total = 0;
    for (const auto& report : error_list_) {
        if (report.type == ErrorType::Error)
            total++;
    }
    return total;
}

unsigned int
ReportManager::NumWarnMessages() const
{
    unsigned int total = 0;
    for (const auto& report : error_list_) {
        if (report.type == ErrorType::Warning)
            total++;
    }
    return total;
}

bool
ReportManager::IsWarningDisabled(int number)
{
    return warn_disable_.count(number) > 0;
}

AutoCountErrors::AutoCountErrors()
  : reports_(CompileContext::get().reports()),
    old_errors_(reports_->total_errors())
{
}

void
AutoCountErrors::Reset()
{
    old_errors_ = reports_->total_errors();
}

bool
AutoCountErrors::ok() const
{
    return old_errors_ == reports_->total_errors();
}
