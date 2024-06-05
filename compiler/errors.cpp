// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
//  Copyright (c) ITB CompuPhase, 1997-2006
//  Copyright (c) 2023 AlliedModders LLC
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

namespace sp {
namespace cc {

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

MessageBuilder::MessageBuilder(int number)
  : number_(number)
{
    auto& cc = CompileContext::get();
    if (auto pos_override = cc.reports()->pos_override())
        where_ = pos_override->pos();
    else
        where_ = CompileContext::get().lexer()->pos();
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
    report.loc = where_;
    report.number = number_;
    if (where_.valid())
        report.fileno = cc.sources()->GetSourceFileIndex(where_);
    else
        report.fileno = 0;

    if (report.fileno < cc.sources()->opened_files().size())
        report.file = cc.sources()->opened_files().at(report.fileno);
    else if (!cc.sources()->opened_files().empty())
        report.file = cc.sources()->opened_files().at(0);

    uint32_t actual_line = cc.sources()->GetLineAndCol(where_, &report.col);

    // Rely on tokline when it's there, but... we should ditch it here, we
    // have the technology.
    if (where_.valid() && !where_.line)
        where_.line = actual_line;
    report.lineno = std::max(where_.line, 1);

    report.type = DeduceErrorType(number_);

    std::ostringstream out;
    if (report.file)
        out << report.file->name() << "(" << report.lineno << ") : ";
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

    cc.reports()->ReportError(std::move(report));
}

void ReportManager::ReportError(ErrorReport&& report) {
    if (!defers_.empty()) {
        defers_.back()->AddDeferred(std::move(report));
        return;
    }

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

void DumpDiagnostic(FILE* fp, const ErrorReport& report) {
    auto line = report.file->GetLine(report.lineno);
    while (!line.empty() && (line.back() == '\r' || line.back() == '\n'))
        line.pop_back();

    // Replace \t with eight spaces since terminals do weird stuff with tabs.
    static constexpr char kPrintedTab[] = "        ";

    std::string printed_line;
    for (size_t i = 0; i < line.size(); i++) {
        if (line[i] == '\t')
            printed_line += kPrintedTab;
        else
            printed_line += line[i];
    }

    fprintf(fp, "%6u | %s\n", report.lineno, printed_line.c_str());

    // This should pass, but doesn't.
    // Fails in tests/compile-only/fail-empty-preproc-expr.sp and :TODO: to
    // investigate why.
    // assert(report.col <= line.size());

    uint32_t num_dashes = 9;
    for (size_t i = 1; i <= std::min((size_t)report.col - 1, line.size()); i++) {
        if (line[i - 1] == '\t')
            num_dashes += 8;
        else
            num_dashes += 1;
    }

    for (uint32_t i = 0; i < num_dashes; i++)
        fprintf(fp, "-");
    fprintf(fp, "^\n");
}

void ReportManager::DumpErrorReport(bool clear) {
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

    for (const auto& report : error_list_) {
        if (report.type == ErrorType::Suppressed)
            continue;
        fprintf(fp, "%s", report.message.c_str());
        if (report.loc.valid())
            DumpDiagnostic(fp, report);
        if (&report != &error_list_.back())
            fprintf(fp, "\n");
    }
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
void ReportManager::EnableWarning(int number, int enable) {
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

MessageBuilder&
MessageBuilder::operator <<(QualType type)
{
    std::string message;
    if (type.is_const())
        message += "const ";
    message += type->prettyName();
    args_.emplace_back(message);
    return *this;
}

ReportManager::ReportManager(CompileContext& cc)
  : cc_(cc)
{
}

unsigned int ReportManager::NumErrorMessages() const {
    unsigned int total = 0;
    for (const auto& report : error_list_) {
        if (report.type == ErrorType::Error)
            total++;
    }
    return total;
}

unsigned int ReportManager::NumWarnMessages() const {
    unsigned int total = 0;
    for (const auto& report : error_list_) {
        if (report.type == ErrorType::Warning)
            total++;
    }
    return total;
}

bool ReportManager::IsWarningDisabled(int number) {
    return warn_disable_.count(number) > 0;
}

void ReportManager::PushAutoDefer(AutoDeferReports* defer) {
    defers_.emplace_back(defer);
}

void ReportManager::PopAutoDefer(AutoDeferReports* defer) {
    assert(!defers_.empty());
    assert(defers_.back() == defer);
    defers_.pop_back();
}

AutoCountErrors::AutoCountErrors()
  : reports_(CompileContext::get().reports()),
    old_errors_(reports_->total_errors())
{
}

void AutoCountErrors::Reset() {
    old_errors_ = reports_->total_errors();
}

bool AutoCountErrors::ok() const {
    return old_errors_ == reports_->total_errors();
}

AutoDeferReports::AutoDeferReports(CompileContext& cc)
  : reports_(cc.reports())
{
    reports_->PushAutoDefer(this);
}

AutoDeferReports::~AutoDeferReports() {
    if (reports_)
        reports_->PopAutoDefer(this);
}

void AutoDeferReports::Report() {
    if (!reports_)
        return;

    reports_->PopAutoDefer(this);

    for (auto&& report : deferred_)
        reports_->ReportError(std::move(report));
    deferred_.clear();
    reports_ = nullptr;
}

void AutoDeferReports::AddDeferred(ErrorReport&& report) {
    if (report.type == ErrorType::Error)
        has_errors_ = true;
    else
        has_warnings_ = true;
    deferred_.emplace_back(std::move(report));
}

} // namespace cc
} // namespace sp
