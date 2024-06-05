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
#pragma once

#include <stdarg.h>

#include <unordered_set>

#include <amtl/am-string.h>
#include "lexer.h"
#include "sc.h"

namespace sp {
namespace cc {

class ParseNode;

enum class ErrorType {
    Suppressed,
    Warning,
    Error
};

struct ErrorReport {
    SourceLocation loc;
    int number;
    uint32_t fileno;
    uint32_t lineno;
    uint32_t col;
    std::shared_ptr<SourceFile> file;
    std::string message;
    ErrorType type;
};

class ReportManager;

class AutoErrorPos final
{
  public:
    explicit AutoErrorPos(const token_pos_t& pos);
    ~AutoErrorPos();

    const token_pos_t& pos() const {
        return pos_;
    }

  private:
    ReportManager* reports_;
    token_pos_t pos_;
    AutoErrorPos* prev_;
};

class MessageBuilder
{
  public:
    explicit MessageBuilder(int number);
    MessageBuilder(ParseNode* node, int number);
    MessageBuilder(MessageBuilder&& other);

    MessageBuilder(const MessageBuilder& other) = delete;

    MessageBuilder(const token_pos_t& where, int number)
      : where_(where),
        number_(number)
    {}
    ~MessageBuilder();

    MessageBuilder& operator <<(const char* arg) {
        args_.emplace_back(arg);
        return *this;
    }
    MessageBuilder& operator <<(const std::string& arg) {
        args_.emplace_back(arg);
        return *this;
    }
    MessageBuilder& operator <<(Atom* atom) {
        args_.emplace_back(atom ? atom->chars() : "<unknown>");
        return *this;
    }
    MessageBuilder& operator <<(Type* type);
    MessageBuilder& operator <<(QualType type);

    template <typename Integer,
              std::enable_if_t<std::is_integral<Integer>::value, bool> = true>
    MessageBuilder& operator <<(Integer n) {
        args_.emplace_back(std::to_string(n));
        return *this;
    }

    void operator =(const MessageBuilder& other) = delete;
    MessageBuilder& operator =(MessageBuilder&& other);

  private:
    token_pos_t where_;
    int number_;
    std::vector<std::string> args_;
    bool disabled_ = false;
};

static inline MessageBuilder report(const token_pos_t& where, int number) {
    return MessageBuilder(where, number);
}
static inline MessageBuilder report(int number) {
    return MessageBuilder(number);
}
static inline MessageBuilder report(ParseNode* node, int number) {
    return MessageBuilder(node, number);
}

#ifdef NDEBUG
static inline void break_on_error(int) {}
#else
void break_on_error(int number);
#endif

int pc_enablewarning(int number, int enable);

class AutoDeferReports;

class ReportManager
{
    friend class AutoDeferReports;

  public:
    ReportManager(CompileContext& cc);

    void ReportError(ErrorReport&& report);
    void ResetErrorFlag() { errflag_ = false; }
    void DumpErrorReport(bool clear);
    bool IsWarningDisabled(int number);

    // 0=disable, 1=enable, 2=toggle
    void EnableWarning(int number, int enable);

    unsigned int total_errors() const { return total_errors_; }
    unsigned int NumErrorMessages() const;
    unsigned int NumWarnMessages() const;

    void set_pos_override(AutoErrorPos* pos) { pos_override_ = pos; }
    AutoErrorPos* pos_override() const { return pos_override_; }

  private:
    void PushAutoDefer(AutoDeferReports* defer);
    void PopAutoDefer(AutoDeferReports* defer);

  private:
    CompileContext& cc_;
    bool errflag_ = false;
    unsigned int errors_on_line_ = 0;
    std::vector<ErrorReport> error_list_;
    AutoErrorPos* pos_override_ = nullptr;
    std::unordered_set<int> warn_disable_;
    int lastline_ = 0;
    short lastfile_ = 0;

    // This is not necessarily equal to the number of reports, since we
    // suppress generating a report if there are too many errors on one
    // line.
    unsigned int total_errors_ = 0;

    // This is the actual # of reported errors.
    size_t total_reported_errors_ = 0;

    std::vector<AutoDeferReports*> defers_;
};

class AutoDeferReports {
    friend class ReportManager;

  public:
    explicit AutoDeferReports(CompileContext& cc);
    ~AutoDeferReports();

    // Deactivate this deferral object and move all deferred reports back
    // into ReportManager.
    void Report();

    bool HasErrors() const { return has_errors_; }
    bool HasWarnings() const { return has_warnings_; }

  private:
    void AddDeferred(ErrorReport&& report);

  private:
    ReportManager* reports_;
    std::vector<ErrorReport> deferred_;
    bool has_errors_ = false;
    bool has_warnings_ = false;
};

class AutoCountErrors
{
  public:
    AutoCountErrors();

    void Reset();
    bool ok() const;

  private:
    ReportManager* reports_;
    unsigned old_errors_;
};

} // namespace cc
} // namespace sp
