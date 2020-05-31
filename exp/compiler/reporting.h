// vim: set ts=2 sw=2 tw=99 et:
// 
// Copyright (C) 2012-2014 AlliedModders LLC, David Anderson
// 
// This file is part of SourcePawn.
// 
// SourcePawn is free software: you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation, either version 3 of the License, or (at your option)
// any later version.
// 
// SourcePawn is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.
#ifndef _include_spcomp_reporting_h_
#define _include_spcomp_reporting_h_

#include <memory>

#include <amtl/am-refcounting.h>
#include <amtl/am-vector.h>
#include <amtl/am-string.h>
#include "shared/string-pool.h"
#include "source-location.h"

namespace sp {

class CompileContext;
class SourceManager;
class Type;
struct FullSourceRef;

enum class rmsg_type
{
  fatal,
  system,
  note,
  syntax,
  type,
  impl,
  warning
};

namespace rmsg
{
  enum Id {
    none,
#define RMSG(Name, Type, String)    Name,
# include "messages.tbl"
#undef RMSG
    sentinel
  };
};

class TMessage : public ke::Refcounted<TMessage>
{
 public: 
  TMessage(const SourceLocation& origin, rmsg::Id msgid)
   : origin_(origin),
     message_id_(msgid)
  {}

  class Arg
  {
   public:
    virtual ~Arg() {}
    virtual std::string Render() = 0;
  };

  class StringArg : public Arg
  {
   public:
    explicit StringArg(const char* str)
     : str_(str)
    {}
    explicit StringArg(const std::string& str)
     : str_(str)
    {}
    explicit StringArg(std::string&& str)
     : str_(str)
    {}

    std::string Render() override {
      return str_;
    }

   private:
    std::string str_;
  };

  class AtomArg : public Arg
  {
   public:
    explicit AtomArg(Atom* atom)
     : atom_(atom)
    {}

    std::string Render() override;

   private:
    Atom* atom_;
  };

  void addArg(Arg* arg) {
    args_.push_back(std::unique_ptr<Arg>(arg));
  }
  void addArg(Atom* atom) {
    addArg(new AtomArg(atom));
  }
  void addArg(const char* str) {
    addArg(new StringArg(str));
  }
  void addArg(const std::string& str) {
    addArg(new StringArg(str));
  }
  void addArg(std::string&& str) {
    addArg(new StringArg(str));
  }
  void addArg(size_t value);
  void addArg(Type* type);

  void addNote(RefPtr<TMessage> note) {
    if (note) {
      assert(!note->notes_.size());
      notes_.push_back(note);
    }
  }

  const SourceLocation& origin() const {
    return origin_;
  }
  const rmsg::Id id() const {
    return message_id_;
  }
  size_t num_notes() const {
    return notes_.size();
  }
  RefPtr<TMessage> note(size_t i) const {
    return notes_[i];
  }
  const std::vector<std::unique_ptr<Arg>>& args() const {
    return args_;
  }

 private:
  SourceLocation origin_;
  rmsg::Id message_id_;
  std::vector<std::unique_ptr<Arg>> args_;
  std::vector<RefPtr<TMessage>> notes_;
};

class MessageBuilder 
{
 public:
  explicit MessageBuilder(RefPtr<TMessage> report)
   : report_(report)
  {}
  MessageBuilder(const MessageBuilder& other)
   : report_(other.report_)
  {}
  MessageBuilder(MessageBuilder&& other)
   : report_(other.report_.forget())
  {}

  template <typename T>
  MessageBuilder& operator <<(const T& other) {
    if (!report_)
      return *this;
    report_->addArg(other);
    return *this;
  }
  MessageBuilder& operator <<(const MessageBuilder& other) {
    if (!report_)
      return *this;
    if (!other.report_)
      return *this;
    assert(report_ != other.report_);
    report_->addNote(other.report_);
    return *this;
  }

  RefPtr<TMessage> get() const {
    return report_;
  }

 private:
  RefPtr<TMessage> report_;
};

// The report manager is responsible for managing errors, warnings, and
// messages reported by the compilation process.
class ReportManager
{
 public:
  ReportManager();

  bool HasErrors() const {
    return HasFatalError() || num_errors_ > 0;
  }
  bool HasFatalError() const {
    return fatal_error_ != rmsg::none;
  }
  bool HasMessages() const {
    return HasFatalError() || messages_.size() > 0;
  }

  void PrintMessages();

 // Internal API.
  void setSourceManager(SourceManager* srcmgr) {
    source_ = srcmgr;
  }

  void reportFatal(rmsg::Id msg) {
    if (!fatal_error_)
      fatal_error_ = msg;
  }
  void reportFatal(const SourceLocation& loc, rmsg::Id msg) {
    if (!fatal_error_) {
      fatal_error_ = msg;
      fatal_loc_ = loc;
    }
  }

  MessageBuilder report(const SourceLocation& loc, rmsg::Id msg_id);
  MessageBuilder note(const SourceLocation& loc, rmsg::Id msg_id);
  MessageBuilder build(const SourceLocation& loc, rmsg::Id msg_id);
  void report(const RefPtr<TMessage>& msg);

 private:
  void printMessage(RefPtr<TMessage> message);
  void printSourceLine(const FullSourceRef& ref);

  std::string renderSourceRef(const FullSourceRef& ref);
  std::string renderMessage(rmsg::Id id,
                            const std::unique_ptr<TMessage::Arg>* args,
                            size_t len);

 private:
  SourceManager* source_;
  rmsg::Id fatal_error_;
  SourceLocation fatal_loc_;

  unsigned num_errors_;
  std::vector<RefPtr<TMessage>> messages_;
};

struct ReportingContext
{
 public:
  ReportingContext(CompileContext& cc, const SourceLocation& loc, bool shouldError = true);
  ReportingContext(ReportManager& rr, const SourceLocation& loc, bool shouldError = true)
   : rr_(rr),
     loc_(loc),
     should_error_(shouldError)
  {}

  void reportFatal(rmsg::Id msg) {
    rr_.reportFatal(msg);
  }
  MessageBuilder report(rmsg::Id msg) {
    return rr_.report(loc_, msg);
  }
  MessageBuilder build(rmsg::Id msg) {
    return rr_.build(loc_, msg);
  }
  MessageBuilder note(rmsg::Id msg) {
    return rr_.note(loc_, msg);
  }

 private:
  ReportManager& rr_;
  const SourceLocation& loc_;
  bool should_error_;
};

}

#endif // _include_spcomp_reporting_h_
