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

#include "source-location.h"
#include "string-pool.h"
#include <am-refcounting.h>
#include <am-vector.h>
#include <am-string.h>
#include "source-manager.h"

namespace sp {

class CompileContext;
class Type;

enum class rmsg_type
{
  fatal,
  system,
  note,
  syntax,
  type
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
  TMessage(const SourceLocation &origin, rmsg::Id msgid)
   : origin_(origin),
     message_id_(msgid)
  {}

  class Arg
  {
   public:
    virtual AString Render(CompileContext &cc) = 0;
  };

  class StringArg : public Arg
  {
   public:
    explicit StringArg(const char *str)
     : str_(str)
    {}
    explicit StringArg(const AString &str)
     : str_(str)
    {}
    explicit StringArg(AString &&str)
     : str_(str)
    {}

    AString Render(CompileContext &cc) override {
      return str_;
    }

   private:
    AString str_;
  };

  class AtomArg : public Arg
  {
   public:
    explicit AtomArg(Atom *atom)
     : atom_(atom)
    {}

    AString Render(CompileContext &cc) override;

   private:
    Atom *atom_;
  };

  void addArg(Arg *arg) {
    args_.append(arg);
  }
  void addArg(Atom *atom) {
    addArg(new AtomArg(atom));
  }
  void addArg(const char *str) {
    addArg(new StringArg(str));
  }
  void addArg(const AString &str) {
    addArg(new StringArg(str));
  }
  void addArg(AString &&str) {
    addArg(new StringArg(str));
  }
  void addArg(Type *type);

  void addNote(Ref<TMessage> note) {
    if (note) {
      assert(!note->notes_.length());
      notes_.append(note);
    }
  }

  const SourceLocation &origin() const {
    return origin_;
  }
  const rmsg::Id id() const {
    return message_id_;
  }
  size_t num_notes() const {
    return notes_.length();
  }
  PassRef<TMessage> note(size_t i) const {
    return notes_[i];
  }
  const Vector<AutoPtr<Arg>> &args() const {
    return args_;
  }

 private:
  SourceLocation origin_;
  rmsg::Id message_id_;
  Vector<AutoPtr<Arg>> args_;
  Vector<Ref<TMessage>> notes_;
};

class MessageBuilder 
{
 public:
  explicit MessageBuilder(Ref<TMessage> report)
   : report_(report)
  {}
  MessageBuilder(MessageBuilder &&other)
   : report_(other.report_.forget())
  {}

  template <typename T>
  MessageBuilder &operator <<(const T &other) {
    if (!report_)
      return *this;
    report_->addArg(other);
    return *this;
  }
  MessageBuilder &operator <<(const MessageBuilder &other) {
    if (!report_)
      return *this;
    assert(report_ != other.report_);
    report_->addNote(other.report_);
    return *this;
  }

 private:
  Ref<TMessage> report_;
};

// The report manager is responsible for managing errors, warnings, and
// messages reported by the compilation process.
class ReportManager
{
 public:
  ReportManager(CompileContext &cc);

  bool HasErrors() const {
    return HasFatalError() || num_errors_ > 0;
  }
  bool HasFatalError() const {
    return fatal_error_ != rmsg::none;
  }

  void PrintMessages();

  void reportFatal(rmsg::Id msg) {
    if (!fatal_error_)
      fatal_error_ = msg;
  }
  void reportFatal(const SourceLocation &loc, rmsg::Id msg) {
    if (!fatal_error_) {
      fatal_error_ = msg;
      fatal_loc_ = loc;
    }
  }

  MessageBuilder report(const SourceLocation &loc, rmsg::Id msg_id);
  MessageBuilder note(const SourceLocation &loc, rmsg::Id msg_id);

 private:
  void printMessage(Ref<TMessage> message);
  void printSourceLine(const FullSourceRef &ref);

  AString renderSourceRef(const FullSourceRef &ref);
  AString renderMessage(rmsg::Id id,
                        const AutoPtr<TMessage::Arg> *args,
                        size_t len);

 private:
  CompileContext &cc_;
  rmsg::Id fatal_error_;
  SourceLocation fatal_loc_;

  unsigned num_errors_;
  Vector<Ref<TMessage>> messages_;
};

struct ReportingContext
{
 public:
  ReportingContext(CompileContext &cc, const SourceLocation &loc, bool shouldError = true)
   : cc_(cc),
     loc_(loc),
     should_error_(shouldError)
  {}

  void reportFatal(rmsg::Id msg);
  MessageBuilder report(rmsg::Id msg);

 private:
  CompileContext &cc_;
  const SourceLocation &loc_;
  bool should_error_;
};

}

#endif // _include_spcomp_reporting_h_
