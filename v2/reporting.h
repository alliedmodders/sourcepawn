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

#include "messages.h"
#include "source-location.h"
#include <am-refcounting.h>

namespace ke {

class CompileContext;

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
#define MSG(Name, Type, String)
#define RMSG(Name, Type, String)    Name,
# include "messages.tbl"
#undef MSG
#undef RMSG
    sentinel
  };
};

class Report : public ke::Refcounted<Report>
{
 public: 
  Report(const SourceLocation &origin, rmsg::Id msgid)
   : origin_(origin),
     message_id_(msgid)
  {}

  class Arg
  {
   public:
  };

 private:
  SourceLocation origin_;
  rmsg::Id message_id_;
  Vector<Ref<Report>> notes_;
};

// The report manager is responsible for managing errors, warnings, and
// messages reported by the compilation process.
class ReportManager
{
 public:
  ReportManager(CompileContext &cc);

  bool HasErrors() const {
    return HasFatalError();
  }
  bool HasFatalError() const {
    return fatal_error_ != rmsg::none;
  }

  void reportFatal(rmsg::Id msg) {
    if (!fatal_error_)
      fatal_error_ = msg;
  }

 private:
  CompileContext &cc_;
  rmsg::Id fatal_error_;
};

struct ReportingContext
{
 public:
  ReportingContext(CompileContext &cc, const SourceLocation &loc, bool shouldError = true)
   : cc_(cc),
     loc_(loc),
     should_error_(shouldError)
  {}

  void reportError(Message msg, ...);
  void reportFatal(rmsg::Id msg);

 private:
  CompileContext &cc_;
  const SourceLocation &loc_;
  bool should_error_;
};

}

#endif // _include_spcomp_reporting_h_
