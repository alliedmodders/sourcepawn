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
#include <amtl/am-platform.h>
#if defined(KE_POSIX)
# include <sys/ioctl.h>
# include <unistd.h>
#endif

#include <algorithm>

#include "reporting.h"
#include "compile-context.h"
#include "source-manager.h"
#include "auto-string.h"

using namespace ke;
using namespace sp;

static const char* sMessageTypeStrings[] = {
  "fatal",
  "system",
  "note",
  "syntax error",
  "type error",
  "internal error",
  "warning",
};

struct rmsg_info {
  rmsg_type type;
  const char* text;
};

static const rmsg_info sMessageTable[] =
{
  { rmsg_type::fatal, "unknown" },
#define RMSG(Name, Type, String) \
  { rmsg_type::Type, String },
# include "messages.tbl"
#undef RMSG
};

static inline const char*
PrintableMessageType(rmsg_type type)
{
  return sMessageTypeStrings[(int)type];
}

static inline const rmsg_info&
GetMessageInfo(rmsg::Id id)
{
  assert(id > rmsg::none && id < rmsg::sentinel);
  return sMessageTable[id];
}

void
TMessage::addArg(Type* type)
{
  addArg(BuildTypeName(type));
}

void
TMessage::addArg(size_t value)
{
  char buffer[24];
  SafeSprintf(buffer, sizeof(buffer), "%" KE_FMT_SIZET, value);
  addArg(buffer);
}

ReportManager::ReportManager()
 : source_(nullptr),
   fatal_error_(rmsg::none),
   num_errors_(0)
{
}

ReportingContext::ReportingContext(CompileContext& cc, const SourceLocation& loc, bool shouldError)
 : rr_(cc.reporting()),
   loc_(loc),
   should_error_(shouldError)
{
}

static const size_t kErrorMessageLimit = 100;

MessageBuilder
ReportManager::note(const SourceLocation& loc, rmsg::Id msg_id)
{
  assert(GetMessageInfo(msg_id).type == rmsg_type::note);

  // Don't create any new messages after we've reached a limit.
  if (num_errors_ >= kErrorMessageLimit)
    return MessageBuilder(nullptr);

  RefPtr<TMessage> message = new TMessage(loc, msg_id);
  return MessageBuilder(message);
}

void
ReportManager::report(const RefPtr<TMessage>& msg)
{
  assert(GetMessageInfo(msg->id()).type != rmsg_type::fatal);

  // Don't create any new messages after we've reached a limit.
  if (num_errors_ >= kErrorMessageLimit)
    return;

  rmsg_type type = GetMessageInfo(msg->id()).type;
  if (type != rmsg_type::note && type != rmsg_type::warning) {
    num_errors_++;

    if (num_errors_ >= kErrorMessageLimit)
      reportFatal(msg->origin(), rmsg::too_many_errors);
  }

  messages_.push_back(msg);
}

MessageBuilder
ReportManager::build(const SourceLocation& loc, rmsg::Id msg_id)
{
  RefPtr<TMessage> message = new TMessage(loc, msg_id);
  return MessageBuilder(message);
}

MessageBuilder
ReportManager::report(const SourceLocation& loc, rmsg::Id msg_id)
{
  if (num_errors_ >= kErrorMessageLimit)
    return MessageBuilder(nullptr);

  RefPtr<TMessage> message = new TMessage(loc, msg_id);
  report(message);

  return MessageBuilder(message);
}

void
ReportManager::PrintMessages()
{
  if (fatal_error_ == rmsg::outofmemory) {
    fprintf(stderr, "fatal error: out of memory\n");
    return;
  }

  for (size_t i = 0; i < messages_.size(); i++) {
    printMessage(messages_[i]);
    if (i != messages_.size() - 1)
      fprintf(stderr, "\n");
  }

  if (fatal_error_ != rmsg::none) {
    FullSourceRef ref = source_->decode(fatal_loc_);
    AutoString line = renderSourceRef(ref);
    line = line + ": ";
    line = line + renderMessage(fatal_error_, nullptr, 0);
    fprintf(stderr, "%s\n", line.ptr());
  }
}

static inline bool
IsDigit(char c)
{
  return c >= '0' && c <= '9';
}

static inline unsigned
GetTerminalWidth()
{
#if defined(KE_POSIX)
  if (!isatty(STDERR_FILENO))
    return 128;

  struct winsize w;
  if (ioctl(STDERR_FILENO, TIOCGWINSZ, &w) == -1)
    return 80;
  return w.ws_col;
#else
  return 80;
#endif
}

static std::string
ExpandTabsInLine(const char* line, size_t length)
{
  AutoString builder;

  size_t last_tab = 0;
  for (size_t i = 0; i < length; i++) {
    if (line[i] == '\t') {
      builder = builder + std::string(&line[last_tab], i - last_tab);
      builder = builder + "        ";

      last_tab = i + 1;
      continue;
    }
  }

  if (last_tab < length)
    builder = builder + std::string(&line[last_tab], length - last_tab);
  return std::string(builder.ptr());
}

void
ReportManager::printSourceLine(const FullSourceRef& ref)
{
  static const char* long_prefix = " ... ";
  static const char* long_suffix = " ... ";
  const size_t prefix_len = strlen(long_prefix);
  const size_t suffix_len = strlen(long_suffix);
  const unsigned min_cols = suffix_len + prefix_len + 12;

  // Fudge factor, we assume we can print at least 16 columns.
  const unsigned max_cols = std::max(GetTerminalWidth(), min_cols);

  const unsigned line_index = ref.line - 1;
  LineExtents* lines = ref.file->lineCache();
  const char* lineptr = ref.file->chars() + lines->at(line_index);
  unsigned line_length = ref.line >= lines->length() - 1
                         ? ref.file->length() - lines->at(line_index)
                         : lines->at(line_index + 1) - lines->at(line_index);

  // Note that line_length includes \n.
  assert(ref.col <= line_length || ref.offset == ref.file->length());

  // Take off newline characters.
  while (line_length &&
         (lineptr[line_length - 1] == '\r' ||
          lineptr[line_length - 1] == '\n'))
  {
    line_length--;
  }

  std::string expanded = ExpandTabsInLine(lineptr, line_length);

  const char* line_print = expanded.c_str();

  // Recompute the column number if we expanded tabs.
  unsigned col = ref.col;
  if (expanded.size() != line_length) {
    for (unsigned i = 0; i < ref.col; i++) {
      if (lineptr[i] == '\t')
        col += 7;
    }
  }

  line_length = expanded.size();

  const char* prefix = "";
  const char* suffix = "";
  if (line_length > max_cols) {
    if (col > max_cols) {
      // Try to reposition everything so we're printing the desired column
      // in the middle of the line.
      static const unsigned max_chars = (max_cols - suffix_len - prefix_len);
      static const unsigned midpoint = max_chars / 2;
      static const unsigned delta = col - midpoint;

      line_print += delta;
      line_length = std::min(max_chars, line_length - delta);
      col = prefix_len + midpoint;

      prefix = long_prefix;
      suffix = long_suffix;
    } else {
      suffix = long_suffix;
      line_length = max_cols - suffix_len;
    }
  }

  {
    std::string line(line_print, line_length);
    fprintf(stderr, "%s%s%s\n", prefix, line.c_str(), suffix);
  }

  for (size_t i = 1; i < col; i++)
    fprintf(stderr, " ");
  fprintf(stderr, "^\n");
}

std::string
ReportManager::renderMessage(rmsg::Id id, const std::unique_ptr<TMessage::Arg>* args, size_t argc)
{
  const rmsg_info& info = GetMessageInfo(id);

  size_t last_insertion = 0;
  size_t text_length = strlen(info.text);

  AutoString builder = PrintableMessageType(info.type);
  builder = builder + ": ";
  for (size_t i = 0; i < text_length; i++) {
    if (info.text[i] == '%' && IsDigit(info.text[i + 1])) {
      unsigned argno = info.text[i + 1] - '0';
      if (argno >= argc) {
        builder = builder + "(!NO SUCH ARGUMENT!)";
        continue;
      }

      builder = builder + std::string(info.text + last_insertion, i - last_insertion);
      builder = builder + args[argno]->Render();

      last_insertion = i + 2;
    }
  }

  if (last_insertion < text_length)
    builder = builder + std::string(info.text + last_insertion, text_length - last_insertion);
  return std::string(builder.ptr());
}

std::string
ReportManager::renderSourceRef(const FullSourceRef& ref)
{
  if (!ref.file)
    return std::string(":0");

  AutoString builder = ref.file->path();
  builder = builder + ":" + AutoString(ref.line) + ":" + AutoString(ref.col);
  return std::string(builder.ptr());
}

void
ReportManager::printMessage(RefPtr<TMessage> message)
{
  TokenHistory history;
  source_->getTokenHistory(message->origin(), &history);

  AutoString line;
  if (history.files.empty())
    line = renderSourceRef(FullSourceRef());
  else
    line = renderSourceRef(history.files[0]);
  line = line + ": ";
  line = line + renderMessage(message->id(),
                              message->args().data(),
                              message->args().size());

  fprintf(stderr, "%s\n", line.ptr());

  if (!history.files.empty())
    printSourceLine(history.files[0]);

  for (size_t i = 0; i < history.macros.size(); i++) {
    FullSourceRef ref = source_->getOrigin(history.macros[i]);
    AutoString note = renderSourceRef(ref);
    note = note + ": ";

    Atom* name = history.macros[i].macro->name;
    std::unique_ptr<TMessage::Arg> arg(new TMessage::AtomArg(name));

    note = note + renderMessage(rmsg::from_macro, &arg, 1);

    fprintf(stderr, "%s\n", note.ptr());
    printSourceLine(ref);
  }

  for (size_t i = 1; i < history.files.size(); i++) {
    AutoString note = renderSourceRef(history.files[i]);
    note = note + ": ";
    note = note + renderMessage(rmsg::included_from, nullptr, 0);

    fprintf(stderr, "%s\n", note.ptr());
  }

  for (size_t i = 0; i < message->num_notes(); i++)
    printMessage(message->note(i));
}

std::string
TMessage::AtomArg::Render()
{
  return std::string(atom_->chars());
}
