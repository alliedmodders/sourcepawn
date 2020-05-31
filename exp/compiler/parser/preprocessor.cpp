// vim: set ts=2 sw=2 tw=99 et:
// 
// Copyright (C) 2012 David Anderson
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
#include "preprocessor.h"
#include "auto-string.h"
#include <time.h>
#ifdef WIN32
# include <windows.h>
#else
# include <sys/types.h>
# include <sys/stat.h>
# include <unistd.h>
#endif
#include <amtl/am-utility.h>
#include <amtl/am-vector.h>

using namespace ke;
using namespace sp;

static const size_t kMaxIncludeDepth = 50;

Preprocessor::Preprocessor(CompileContext& cc)
 : cc_(cc),
   options_(cc_.options()),
   keywords_(cc),
   tokens_(&normal_tokens_),
   allow_macro_expansion_(true),
   disable_includes_(false),
   include_depth_(0),
   comment_handler_(nullptr)
{
  setup_builtin_macros();
}

void
Preprocessor::setup_builtin_macros()
{
  define_builtin_int("__sourcepawn__", 2);
  define_builtin_int("__sourcepawn2__", 1);

  // __DATE__ and __TIME__
  {
    struct tm curtime;
    time_t td = time(nullptr);
#if defined(KE_WINDOWS)
    if (struct tm* rv = _localtime64(&td)) {
      curtime = *rv;
    } else {
      MemsetZero(&curtime);
    }
#else
    localtime_r(&td, &curtime);
#endif

    char datestring[64];
    char timestring[64];

#if defined(KE_EMSCRIPTEN)
    snprintf(datestring, sizeof(datestring),
             "%02d/%02d/%04d",
             curtime.tm_mon + 1, curtime.tm_mday, curtime.tm_year + 1900);
    snprintf(timestring, sizeof(timestring),
             "%02d:%02d:%02d",
             curtime.tm_hour, curtime.tm_min, curtime.tm_sec);
#else
    strftime(datestring, sizeof(datestring), "%m/%d/%Y", &curtime);
    strftime(timestring, sizeof(timestring), "%H:%M:%S", &curtime);
#endif

    define_builtin_string("__DATE__", datestring);
    define_builtin_string("__TIME__", timestring);
  }
}

void
Preprocessor::define_builtin_int(const char* name, int64_t value)
{
  Atom* id = cc_.add(name);
  AtomMap<Macro*>::Insert p = macros_.findForAdd(id);
  assert(!p.found());

  TokenList* tokens = new (cc_.pool()) TokenList(1);
  tokens->at(0) = Token(TOK_INTEGER_LITERAL);
  tokens->at(0).setIntValue(value);

  macros_.add(p, id, new (cc_.pool()) Macro(SourceLocation(), id, tokens));
}

void
Preprocessor::define_builtin_string(const char* name, const char* str)
{
  Atom* id = cc_.add(name);
  AtomMap<Macro*>::Insert p = macros_.findForAdd(id);
  assert(!p.found());

  TokenList* tokens = new (cc_.pool()) TokenList(1);
  tokens->at(0) = Token(TOK_STRING_LITERAL);
  tokens->at(0).setAtom(cc_.add(str));

  macros_.add(p, id, new (cc_.pool()) Macro(SourceLocation(), id, tokens));
}

bool
Preprocessor::enter(RefPtr<SourceFile> file)
{
  LREntry tr = cc_.source().trackFile(SourceLocation(), file);
  if (!tr.valid())
    return false;

  tokens_ = &normal_tokens_;
  tokens_->reset();
  lexer_stack_.clear();
  macro_lexer_ = nullptr;
  next_deprecation_message_ = "";
  allow_macro_expansion_ = true;
  lex_options_.RequireNewdecls = options_.RequireNewdecls;

  lexer_ = new Lexer(cc_, *this, lex_options_, file, tr);
  return true;
}

TokenKind
Preprocessor::next()
{
  if (Token* buffered = tokens_->maybePop())
    return buffered->kind;
  return scan();
}

TokenKind
Preprocessor::peek()
{
  if (tokens_->pushed())
    return tokens_->peek()->kind;

  TokenKind kind = scan();
  tokens_->push();
  return kind;
}

TokenKind
Preprocessor::scan()
{
  Token tok;
  TokenKind kind;
  do {
    assert(!!macro_lexer_ != !!lexer_);

    // :TODO: we can't lex directly into the ring (?!) because the ring might
    // change. We need to analyze and fix this for performance.
    kind = macro_lexer_
           ? macro_lexer_->next(&tok)
           : lexer_->next(&tok);

    // Parent lexer should filter out comments.
    assert(kind != TOK_COMMENT);
  } while (kind == TOK_NONE);

  *tokens_->moveNext() = tok;
  return kind;
}

bool
Preprocessor::match(TokenKind tok)
{
  if (tokens_->pushed()) {
    if (tokens_->peek()->kind == tok) {
      tokens_->pop();
      return true;
    }
    return false;
  }

  if (scan() == tok)
    return true;

  tokens_->push();
  return false;
}

bool
Preprocessor::expect(TokenKind kind)
{
  TokenKind got = next();
  if (got == kind)
    return true;

  // SP1 unlexed the token here. We do not do that in SP2, reasoning that it's
  // better to always make forward progress.
  cc_.report(tokens_->current()->start.loc, rmsg::wrong_token)
    << TokenNames[kind]
    << TokenNames[got];
  return false;
}

TokenKind
Preprocessor::peekTokenSameLine()
{
  // We should not call this without having parsed at least one token.
  assert(tokens_->numTokens() > 0);

  // We should never be calling this while in a directive.
  assert(tokens_ == &normal_tokens_);

  // Make sure we've lexed at least one token into the future.
  if (!tokens_->pushed()) {
    scan();
    undo();
  }

  const Token* cur = current();
  const Token* next = tokens_->peek();

  // If both tokens are in the same source file, this becomes very easy.
  if (cur->source_id == next->source_id) {
    return (cur->end.line == next->start.line && next->kind != TOK_EOF)
           ? next->kind
           : TOK_EOL;
  }

  // Otherwise, we need to tell whether the insertion points are on the same
  // line. This is very slow.
  if (cc_.source().areLocationsInsertedOnSameLine(cur->end.loc, next->start.loc))
    return next->kind;
  return TOK_EOL;
}

static inline bool
FileExists(const AutoString& path)
{
#if defined(KE_WINDOWS)
  DWORD type = GetFileAttributesA(path);
  if (type == INVALID_FILE_ATTRIBUTES)
    return false;
  return !(type & FILE_ATTRIBUTE_DIRECTORY);
#else
  struct stat st;
  if (stat(path, &st) == -1)
    return false;
  return S_ISREG(st.st_mode);
#endif
}

static inline bool
IsPathSeparator(char c)
{
#if defined(KE_WINDOWS)
  if (c == '\\')
    return true;
#endif
  return c == '/';
}

AutoString
Preprocessor::searchPaths(const char* file, const char* where)
{
  // If the path is absolute, we perform no searching.
#if defined(KE_WINDOWS)
  if (((file[0] >= 'A' && file[0] <= 'Z') || (file[0] >= 'a' && file[0] <= 'z')) &&
      file[1] == ':' &&
      file[2] == '\\' &&
      FileExists(file))
  {
    return file;
  }
#endif
  if (IsPathSeparator(file[0]) && FileExists(file))
    return file;

  if (where) {
    // Find the position of the last path separator in the current file context.
#if defined(KE_WINDOWS)
    const char* e1 = strrchr(where, '\\');
    const char* e2 = strrchr(where, '/');
    
    const char* end;
    if ((e1 && e2 && (e1 > e2)) || (e1 && !e2))
      end = e1;
    else if ((e1 && e2 && (e1 < e2)) || (e2 && !e1))
      end = e2;
    else
      end = NULL;
#else
    const char* end = strrchr(where, '/');
#endif

    if (!end) {
      // Just search in the current working directory.
      if (FileExists(file))
        return file;
    } else {
      AutoString path = AutoString(where, end - where + 1) + file;
      if (FileExists(path))
        return path;
    }
  }

  for (size_t i = 0; i < options_.SearchPaths.size(); i++) {
    AutoString search = AutoString(options_.SearchPaths[i]) + file;
    if (FileExists(search))
      return search;
  }

  return AutoString();
}

bool
Preprocessor::enterFile(TokenKind directive,
                        const SourceLocation& from,
                        const char* file,
                        const char* where)
{
  if (disable_includes_)
    return false;

  AutoString path = searchPaths(file, where);
  if (!path.length()) {
    // Try to push_back '.inc'.
    size_t len = strlen(file);
    if (len < 4 || strcmp(&file[len - 4], ".inc") != 0) {
      AutoString new_file = AutoString(file) + ".inc";
      path = searchPaths(new_file, where);
    }
  }

  if (!path.length()) {
    if (directive == TOK_M_TRYINCLUDE)
      return true;

    cc_.report(from, rmsg::include_not_found)
      << file;
    return false;
  }

  ReportingContext rc(cc_, from);
  RefPtr<SourceFile> new_file = cc_.source().open(rc, path.ptr());
  if (!new_file)
    return false;

  LREntry tl = cc_.source().trackFile(from, new_file);
  if (!tl.valid()) {
    // Error was already reported.
    return false;
  }

  if (include_depth_ >= kMaxIncludeDepth) {
    cc_.report(from, rmsg::include_depth_exceeded);
    return false;
  }

  include_depth_++;

  assert(!macro_lexer_ && lexer_);
  lexer_stack_.push_back(SavedLexer(lexer_, nullptr));
  lexer_ = new Lexer(cc_, *this, lexer_->options(), new_file, tl);
  return true;
}

void
Preprocessor::defineMacro(Atom* name, const SourceLocation& nameLoc, TokenList* tokens)
{
  AtomMap<Macro*>::Insert p = macros_.findForAdd(name);
  if (p.found()) {
    if (p->value->definedAt.isSet()) {
      cc_.report(nameLoc, rmsg::macro_redefinition)
        << name
        << (cc_.note(p->value->definedAt, rmsg::previous_location));
    } else {
      cc_.report(nameLoc, rmsg::builtin_macro_redefinition)
        << name;
    }
    return;
  }

  Macro* macro = new (cc_.pool()) Macro(nameLoc, name, tokens);
  macros_.add(p, name, macro);
}

bool
Preprocessor::removeMacro(const SourceLocation& loc, Atom* name)
{
  AtomMap<Macro*>::Result p = macros_.find(name);
  if (!p.found()) {
    cc_.report(loc, rmsg::macro_not_found)
      << name;
    return false;
  }
  if (!p->value->definedAt.isSet()) {
    cc_.report(loc, rmsg::cannot_undef_builtin)
      << name;
    return false;
  }

  macros_.remove(p);
  return true;
}

bool
Preprocessor::enterMacro(const SourceLocation& nameLoc, Atom* name)
{
  if (!allow_macro_expansion_)
    return false;

  AtomMap<Macro*>::Result p = macros_.find(name);
  if (!p.found())
    return false;

  Macro* macro = p->value;
  
  // If this macro is active, then we're currently expanding it, and we must make sure not to
  // expand it again and potentially infinitely recurse.
  if (macro->active)
    return false;

  // If the macro has no tokens, don't bother doing anything. We still return
  // so that we do not lex this as an identifier.
  if (macro->tokens->size() == 0)
    return true;

  LREntry range = cc_.source().trackMacro(nameLoc, macro);
  if (!range.valid()) {
    // Error was already reported.
    return true;
  }

  // We cache macro lexers since we expect to see more macros than we do
  // #includes. This may be an overoptimization given that we're discouraging
  // macros in SP2.
  RefPtr<MacroLexer> macro_lexer;
  if (recycled_macro_lexers_.empty()) {
    macro_lexer = new MacroLexer(cc_, *this, macro, range);
  } else {
    macro_lexer = ke::PopBack(&recycled_macro_lexers_);
    macro_lexer->Reuse(macro, range);
  }

  lexer_stack_.push_back(SavedLexer(lexer_, macro_lexer_));
  lexer_ = nullptr;
  macro_lexer_ = macro_lexer;
  return true;
}

bool
Preprocessor::handleEndOfFile()
{
  // We should never receive EOF in a directive.
  assert(!lexer_ || !lexer_->processingDirective());

  if (lexer_stack_.empty()) {
    // We're in a topmost file, so return an EOF.
    assert(lexer_ && !macro_lexer_);
    return false;
  }

  if (macro_lexer_) {
    recycled_macro_lexers_.push_back(macro_lexer_);
  } else if (lexer_) {
    assert(include_depth_ > 0);
    include_depth_--;
  }

  {
    SavedLexer& saved = lexer_stack_.back();
    lexer_ = saved.lexer;
    macro_lexer_ = saved.macro_lexer;
    lexer_stack_.pop_back();
  }

  assert(!!lexer_ != !!macro_lexer_);
  return true;
}

void
Preprocessor::addComment(CommentPos where, const SourceRange& extends)
{
  if (comment_handler_)
    comment_handler_->HandleComment(where, extends);
}

void
Preprocessor::eatRestOfLine()
{
  while (true) {
    TokenKind tok = peekTokenSameLine();
    if (tok == TOK_EOL || tok == TOK_EOF)
      return;
    next();
  }
}

void
Preprocessor::skipUntil(TokenKind kind, SkipFlags flags, TokenKind opener)
{
  TokenKind tok = ((flags & SkipFlags::StartAtCurrent) == SkipFlags::StartAtCurrent)
                  ? current()->kind
                  : peek();

  if (tok == kind && ((flags & SkipFlags::StopBeforeMatch) == SkipFlags::StopBeforeMatch))
    return;

  while (tok != TOK_EOF) {
    if ((flags & SkipFlags::StopAtLine) == SkipFlags::StopAtLine) {
      if (peekTokenSameLine() == TOK_EOL)
        return;
      if (match(TOK_SEMICOLON))
        return;
    }

    if (tok == kind)
      break;
    if ((flags & SkipFlags::StopAtSemi) == SkipFlags::StopAtSemi) {
      if (tok == TOK_SEMICOLON)
        break;
    }
    if (kind == opener)
      break;

    next();
    tok = peek();
  }

  if ((flags & SkipFlags::StopBeforeMatch) != SkipFlags::StopBeforeMatch)
    next();
}
