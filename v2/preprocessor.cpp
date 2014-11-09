/* vim: set ts=2 sw=2 tw=99 et:
 *
 * Copyright (C) 2012 David Anderson
 *
 * This file is part of SourcePawn.
 *
 * SourcePawn is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free
 * Software Foundation, either version 3 of the License, or (at your option)
 * any later version.
 * 
 * SourcePawn is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * SourcePawn. If not, see http://www.gnu.org/licenses/.
 */
#include "preprocessor.h"
#include <am-vector.h>
#include <ctype.h>
#include <assert.h>
#include <time.h>
#ifdef WIN32
# include <windows.h>
#else
# include <sys/types.h>
# include <sys/stat.h>
# include <unistd.h>
#endif

using namespace ke;

Preprocessor::Preprocessor(CompileContext &cc)
: cc_(cc),
  text_(NULL),
  macros_(SystemAllocatorPolicy()),
  macroStrings_()
{
  macros_.init(16);
  macroStrings_.init();
}

Preprocessor::~Preprocessor()
{
  if (text_)
    delete text_;

  assert(lexers_.empty());
}

void
Preprocessor::preprocess(TranslationUnit *unit)
{
  setup_global_macros();
  preprocess(unit, unit->text(), unit->length());
  unit->givePreprocessedText(buffer_.finish(), buffer_.length(), files_);
}

void
Preprocessor::setup_global_macros()
{
  // __sourcepawn2__ == 1
  {
    Atom *sp2_id = cc_.add("__sourcepawn2__");
    MacroTable::Insert p = macros_.findForAdd(sp2_id);
    macros_.add(p, MacroEntry(sp2_id, (char *)"1", 1, nullptr, SourceLocation()));
  }

  // __DATE__ and __TIME__
  {
    struct tm curtime;
    time_t td = time(nullptr);
    localtime_r(&td, &curtime);

#if defined EMSCRIPTEN
    snprintf(date_, sizeof(date_), "\"%02d/%02d/%04d\"", curtime->tm_mon + 1, curtime->tm_mday, curtime->tm_year + 1900);
    snprintf(ltime_, sizeof(ltime_), "\"%02d:%02d:%02d\"", curtime->tm_hour, curtime->tm_min, curtime->tm_sec);
#else
    strftime(date_, sizeof(date_), "\"%m/%d/%Y\"", &curtime);
    strftime(ltime_, sizeof(ltime_), "\"%H:%M:%S\"", &curtime);
#endif

    Atom *date_atom = cc_.add("__DATE__");
    {
      MacroTable::Insert p = macros_.findForAdd(date_atom);
      macros_.add(p, MacroEntry(date_atom, date_, strlen(date_), nullptr, SourceLocation()));
    }

    Atom *time_atom = cc_.add("__TIME__");
    {
      MacroTable::Insert p = macros_.findForAdd(time_atom);
      macros_.add(p, MacroEntry(time_atom, ltime_, strlen(ltime_), nullptr, SourceLocation()));
    }
  }
}

class CommentStripper : public TextProcessor
{
 public:
  CommentStripper(CompileContext &cc, FileContext *file, char *text, size_t length)
  : TextProcessor(text, length),
    cc_(cc),
    file_(file)
  {
  }

  void strip();

 private:
  CompileContext &cc_;
  FileContext *file_;
};

void
CommentStripper::strip()
{
  while (more()) {
    char c = read();

    switch (c) {
     case '/':
     {
      c = read();

      // Rewind back to the '/'
      pos_ -= 2;

      // C++-style comments (// x)
      if (c == '/') {
        while (!IsLineTerminator(get()))
          *pos_++ = ' ';
        break;
      }

      // C-style comments (/* x */)
      if (c == '*') {
        SourceLocation begin(file_, pos());
        *pos_++ = ' ';
        *pos_++ = ' ';

        // Erase until we see '*' and '/'
        for (;;) {
          c = read();
          if (IsLineTerminator(c)) {
            // Note we don't compute nextline here, since we want to keep the
            // original line number.
            if (c == '\0') {
              cc_.reportError(begin, Message_UnterminatedComment);
              return;
            }
            nextline(c);
            continue;
          }

          *(pos_ - 1) = ' ';
          if (c == '*') {
            if (get() == '/') {
              *pos_++ = ' ';
              break;
            }
          }
        }
      }

      // We're at the '/', so go forward once.
      pos_++;
      break;
     }

     case '"':
     case '\'':
     {
       char match = c;
       do {
         if ((c = read()) == match)
           break;
         if (c == '\\')
           read();
         // :TODO: handle \ newline
       } while (c != '\0');
       break;
     }

     case '\n':
     case '\r':
      nextline(c);
      break;
    }
  }
}

PreprocessingLexer::PreprocessingLexer(CompileContext &cc, FileContext *file, char *text, size_t length)
: BasicLexer(cc, file, text, length),
  pushed_(false)
{
}

TokenKind
PreprocessingLexer::scan()
{
  begin_ = pos_;

  for (;;) {
    char c = read();
    switch (c) {
      case ' ':
      case '\t':
        break;

      case '\n':
      case '\r':
        pos_--;
        return TOK_NEWLINE;

      case '\0':
        return TOK_EOF;

      case '*':
        return TOK_STAR;
 
      case '+':
        return TOK_PLUS;
 
      case '&':
        c = read();
        if (c == '&')
          return TOK_AND;
        pos_--;
        return TOK_BITAND;
 
      case '|':
        c = read();
        if (c == '|')
          return TOK_OR;
        pos_--;
        return TOK_BITOR;
 
      case '^':
        c = read();
        pos_--;
        return TOK_BITXOR;
 
      case '%':
        c = read();
        pos_--;
        return TOK_PERCENT;
 
      case '-':
        c = read();
        if (c == '-')
          return TOK_DECREMENT;
        pos_--;
        return TOK_MINUS;
 
      case '!':
        c = read();
        pos_--;
        return TOK_NOT;
 
      case '=':
        c = read();
        if (c == '=')
          return TOK_EQUALS;
        cc_.reportError(begin(), Message_UnexpectedCharacter, c, uint8_t(c));
        return TOK_ERROR;
 
      case '<':
        c = read();
        if (c == '=')
          return TOK_LE;
        if (c == '<') {
          c = read();
          pos_--;
          return TOK_SHL;
        }
        pos_--;
        return TOK_LT;

      case '>':
        c = read();
        if (c == '=')
          return TOK_GE;
        if (c == '>') {
          c = read();
          if (c == '>') {
            c = read();
            pos_--;
            return TOK_USHR;
          }
          return TOK_SHR;
        }
        pos_--;
        return TOK_GT;

      case '(': return TOK_LPAREN;
      case ')': return TOK_RPAREN;
      case '~': return TOK_TILDE;
      case '?': return TOK_QMARK;
      case ':': return TOK_COLON;
  
      default:
      {
        if (c >= '0' && c <= '9')
          return numberLiteral(c);
        if (IsIdentStart(c)) {
          literal_.clear();
          name(c);
          if (strcmp(literal(), "defined") == 0)
            return TOK_DEFINED;
          return TOK_NAME;
        }
        cc_.reportError(begin(), Message_UnexpectedCharacter, c, uint8_t(c));
        return TOK_ERROR;
      }
    }
  }
}

TokenKind
PreprocessingLexer::lex()
{
  if (pushed_) {
    pushed_ = false;
    return saved_;
  }
  return scan();
}

TokenKind
PreprocessingLexer::peek()
{
  if (pushed_)
    return saved_;
  pushed_ = true;
  saved_ = scan();
  return saved_;
}

bool
PreprocessingLexer::expect(TokenKind kind)
{
  if (!match(kind)) {
    cc_.reportError(begin(), Message_BadDirectiveToken, TokenNames[kind]);
    return false;
  }
  return true;
}

TokenKind
PreprocessingLexer::command()
{
  begin_ = linebegin_;

  char c = firstNonSpaceChar();
  if (c != '#') {
    pos_--;
    return TOK_NONE;
  }

  literal_.clear();
  literal_.append('#');

  c = read();
  if (!IsIdentStart(c)) {
    cc_.reportError(loc(), Message_BadDirectiveName);
    return TOK_ERROR;
  }

  return identifier(c);
}

void
PreprocessingLexer::readline()
{
  // Throw out any old tokens.
  pushed_ = false;
  while (true) {
    char c = read();
    if (IsLineTerminator(c)) {
      nextline(c);
      break;
    }
  }
}

bool
PreprocessingLexer::readEndOfDirective()
{
  bool onlyspaces = true;
  if (pushed_) {
    pushed_ = false;
    if (saved_ == TOK_NEWLINE || saved_ == TOK_EOF) {
      if (saved_ == TOK_NEWLINE) {
        // Hack - if we lex a newline, we stop right before, not after. We
        // could probably get away with just advancing the line.
        assert(IsLineTerminator(get()));
        nextline(read());
      }
      return true;
    }
    onlyspaces = false;
  }

  while (true) {
    char c = read();
    if (IsLineTerminator(c)) {
      nextline(c);
      break;
    }
    onlyspaces &= IsSkipSpace(c);
  }
  return onlyspaces;
}

void
PreprocessingLexer::readUntilEnd(char **beginp, char **endp)
{
  assert(!pushed_);
  assert(saved_streams_.empty());

  // First valid character position.
  char *begin = skipSpaces();

  // Find the line terminator.
  while (!IsLineTerminator(get()))
    read();

  // Narrow until we've eliminated trailing spaces.
  char *end = ptr();
  while (end > begin) {
    if (!IsSkipSpace(*end) && !IsLineTerminator(*end))
      break;
    end--;
  }

  *beginp = begin;
  *endp = end + 1;
}

TokenKind
PreprocessingLexer::readUntilName(CompileBuffer &buffer)
{
  for (;;) {
    char c = read();
    if (IsLineTerminator(c)) {
      nextline(c);
      return TOK_NONE;
    }

    if (IsIdentStart(c)) {
      literal_.clear();
      literal_.append(c);
      for (;;) {
        c = read();
        if (!IsIdentChar(c)) {
          pos_--;
          break;
        }
        literal_.append(c);
      }
      literal_.append('\0');
      return TOK_NAME;
    }

    buffer.append(c);

    // Don't substitute inside string literals.
    if (c == '"' || c == '\'') {
      char match = c;
      while (true) {
        c = read();
        if (IsLineTerminator(c))
          break;
        buffer.append(c);
        if (c == match)
          break;
        if (c == '\\') {
          c = read();
          if (IsLineTerminator(c)) {
            // :TODO:
            abort();
          }
          buffer.append(c);
        }
      }
    }
  }
}

struct Op
{
  TokenKind tok;
  int order;
  int prec;
  int arity;
};

static Op Operators[] = {
#define _(name, token, order, prec, arity)  \
  { TOK_##token, order, prec, arity },
  OPERATOR_MAP(_)
#undef _
  { TOK_NONE, -1, -1, -1 }
};

static inline const Op *
FindOperator(TokenKind tok)
{
  switch (tok) {
#define _(name, token, order, prec, arity)  \
    case TOK_##token:                       \
      return &Operators[Operator_##name];   \
      break;
    OPERATOR_MAP(_)
#undef _
    default:
      return NULL;
  }
}

static int
BinaryEval(TokenKind tok, int left, int right)
{
  // :TODO:
  fprintf(stderr, "no\n");
  abort();
  return 0;
}

bool
Preprocessor::unary(int *val)
{
  TokenKind tok = text_->lex();
  SourceLocation pos = text_->begin();

  switch (tok) {
    case TOK_MINUS:
      if (!unary(val))
        return false;
      *val = -*val;
      return true;
 
    case TOK_NOT:
      if (!unary(val))
        return false;
      *val = !*val;
      return true;
 
    case TOK_TILDE:
      if (!unary(val))
        return false;
      *val = ~*val;
      return true;

    case TOK_LPAREN:
    {
      if (!expr(val))
        return false;
      return text_->expect(TOK_RPAREN);
    }

    case TOK_DEFINED:
    {
      if (!text_->expect(TOK_NAME))
        return false;
      Atom *id = text_->name();
      MacroTable::Result r = macros_.find(id);
      *val = r.found() ? 1 : 0;
      return true;
    }

    case TOK_INTEGER_LITERAL:
    {
      *val = StringToInt32(text_->literal());
      return true;
    }

    case TOK_NAME:
    {
      Atom *id = text_->name();
      MacroTable::Result r = macros_.find(id);
      if (!r.found()) {
        cc_.reportError(pos, Message_MacroNotFound, text_->literal());
        return false;
      }
      text_->addText(r->value, r->valueLength);
      return true;
    }
  }

  cc_.reportError(pos, Message_ExpectedExpression, TokenNames[tok]);
  return false;
}

bool
Preprocessor::binary(int prec, int *val)
{
  assert(prec <= 12);
  assert(prec >= 2);

  int left;
  bool r = (prec >= 2)
           ? unary(&left)
           : binary(prec - 1, &left);
  if (!r)
    return false;

  while (true) {
    TokenKind tok = text_->lex();
    const Op *op = FindOperator(tok);
    if (!op || op->prec != prec)
      break;
    assert(op->arity == 2);
    int right;
    bool r = (prec >= 2)
             ? unary(&left)
             : binary(prec - 1, &right);
    if (!r)
      return false;
    left = BinaryEval(tok, left, right);
  }

  *val = left;
  return true;
}

bool
Preprocessor::expr(int *val)
{
  int e;
  if (!binary(12, &e))
    return false;

  if (!text_->match(TOK_QMARK)) {
    *val = e;
    return true;
  }

  int left, right;
  if (!expr(&left))
    return false;
  if (!text_->expect(TOK_COLON))
    return false;
  if (!expr(&right))
    return false;

  *val = e ? left : right;
  return true;
}

void
Preprocessor::substitute()
{
  assert(!text_->stacked());

  Atom *initial = nullptr;

  while (true) {
    // "stacked" means we are lexing substitutions and not the primary stream.
    bool stacked = text_->stacked();

    // Find an id to try and substitute.
    TokenKind tok = text_->readUntilName(buffer_);
    if (tok != TOK_NAME)
      break;

    Atom *id = text_->name();
    MacroTable::Result r = macros_.find(id);

    // If the id is equal to the initial id, to avoid infinite recursion, we
    // don't perform a substitution.
    if (!r.found() || id == initial) {
      buffer_.append(text_->literal(), text_->literal_length());
      continue;
    }

    // Remember the initial identifier we lexed, if we're starting a new
    // substitution.
    if (!stacked)
      initial = id;

    // "Substitute" the macro value. The substitution occurs by injecting the
    // macro string into the lexer's stream, tricking us into either copying
    // it into the buffer or continuing subsitutions.
    text_->addText(r->value, r->valueLength);
  }

  buffer_.append('\n');
}

static inline bool
FileExists(const AutoString &path)
{
#ifdef WIN32
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
#ifdef WIN32
  if (c == '\\')
    return true;
#endif
  return c == '/';
}

AutoString
Preprocessor::searchPaths(const char *file)
{
  // If the path is absolute, we perform no searching.
#if defined WIN32
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

  // Find the position of the last path separator in the current file context.
  const char *curpath = text_->file()->path();
#ifdef WIN32
  const char *e1 = strrchr(curpath, '\\');
  const char *e2 = strrchr(curpath, '/');
  
  const char *end;
  if ((e1 && e2 && (e1 > e2)) || (e1 && !e2))
    end = e1;
  else if ((e1 && e2 && (e1 < e2)) || (e2 && !e1))
    end = e2;
  else
    end = NULL;
#else
  const char *end = strrchr(curpath, '/');
#endif

  if (!end) {
    // Just search in the current working directory.
    if (FileExists(file))
      return file;
  } else {
    AutoString path = AutoString(curpath, end - curpath + 1) + file;
    if (FileExists(path))
      return path;
  }

  for (size_t i = 0; i < cc_.numSearchPaths(); i++) {
    AutoString search = cc_.getSearchPath(i) + file;
    if (FileExists(search))
      return search;
  }

  return AutoString();
}

bool
Preprocessor::include(TokenKind cmd, const char *file)
{
  AutoString path = searchPaths(file);
  
  if (!path.length()) {
    // Try to append '.inc'.
    size_t len = strlen(file);
    if (len < 4 || strcmp(&file[len - 4], ".inc") != 0) {
      AutoString new_file = AutoString(file) + ".inc";
      path = searchPaths(new_file);
    }
  }

  if (!path.length()) {
    if (cmd == TOK_M_TRYINCLUDE)
      return true;

    cc_.reportError(text_->begin(), Message_IncludeNotFound, file);
    return false;
  }

  char *text;
  size_t length;
  if (!ReadFileChars(path, &text, &length)) {
    cc_.reportError(text_->begin(), Message_ErrorReadingInclude, path.ptr());
    return false;
  }

  // Create a new file context.
  FileContext *fx = new FileContext(text_->file(), path);
  size_t id = files_.length();
  files_.append(fx);

  // Strip comments.
  {
    CommentStripper stripper(cc_, fx, text, length);
    stripper.strip();
  }

  // Write a directive to the output buffer that we are switching to a new
  // compile context. The sprintf is safe since we are not writing a huge
  // string.
  char line[256];
  sprintf(line, "#file %" KE_FMT_SIZET " // ", id);
  buffer_.append(line);
  buffer_.append(path, path.length());
  buffer_.append('\n');

  // Swap in a new lexer.
  lexers_.append(text_);
  text_ = new PreprocessingLexer(cc_, fx, text, length);

  // Run the preprocessor.
  preprocess();

  // If we still have a lexer left over, destroy it.
  delete text_;

  // Restore the previous lexer.
  text_ = lexers_.popCopy();

  // Write a directive to the output buffer indicating that we're leaving the
  // current file, so it can restore any line number information.
  buffer_.append("#leaving\n");
  return true;
}

bool
Preprocessor::directive(TokenKind cmd)
{
  switch (cmd) {
    case TOK_M_DEFINE:
    {
      TokenKind tok = text_->lex();
      if (tok != TOK_NAME) {
        cc_.reportError(text_->begin(), Message_BadDirectiveToken, TokenNames[TOK_NAME]);
        return false;
      }
      Atom *id = text_->name();
      MacroTable::Insert p = macros_.findForAdd(id);
      if (p.found()) {
        if (p->file) {
          cc_.reportError(text_->begin(), Message_MacroRedefinition, id->chars(), p->file->path(), p->loc.line);
        } else {
          cc_.reportError(text_->begin(), Message_BuiltinMacroRedefinition, id->chars());
        }
        return false;
      }

      char *ptr, *end;
      text_->readUntilEnd(&ptr, &end);

      // If we're in a temporary file buffer, allocate from a local string
      // pool instead, since temporary file buffers are freed eagerly.
      if (!lexers_.empty()) {
        size_t len = end - ptr;

        // The const cast is okay here. Nothing is modified.
        ptr = const_cast<char *>(macroStrings_.add(ptr, len)->chars());
        end = ptr + len;
      }

      // Even though the text processors take a char *, nothing is modified
      // past the comment stripping phase.
      macros_.add(p, MacroEntry(id, ptr, end - ptr, text_->file(), text_->begin()));
      break;
    }

    case TOK_M_ELSE:
    {
      if (text_->ifstate() == IfNone) {
        cc_.reportError(text_->begin(), Message_ElseWithoutIf);
        return false;
      }
      if (text_->got_else()) {
        cc_.reportError(text_->begin(), Message_ElseDeclaredTwice, text_->ifpos().line);
        return false;
      }

      text_->set_in_else();
      break;
    }

    case TOK_M_ENDIF:
    {
      if (text_->ifstate() == IfNone) {
        cc_.reportError(text_->begin(), Message_EndIfWithoutIf);
        return false;
      }

      text_->popif();
      break;
    }

    case TOK_M_ENDINPUT:
    {
      if (!text_->readEndOfDirective())
        cc_.reportError(text_->loc(), Message_ExtraCharactersAfterDirective);

      // Simulate the file being magically ended. SP1 doesn't report mismatched
      // #if declarations in nested files, but we do, so we also purge the if
      // stack.
      text_->endinput();
      text_->purgeIfStack();
      break;
    }

    case TOK_M_IF:
    {
      SourcePosition pos = text_->pos();
      int val;
      if (!expr(&val))
        val = 0;
      text_->pushif(pos, val ? IfReading : IfIgnoring);
      break;
    }

    case TOK_M_INCLUDE:
    case TOK_M_TRYINCLUDE:
    {
      if (lexers_.length() >= 50) {
        cc_.reportError(text_->loc(), Message_TooMuchIncludeNesting);
        return false;
      }

      // Search for a delimiter.
      char c = text_->firstNonSpaceChar();
      if (c != '"' && c != '<') {
        cc_.reportError(text_->loc(), Message_BadIncludeSyntax);
        return false;
      }

      char match = (c == '"') ? '"' : '>';

      command_.clear();
      while (true) {
        char c = text_->read();
        if (TextProcessor::IsLineTerminator(c)) {
          cc_.reportError(text_->loc(), Message_BadIncludeSyntax);
          return false;
        }
        if (c == match)
          break;
        command_.append(c);
      }
      command_.append('\0');

      return include(cmd, command_.buffer());
    }

    case TOK_M_PRAGMA:
    {
      if (!text_->expect(TOK_NAME))
        return false;
      if (strcmp(text_->literal(), "deprecated") == 0) {
        char *begin, *end;
        text_->readUntilEnd(&begin, &end);

        // The original Pawn compiler interleaves preprocessing and parsing,
        // and #pragma deprecated sets global state instead of specifying an
        // identifier (like MSVC). So, at this point in the stream, we are
        // forced to leave a breadcrub in the output buffer.
        buffer_.append("#deprecate ");
        buffer_.append(begin, end - begin);
      } else if (strcmp(text_->literal(), "newdecls") == 0) {
        SourceLocation loc = text_->loc();

        char *begin, *end;
        text_->readUntilEnd(&begin, &end);

        AString str(begin, end - begin);

        // The semicolon directive also affects parser state, so we inject it
        // into the output buffer.
        if (str.compare("required") == 0)
          buffer_.append("#require_newdecls\n");
        else if (str.compare("optional") == 0)
          buffer_.append("#optional_newdecls\n");
        else
          cc_.reportError(loc, Message_InvalidNewDeclState);
      } else if (strcmp(text_->literal(), "semicolon") == 0) {
        // The semicolon directive also affects parser state, so we inject it
        // into the output buffer.
        int val;
        if (!expr(&val))
          return false;
        if (val)
          buffer_.append("#require_semicolons\n");
        else
          buffer_.append("#optional_semicolons\n");
      } else {
        cc_.reportError(text_->loc(), Message_UnknownPragma, text_->literal());
        return false;
      }
      break;
    }

    case TOK_M_UNDEF:
    {
      if (!text_->expect(TOK_NAME))
        return false;
      Atom *id = text_->name();
      MacroTable::Result r = macros_.find(id);
      if (!r.found()) {
        cc_.reportError(text_->begin(), Message_MacroNotFound, text_->literal());
        return false;
      }
      if (!r->file) {
        cc_.reportError(text_->begin(), Message_UndefBuiltinMacro, text_->literal());
        return false;
      }
      macros_.remove(r);
      break;
    }

    default:
    {
      cc_.reportError(text_->loc(), Message_UnknownDirective, text_->literal());
      return false;
    }
  }

  buffer_.append('\n');
  return true;
}

void
Preprocessor::preprocess(FileContext *file, char *text, size_t length)
{
  {
    CommentStripper stripper(cc_, file, text, length);
    stripper.strip();
  }

  if (text_)
    lexers_.append(text_);

  text_ = new PreprocessingLexer(cc_, file, text, length);
  preprocess();
}

// Some commands we can't ignore, for example - the #else after an #if 0, we
// handle here.
void
Preprocessor::check_ignored_command(TokenKind cmd)
{
  switch (cmd) {
    case TOK_M_IF:
      text_->pushif(text_->pos(), IfMustIgnore);
      break;

    case TOK_M_ELSE:
      if (text_->got_else())
        cc_.reportError(text_->begin(), Message_ElseDeclaredTwice, text_->ifpos().line);
      text_->set_in_else();
      break;
  }
}

void
Preprocessor::preprocess()
{
  while (text_->more()) {
    TokenKind cmd = text_->command();

    if (text_->ifstate() >= IfIgnoring) {
      if (cmd != TOK_M_ENDIF) {
        check_ignored_command(cmd);
        text_->readline();
        buffer_.append('\n');
        continue;
      }
    }

    if (cmd == TOK_NONE) {
      text_->resetToLineStart();
      substitute();
      continue;
    }

    bool success = directive(cmd);

    if (!text_) {
      // Surprise! We're done early and the lexer has been stolen from us.
      return;
    }

    // Check for excess characters. Don't double error if we didn't recognize
    // the directive.
    if (!text_->readEndOfDirective() && success)
      cc_.reportError(text_->loc(), Message_ExtraCharactersAfterDirective);
  }

  if (text_->ifstate() != IfNone)
    cc_.reportError(text_->ifpos(), Message_UnterminatedIf);
}

