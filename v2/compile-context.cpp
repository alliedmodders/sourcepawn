// vim: set sts=2 ts=8 sw=2 tw=99 et:
//
// Copyright (C) 2012-2014 David Anderson
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
#include "compile-context.h"
#include "compile-phases.h"
#include "preprocessor.h"
#include "parser.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

using namespace ke;

ThreadLocal<CompileContext *> ke::CurrentCompileContext;

bool
ke::ReadFileChars(const char *path, char **textp, size_t *lengthp)
{
  FILE *fp = fopen(path, "rb");
  if (!fp)
    return false;

  if (fseek(fp, 0, SEEK_END) == -1) {
    fclose(fp);
    return false;
  }

  long size = ftell(fp);
  if (size == -1 || fseek(fp, 0, SEEK_SET) == -1) {
    fclose(fp);
    return false;
  }

  char *buffer = (char *)calloc(size, 1);
  if (!buffer)
    return false;

  if (fread(buffer, 1, size, fp) != size_t(size)) {
    free(buffer);
    fclose(fp);
    return false;
  }

  fclose(fp);

  *textp = buffer;
  *lengthp = size;
  return true;
}

static inline TranslationUnit *
ReadFile(const char *path)
{
  char *buffer;
  size_t size;
  if (!ReadFileChars(path, &buffer, &size))
    return NULL;

  TranslationUnit *tu = new TranslationUnit(strdup(path), buffer, size);
  return tu;
}

CompileContext::CompileContext(int argc, char **argv)
  : outOfMemory_(false),
    keywords_(SystemAllocatorPolicy()),
    strings_()
{
  assert(!CurrentCompileContext);

  CurrentCompileContext = this;

  if (argc < 2) {
    fprintf(stdout, "usage: <file>\n");
    return;
  }

  TranslationUnit *tu = ReadFile(argv[1]);
  if (!tu) {
    fprintf(stderr, "error reading file: %s\n", argv[1]);
    return;
  }

  units_.append(tu);

  // We automatically add "include" from the current working directory.
  searchPaths_.append("include/");
}

CompileContext::~CompileContext()
{
  for (size_t i = 0; i < units_.length(); i++)
    delete units_[i];
  for (size_t i = 0; i < errors_.length(); i++)
    free(errors_[i].message);
  CurrentCompileContext = NULL;
}

bool
CompileContext::compile()
{
  if (!initKeywords())
    return false;
  if (!strings_.init())
    return false;
  if (!types_.initialize())
    return false;

  Preprocessor pp(*this);
  pp.preprocess(units_[0]);

  printf("-- Preprocessing --\n");

  puts(units_[0]->text());

  printf("-- Parsing --\n");

  {
    Parser p(*this, units_[0]);
    ParseTree *tree = p.parse();
    if (!tree || errors_.length())
      return false;
    units_[0]->attach(tree);
    tree->dump(stdout);
  }

  printf("\n-- Name and Type Binding --\n");

  if (!ResolveNames(*this, units_[0]))
    return false;
  if (!ResolveTypes(*this, units_[0]))
    return false;

  //units_[0]->tree()->dump(stdout);

  {
    //AmxEmitter sema(*this, units_[0]);
    //if (!sema.compile())
    //  return false;
  }

  if (errors_.length())
    return false;
  if (outOfMemory_)
    return false;

  return true;
}

TokenKind
CompileContext::findKeyword(const char *id)
{
  KeywordTable::Result r = keywords_.find(id);
  if (!r.found())
    return TOK_NONE;
  return r->tok;
}

bool
CompileContext::defineKeyword(TokenKind tok)
{
  KeywordTable::Insert p = keywords_.findForAdd(TokenNames[tok]);
  assert(!p.found());

  KeywordEntry e;
  e.id = TokenNames[tok];
  e.tok = tok;
  keywords_.add(p, e);
  return true;
}

bool
CompileContext::initKeywords()
{
  if (!keywords_.init(128))
    return false;

#define _(name)                                                               \
  if (!defineKeyword(TOK_##name))                                             \
    return false;
  KEYWORDMAP(_)
#undef _

  return true;
}

const MessageInfo ke::Messages[] =
{
#define MSG(Name, Type, String) \
    { MessageType_##Type, String },
# include "messages.tbl"
#undef MSG
    { MessageType_SyntaxError, NULL }
};

const char *ke::MessageTypes[] =
{
  "syntax",
  "type",
  "system"
};

#if defined(_MSC_VER)
# define VA_COPY(to, from)  to = from
#else
# define VA_COPY(to, from)  va_copy(to, from)
#endif

static char *
BuildErrorMessage(const char *format, va_list ap)
{
  va_list use;
  VA_COPY(use, ap);

  // We over-allocate and zero terminate ahead of time, since LIBCRT's
  // version of vsnprintf is screwy.
  size_t size = 255;
  char *buffer = (char *)calloc(size + 1, 1);
  if (!buffer)
    return NULL;

  for (;;) {
    int r = vsnprintf(buffer, size, format, use);

    if (r > -1 && size_t(r) < size)
      return buffer;
     
#if defined(_MSC_VER)
    if (r < 0)
      size *= 2;
#else
    if (r > -1)
      size = size_t(r) + 1;
    else
      size *= 2;
#endif

    free(buffer);
    if ((buffer = (char *)calloc(size + 1, 1)) == NULL)
      return NULL;

    VA_COPY(use, ap);
  }
}

void
CompileContext::reportErrorVa(const SourceLocation &loc, Message msg, va_list ap)
{
  char *message = BuildErrorMessage(Messages[msg].format, ap);
  if (!message) {
    setOutOfMemory();
    return;
  }

  CompileError report;
  report.file = loc.file;
  report.type = MessageTypes[Messages[msg].type];
  report.message = message;
  report.line = loc.line;
  report.col = loc.col;
  if (!errors_.append(report))
    setOutOfMemory();
}

void
CompileContext::reportError(const SourceLocation &loc, Message msg, ...)
{
  va_list ap;
  va_start(ap, msg);
  reportErrorVa(loc, msg, ap);
  va_end(ap);
}

#if defined __linux__
extern "C" void __cxa_pure_virtual(void)
{
}

void *operator new(size_t size)
{
	return malloc(size);
}

void *operator new[](size_t size) 
{
	return malloc(size);
}

void operator delete(void *ptr) 
{
	free(ptr);
}

void operator delete[](void * ptr)
{
	free(ptr);
}
#endif

