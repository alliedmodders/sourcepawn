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
#ifndef _include_spcomp_preprocessor_h_
#define _include_spcomp_preprocessor_h_

#include "compile-context.h"
#include "keyword-table.h"
#include "lexer.h"
#include "macro-lexer.h"
#include "process-options.h"
#include "pool-allocator.h"

namespace sp {

class CommentDelegate
{
 public:
  virtual void HandleComment(CommentPos where, const SourceRange& extends) = 0;
};

enum class SkipFlags
{
  None            = 0x0,
  StopAtLine      = 0x1,
  StopBeforeMatch = 0x2,
  StopAtSemi      = 0x4,
  StartAtCurrent  = 0x8
};
KE_DEFINE_ENUM_OPERATORS(SkipFlags)

// In earlier iterations of SourcePawn, we used three passes for lexing. The
// first pass stripped comments. The second pass handled preprocessor
// directives. The final pass lexed the entire concatenated source buffer for
// all source files and includes.
//
// This had two disadvantages. First, it is slower than having a single-pass
// lexer. Second, we lost important information in the first two passes, such
// as the ability to attach comments to tokens, or track the origin of tokens
// pasted via preprocessor directives.
//
// The new system unifies all phases together. As such, the Preprocessor is no
// longer a distinct phase, but the actual interface for tokenization. Since
// some components still operate on a per-file basis, we keep the concept of a
// lexing (the old text-processor) distinct, and the preprocessor keeps a stack
// of lexers.
class Preprocessor
{
  friend class Lexer;
  friend class MacroLexer;

 public:
  Preprocessor(CompileContext& cc);

  // Start preprocessing a file. This blows away any existing lexer state.
  bool enter(RefPtr<SourceFile> file);

  // Functions for the parser and eval().
  TokenKind peek();
  bool match(TokenKind kind);
  bool expect(TokenKind kind);
  void undo() {
    tokens_->push();
  }

  // Disable #include.
  void disableIncludes() {
    disable_includes_ = true;
  }
  void setCommentDelegate(CommentDelegate* handler) {
    comment_handler_ = handler;
  }
  bool traceComments() const {
    return !!comment_handler_;
  }

  // If there is a newline in between the most recently lexed token and the
  // next token, return EOL. Otherwise, return the next token. The next token
  // is left unbuffered, so the current token ends up unchanged.
  TokenKind peekTokenSameLine();

  // Retrieves the most recently scanned, unbuffered token.
  const Token* current() const {
    return tokens_->current();
  }
  const SourceLocation& begin() const {
    return current()->start.loc;
  }
  const SourceLocation& end() const {
    return current()->end.loc;
  }
  Atom* current_name() const {
    return current()->atom();
  }

  // Advances the token stream by one token, returning the new token kind.
  TokenKind next();
  const Token* nextToken() {
    next();
    return current();
  }

  // Injects a token. See TokenRing::inject.
  void injectBack(const Token& tok) {
    tokens_->inject(tok);
  }

  // Consumes the rest of a line to assist with error messaging.
  void eatRestOfLine();

  // Consume until a token has been reached. If opener is specified, it acts
  // as a second stopping point.
  void skipUntil(TokenKind kind, SkipFlags flags, TokenKind opener = TOK_NONE);

 private:
  // Internal implementation for next().
  TokenKind scan();

  void setup_builtin_macros();
  void define_builtin_int(const char* name, int64_t value);
  void define_builtin_string(const char* name, const char* str);

 private: // Interfaces for Lexer and MacroLexer.
  // #include and #try_include support.
  bool enterFile(TokenKind directive,
                 const SourceLocation& from,
                 const char* file,
                 const char* where);

  // Defines a new macro, reporting an error if one already exists with the
  // same name.
  void defineMacro(Atom* name,
                   const SourceLocation& nameLoc,
                   TokenList* tokens);

  // Returns true if the name is a macro and the macro was entered; false
  // otherwise.
  bool enterMacro(const SourceLocation& loc, Atom* name);

  // Returns false if an error was reported.
  bool removeMacro(const SourceLocation& loc, Atom* name);

  // Leaves a lexer. If this returns true, the lexer is finished, and it should
  // return TOK_NONE instead of TOK_EOF.
  bool handleEndOfFile();

  void setNextDeprecationMessage(const char* buffer, size_t length) {
    next_deprecation_message_ = std::string(buffer, length);
  }

  // Nasty business of evaluating a preprocessor expression. Implemented in
  // tk-evaluator.cpp. Returns false if an error occurred, in which case val
  // is left unchanged.
  bool eval(int* val);
  bool eval_inner(int* val);
  bool eval_unary(int* val);
  bool eval_binary(int prec, int* val);

  // These variants handle macro expansion inside evals.
  TokenKind eval_next();
  TokenKind eval_peek();
  bool eval_match(TokenKind kind);
  bool eval_expect(TokenKind kind);

  // Track comment ranges so we can piece together documentation afterward.
  void addComment(CommentPos where, const SourceRange& extends);

  TokenKind findKeyword(Atom* name) {
    return keywords_.findKeyword(name);
  }

  bool& macro_expansion() {
    return allow_macro_expansion_;
  }

 private:
  // Internal functions.
  AutoString searchPaths(const char* file, const char* where);

 private:
  struct SavedLexer {
    RefPtr<Lexer> lexer;
    RefPtr<MacroLexer> macro_lexer;

    SavedLexer()
    {}
    SavedLexer(RefPtr<Lexer> lexer, RefPtr<MacroLexer> macro_lexer)
     : lexer(lexer),
       macro_lexer(macro_lexer)
    {}
  };

 private:
  CompileContext& cc_;
  const CompileOptions& options_;
  LexOptions lex_options_;
  KeywordTable keywords_;

  AtomMap<Macro*> macros_;

  std::vector<RefPtr<MacroLexer>> recycled_macro_lexers_;
  std::vector<SavedLexer> lexer_stack_;

  // Exactly one of these is set at a given time.
  RefPtr<Lexer> lexer_;
  RefPtr<MacroLexer> macro_lexer_;

  // We keep two separate token buffers. It's important that preprocessor
  // tokens do not get injected into the middle of the normal token stream.
  // SP 1.7 requires a large lookahead (4 tokens), and it's easy for an #ifdef
  // to lex into that. It was particularly easy in older iterations of the
  // compiler, but presently, we can handle most preprocessor directives
  // entirely from within the lexer itself. The only case where we cannot is
  // eval().
  TokenRing pp_tokens_;
  TokenRing normal_tokens_;
  TokenRing* tokens_;

  // The next deprecation message.
  std::string next_deprecation_message_;

  // Whether or not macro expansion is allowed.
  bool allow_macro_expansion_;

  // Whether or not #include is disabled.
  bool disable_includes_;

  // Number of files currently #included in the lexer stack.
  size_t include_depth_;

  // Handler for comments.
  CommentDelegate* comment_handler_;
};

}

#endif // _include_spcomp_preprocessor_h_
