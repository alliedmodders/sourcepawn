// vim: set ts=8 sts=4 sw=4 tw=99 et:
/*  Pawn compiler - File input, preprocessing and lexical analysis functions
 *
 *  Copyright (c) ITB CompuPhase, 1997-2006
 *
 *  This software is provided "as-is", without any express or implied warranty.
 *  In no event will the authors be held liable for any damages arising from
 *  the use of this software.
 *
 *  Permission is granted to anyone to use this software for any purpose,
 *  including commercial applications, and to alter it and redistribute it
 *  freely, subject to the following restrictions:
 *
 *  1.  The origin of this software must not be misrepresented; you must not
 *      claim that you wrote the original software. If you use this software in
 *      a product, an acknowledgment in the product documentation would be
 *      appreciated but is not required.
 *  2.  Altered source versions must be plainly marked as such, and must not be
 *      misrepresented as being the original software.
 *  3.  This notice may not be removed or altered from any source distribution.
 *
 *  Version: $Id$
 */
#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <filesystem>
#include <string>
#include <unordered_set>
#include <utility>

#if defined __linux__ || defined __FreeBSD__ || defined __OpenBSD__ || defined DARWIN
#    include <unistd.h>
#endif

#if defined _MSC_VER && defined _WIN32
#    include <direct.h>
#endif

#include <amtl/am-hashmap.h>
#include <amtl/am-platform.h>
#include <amtl/am-raii.h>
#include <amtl/am-string.h>
#include <sp_typeutil.h>
#include "array-helpers.h"
#include "compile-options.h"
#include "errors.h"
#include "lexer.h"
#include "lexer-inl.h"
#include "parser.h"
#include "sc.h"
#include "sci18n.h"
#include "sctracker.h"
#include "semantics.h"
#include "source-manager.h"
#include "symbols.h"
#include "types.h"

namespace sp {
namespace cc {

namespace fs = std::filesystem;

// Flags for litchar().
//
// Decode utf-8 and error on failure. If unset, non-ASCII characters will be
// returned as their original bytes.
static constexpr int kLitcharUtf8 = 0x1;
// Do not error, because the characters are being ignored.
static constexpr int kLitcharSkipping = 0x2;

bool Lexer::PlungeQualifiedFile(const token_pos_t& from, const std::string& name) {
    auto fp = OpenFile(from, name);
    if (!fp)
        return false;
    if (fp->included())
        return true;

    PlungeFile(fp);
    return true;
}

void Lexer::PlungeFile(std::shared_ptr<SourceFile> fp) {
    assert(!IsSkipping());
    assert(skiplevel_ == ifstack_.size()); /* these two are always the same when "parsing" */

    // Save any lookahead tokens.
    state_.token_buffer = token_buffer_;
    while (token_buffer_->depth > 0) {
        lexpop();
        state_.saved_tokens.emplace_back(std::move(*current_token()));
    }

    auto pos = current_token()->start;
    state_.entry_preproc_if_stack_size = ifstack_.size();
    PushLexerState();
    EnterFile(std::move(fp), pos);
}

std::shared_ptr<SourceFile> Lexer::OpenFile(const token_pos_t& from, const std::string& name) {
    AutoCountErrors detect_errors;

    if (auto sf = cc_.sources()->Open(from, name))
        return sf;

    static const std::vector<std::string> extensions = {".inc", ".p", ".pawn"};
    for (const auto& extension : extensions) {
        auto alt_name = name + extension;
        if (auto sf = cc_.sources()->Open(from, alt_name))
            return sf;
        if (!detect_errors.ok())
            return nullptr;
    }
    return nullptr;
}

bool
Lexer::PlungeFile(const token_pos_t& from, const std::string& name, int try_currentpath,
                  int try_includepaths)
{
    if (try_currentpath) {
        if (PlungeQualifiedFile(from, name))
            return true;

        // failed to open the file in the active directory, try to open the file
        // in the same directory as the current file --but first check whether
        // there is a (relative) path for the current file
        fs::path current_path(state_.inpf->name());
        auto parent_path = current_path.parent_path();
        if (!parent_path.empty()) {
            auto new_path = parent_path / name;
            if (PlungeQualifiedFile(from, new_path.string()))
                return true;
        }
    }

    if (try_includepaths && !fs::path(name).is_absolute()) {
        auto& cc = CompileContext::get();
        for (const auto& inc_path : cc.options()->include_paths) {
            auto path = fs::path(inc_path) / fs::path(name);
            if (PlungeQualifiedFile(from, path.string()))
                return true;
        }
    }

    return false;
}

std::string StringizePath(const fs::path& in_path) {
    auto path = '"' + in_path.string() + '"';
    auto pos = path.find('\\');
    while (pos != std::string::npos) {
        path.insert(pos + 1, 1, '\\');
        pos = path.find('\\', pos + 2);
    }
    return path;
}

void Lexer::SetFileDefines(const std::shared_ptr<SourceFile> file) {
    if (file->is_builtin())
        return;

    fs::path path = fs::canonical(file->name());

    auto full_path = StringizePath(path);
    auto name = StringizePath(path.filename());

    AddMacro("__FILE_PATH__", full_path.c_str());
    AddMacro("__FILE_NAME__", name.c_str());
}

void Lexer::CheckLineEmpty(bool allow_semi) {
    AutoCountErrors errors;
    while (true) {
        int tok = lex_same_line();
        if (tok == tEOL) {
            if (IsInMacro()) {
                HandleEof();
                continue;
            }
            break;
        }
        if (tok == ';' && allow_semi && peek_same_line() == tEOL)
            break;
        if (errors.ok() && tok >= ' ')
            report(38);
    }
}

void
Lexer::SynthesizeIncludePathToken()
{
    SkipLineWhitespace();

    auto tok = PushSynthesizedToken(tSYN_INCLUDE_PATH, pos());

    char open_c = peek();
    char close_c;
    if (open_c == '"' || open_c == '<') {
        close_c = (char)((open_c == '"') ? '"' : '>');
        advance();
    } else {
        close_c = 0;
        report(247);
    }

    SkipLineWhitespace();

    std::string name;

    int i = 0;
    while (true) {
        char c = peek();
        if (c == close_c || c == '\0' || i >= (int)sizeof(name) - 1 || IsNewline(c))
            break;
        if (fs::path::preferred_separator != '/' && c == '/') {
            name.push_back(fs::path::preferred_separator);
            advance();
        } else {
            name.push_back(advance());
        }
    }
    while (!name.empty() && name.back() == ' ')
        name.pop_back();

    if (close_c) {
        if (advance() != close_c)
            report(37);
    }

    CheckLineEmpty();

    if (!open_c)
        open_c = '"';
    tok->atom = cc_.atom(ke::StringPrintf("%c%s", open_c, name.c_str()));
}

/*  ftoi
 *
 *  Attempts to interpret a numeric symbol as a rational number, either as
 *  IEEE 754 single/double precision floating point or as a fixed point integer.
 *  On success it returns the number of characters processed and the value is
 *  stored in "val". Otherwise it returns 0 and "val" is unchanged.
 *
 *  Pawn has stricter definition for rational numbers than most:
 *  o  the value must start with a digit; ".5" is not a valid number, you
 *     should write "0.5"
 *  o  a period must appear in the value, even if an exponent is given; "2e3"
 *     is not a valid number, you should write "2.0e3"
 *  o  at least one digit must follow the period; "6." is not a valid number,
 *     you should write "6.0"
 */
void Lexer::lex_float(full_token_t* tok, cell_t whole) {
    double fnum = whole;

    double ffrac = 0.0;
    double fmult = 1.0;
    while (true) {
        char c = peek();
        if (!IsDigit(c) && c != '_')
            break;
        advance();
        if (c != '_') {
            ffrac = (ffrac * 10.0) + (c - '0');
            fmult = fmult / 10.0;
        }
    }
    fnum += ffrac * fmult; /* form the number so far */
    if (match_char('e')) {     /* optional fractional part */
        int sign;
        if (match_char('-'))
            sign = -1;
        else
            sign = 1;
        int exp = 0;
        int ndigits = 0;
        while (true) {
            char c = peek();
            if (!IsDigit(c))
                break;
            advance();
            exp = (exp * 10) + (c - '0');
            ndigits++;
        }
        if (!ndigits)
            report(425);
        fmult = pow(10.0, exp * sign);
        fnum *= fmult;
    }

    /* floating point */
    float value = (float)fnum;
    tok->numeric_value = FloatCellUnion(value).cell;
    tok->id = tRATIONAL;
}

int Lexer::preproc_expr(cell* val, Type** type) {
    ke::SaveAndSet<bool> forbid_const(&cc_.in_preprocessor(), true);
    return Parser::PreprocExpr(val, type); /* get value (or 0 on error) */
}

enum {
    CMD_NONE,
    CMD_TERM,
    CMD_EMPTYLINE,
    CMD_CONDFALSE,
    CMD_INCLUDE,
    CMD_DEFINE,
    CMD_IF,
    CMD_DIRECTIVE,
    CMD_INJECTED,
};

void Lexer::HandleDirectives() {
    assert(peek() == '#');

    ke::SaveAndSet<token_buffer_t*> switch_buffers(&token_buffer_, &preproc_buffer_);
    assert(token_buffer_->depth == 0);

    // We should be guaranteed that nested FindNextToken will bail out, since
    // we're already at a valid token.
    int tok = LexNewToken();
    switch (tok) {
        case tpIF: {
            ifstack_.emplace_back(0);
            skiplevel_ = ifstack_.size();

            cell val = 0;
            preproc_expr(&val, NULL); /* get value (or 0 on error) */
            CheckLineEmpty();

            ifstack_.back() = (char)(val ? PARSEMODE : SKIPMODE);

            if (IsSkipping())
                HandleSkippedSection();
            break;
        }

        // By definition, tpELSE, tpELSEIF, and tpENDIF are reached here by
        // being in a non-skipped section.
        case tpELSE:
            if (ifstack_.empty()) {
                report(26);
                break;
            }
            if ((ifstack_.back() & HANDLED_ELSE) == HANDLED_ELSE)
                report(60); /* multiple #else directives between #if ... #endif */
            ifstack_.back() |= (char)(SKIPMODE | HANDLED_ELSE);
            CheckLineEmpty();
            HandleSkippedSection();
            break;

        case tpELSEIF: {
            if (ifstack_.empty()) {
                report(26);
                break;
            }
            if ((ifstack_.back() & HANDLED_ELSE) == HANDLED_ELSE)
                report(61); /* #elseif directive may not follow an #else */
            ifstack_.back() |= (char)SKIPMODE;
            HandleSkippedSection();
            break;
        }

        case tpENDIF:
            if (ifstack_.empty()) {
                report(26); /* no matching "#if" */
                break;
            }
            ifstack_.pop_back();
            if (ifstack_.size() < skiplevel_)
                skiplevel_ = ifstack_.size();
            CheckLineEmpty();
            break;

        case tINCLUDE: /* #include directive */
        case tpTRYINCLUDE: {
            auto pos = current_token()->start;
            SynthesizeIncludePathToken();
            PushSynthesizedToken((TokenKind)tok, pos);
            break;
        }
        case tpASSERT:
        {
            ke::SaveAndSet<bool> reset(&cc_.detected_illegal_preproc_symbols(), false);

            cell val = 0;
            preproc_expr(&val, NULL); /* get constant expression (or 0 on error) */
            if (!val)
                report(415);

            CheckLineEmpty();
            break;
        }
        case tpPRAGMA:
            {
                ke::SaveAndSet<bool> no_macros(&allow_substitutions_, false);
                if (lex() != tSYMBOL) {
                    report(207);
                    break;
                }
            }
            if (current_token()->atom->str() == "ctrlchar") {
                int tok = lex_same_line();
                if (tok == tEOL) {
                    ctrlchar_ = cc_.options()->ctrlchar_org;
                } else {
                    if (tok == tNUMBER)
                        ctrlchar_ = (char)current_token()->value();
                    else
                        report(27); /* invalid character constant */
                }
            } else if (current_token()->atom->str() == "deprecated") {
                deprecate_ = SkimUntilEndOfLine();
            } else if (current_token()->atom->str() == "dynamic") {
                preproc_expr(&cc_.options()->pragma_dynamic, NULL);
            } else if (current_token()->atom->str() == "rational") {
                report(250);
                SkimUntilEndOfLine();
            } else if (current_token()->atom->str() == "semicolon") {
                cell val;
                preproc_expr(&val, NULL);
                state_.need_semicolon = !!val;
            } else if (current_token()->atom->str() == "newdecls") {
                int tok = lex_same_line();
                if (tok != tSYMBOL) {
                    report(146);
                    break;
                }
                auto atom = current_token()->atom;
                if (atom->str() == "required")
                    state_.require_newdecls = true;
                else if (atom->str() == "optional")
                    state_.require_newdecls = false;
                else
                    report(146);
            } else if (current_token()->atom->str() == "tabsize") {
                cell val;
                preproc_expr(&val, NULL);
                cc_.options()->tabsize = (int)val;
            } else if (current_token()->atom->str() == "unused") {
                auto pos = current_token()->start;
                if (!need_same_line(tSYMBOL))
                    break;

                std::vector<std::string> parts = { current_token()->atom->str() };
                while (match_same_line(',')) {
                    if (!need_same_line(tSYMBOL))
                        break;
                    parts.emplace_back(current_token()->atom->str());
                }

                auto tok = PushSynthesizedToken(tSYN_PRAGMA_UNUSED, pos);
                tok->atom = cc_.atom(ke::Join(parts, ","));
            } else {
                report(207); /* unknown #pragma */
            }
            CheckLineEmpty(true);
            break;

        case tpENDINPUT:
        case tpENDSCRPT:
            CheckLineEmpty();
            state_.pos = state_.end;
            break;
        case tpDEFINE: {
            Atom* symbol;
            {
                ke::SaveAndSet<bool> no_macros(&allow_substitutions_, false);
                ke::SaveAndSet<bool> no_keywords(&allow_keywords_, false);

                if (!needsymbol(&symbol))
                    break;
            }
            if (!alpha(symbol->str()[0])) {
                report(74); /* pattern must start with an alphabetic character */
                break;
            }

            auto macro_pos = current_token()->start;

            ke::Maybe<tr::vector<int>> args;
            if (match_char('(')) {
                ke::SaveAndSet<bool> no_macros(&allow_substitutions_, false);
                AutoCountErrors errors;

                std::unordered_set<int> seen;

                args.init();
                do {
                    if (args.get().empty() && match(')')) {
                        lexpush();
                        break;
                    }
                    if (!need('%'))
                        break;
                    char c = peek();
                    if (c < '0' || c > '9') {
                        report(426);
                        break;
                    }
                    advance();

                    int arg = (c - '0');
                    if (seen.count(arg)) {
                        report(427) << arg;
                        break;
                    }
                    seen.emplace(arg);
                    args.get().emplace_back(arg);
                } while (match(','));
                if (!errors.ok())
                    break;
                if (!need(')'))
                    break;
            } else {
                if (!IsSpace(peek())) {
                    report(430);
                    break;
                }
            }

            MacroEntry def;
            if (HasMacro(symbol)) {
                report(201) << symbol; /* redefinition of macro (non-identical) */
                break;
            }

            auto macro = std::make_shared<MacroEntry>();
            macro->args = std::move(args);
            macro->pattern = symbol;
            macro->documentation = std::move(deprecate_);
            macro->deprecated = !macro->documentation.empty();
            macro->pos = macro_pos;

            tr::vector<size_t>* arg_positions = nullptr;
            if (macro->args)
                arg_positions =  &macro->arg_positions;
            macro->substitute = cc_.atom(SkimUntilEndOfLine(arg_positions));

            macros_[symbol] = std::move(macro);
            break;
        } /* case */
        case tpUNDEF: {
            ke::SaveAndSet<bool> no_macros(&allow_substitutions_, false);
            if (!need(tSYMBOL))
                break;
            DeleteMacro(current_token()->atom);
            CheckLineEmpty();
            break;
        }
        case tpERROR: {
            auto str = SkimUntilEndOfLine();
            report(416) << str;
            break;
        }
        case tpWARNING: {
            auto str = SkimUntilEndOfLine();
            report(224) << str;
            break;
        }
        default:
            report(31); /* unknown compiler directive */
    }

    // Make sure we eat everything remaining on the line.
    while (lex_same_line() != tEOL)
        continue;

    // Because we might have pre-lexed additional characters into the
    // preprocessor stream, we need to clear the buffer.
    preproc_buffer_.depth = 0;
}

void Lexer::HandleSkippedSection() {
    // Eat stuff until we reach a new directive.
    while (more()) {
        char c = peek();
        if (IsNewline(c)) {
            HandleNewline(c, '\0');
            continue;
        }
        if (c == '/' && peek2() == '/') {
            HandleSingleLineComment();
            continue;
        }
        if (c == '/' && peek2() == '*') {
            HandleMultiLineComment();
            continue;
        }

        if (c == '#' && tokens_on_line_ == 0) {
            int tok = LexNewToken();
            switch (tok) {
                case tpIF:
                    ifstack_.emplace_back(0);
                    continue;

                case tpELSE:
                    // Handle errors in the if/else structure even if skipping.
                    if ((ifstack_.back() & HANDLED_ELSE) == HANDLED_ELSE) {
                        report(60); /* multiple #else directives between #if ... #endif */
                        continue;
                    }
                    if ((ifstack_.back() & PARSEMODE) != PARSEMODE) {
                        ifstack_.back() &= (char)~SKIPMODE;
                        ifstack_.back() |= HANDLED_ELSE;
                        // Note: just because we enabled parsemode, we might be
                        // skipping due to a higher-up #if. We need to re-check.
                        if (!IsSkipping()) {
                            CheckLineEmpty();
                            return;
                        }
                    }
                    continue;

                case tpELSEIF:
                    if ((ifstack_.back() & HANDLED_ELSE) == HANDLED_ELSE) {
                        report(61); /* #elseif directive may not follow an #else */
                        continue;
                    }

                    if ((ifstack_.back() & PARSEMODE) != PARSEMODE) {
                        if (skiplevel_ != ifstack_.size())
                            continue; // Every section must be skipped.

                        cell val = 0;
                        preproc_expr(&val, NULL); /* get value (or 0 on error) */

                        ifstack_.back() &= (char)~SKIPMODE;
                        ifstack_.back() |= (char)(val ? PARSEMODE : SKIPMODE);

                        if (!IsSkipping()) {
                            CheckLineEmpty();
                            return;
                        }
                    }
                    continue;

                case tpENDIF:
                    CheckLineEmpty();

                    ifstack_.pop_back();
                    if (ifstack_.size() < skiplevel_)
                        skiplevel_ = ifstack_.size();

                    if (!IsSkipping())
                        return;
                    continue;

                default:
                    continue;
            }
        }

        if (!IsSpace(c))
            tokens_on_line_++;
        advance();
    }
}

void Lexer::SkipLineWhitespace() {
    while (true) {
        char c = peek();
        if (!IsSpace(c) || IsNewline(c))
            break;
        advance();
    }
}

static inline void AddText(std::string* text, const unsigned char** start,
                           const unsigned char* end, char extra)
{
    if (!*start)
        return;
    *text += std::string((const char *)*start, (const char *)end);
    if (extra != '\0' && !text->empty() && text->back() != extra)
        text->push_back(extra);
    *start = nullptr;
}

std::string Lexer::SkimUntilEndOfLine(tr::vector<size_t>* macro_args) {
    std::string text;

    const unsigned char* start = nullptr;
    while (true) {
        char c = peek();
        if (c == '\0' || IsNewline(c))
            break;
        if (c == '/' && peek2() == '/')
            break;
        if (c == '/' && peek2() == '*') {
            AddText(&text, &start, char_stream(), ' ');
            HandleMultiLineComment();
            continue;
        }
        if (c == '\\') {
            auto end = char_stream();
            if (MaybeHandleLineContinuation()) {
                AddText(&text, &start, end, ' ');
                continue;
            }
        }

        if (!IsSpace(c) && !start)
            start = char_stream();

        advance();

        if (c == '\"' || c == '\'') {
            // Skip any tokens inside strings.
            char term = c;
            while (true) {
                auto saved_pos = char_stream();
                cell ch = get_utf8_char();
                if (ch > 0x7f)
                    continue;

                if (ch == ctrlchar_ && peek() == term) {
                    advance();
                    continue;
                }
                if (ch == '\\') {
                    AddText(&text, &start, saved_pos, '\0');
                    if (MaybeHandleLineContinuation())
                        start = char_stream();
                    else
                        start = saved_pos;
                    continue;
                }
                if (ch == 0 || ch == term || IsNewline((char)ch))
                    break;
            }
        } else if (c == '%' && macro_args && IsDigit(peek())) {
            advance();
            AddText(&text, &start, char_stream(), '\0');
            macro_args->emplace_back(text.size() - 2);

            // Don't accidentally trim any whitespace around the %N token.
            start = char_stream();
        }
    }

    AddText(&text, &start, char_stream(), '\0');

    while (!text.empty() && IsSpace(text.back()))
        text.pop_back();

    return text;
}

// Find the starting position of the next token. This eats newlines, whitespace,
// and EOF scenarios if there are nested files.
bool Lexer::FindNextToken() {
    assert(token_buffer_->depth == 0);

    while (true) {
        if (!freading_)
            return false;

        auto work_start = char_stream();
        auto work_line = line_start();
        bool is_line_start = (work_line == work_start) && !IsInMacro();

        // Skip whitespace.
        while (true) {
            char c = peek();
            if (!IsSpace(c))
                break;

            if (c == '\r' || c == '\n')
                break;

            advance();
        }

        char c = peek();
        switch (c) {
            case '\r':
            case '\n':
                if (IsPreprocessing())
                    return false;

                // Handling the newline may give us more whitespace, so we restart
                // the loop.
                HandleNewline(c, '\0');
                continue;

            case '/':
                if (peek2() == '/') {
                    HandleSingleLineComment();
                    continue;
                }
                if (peek2() == '*') {
                    HandleMultiLineComment();
                    continue;
                }
                return true;

            case '#':
                if (IsPreprocessing() || tokens_on_line_ > 0)
                    return true;
                HandleDirectives();
                if (token_buffer_->depth > 0)
                    return true; // token was synthesized, exit.
                continue;

            // This is a line continuation, but it's an invalid token anywhere
            // but at the end of a line (modulo whitespace). It's a little
            // tricky to handle.
            case '\\':
            {
                if (MaybeHandleLineContinuation())
                    continue;
                return true;
            }

            case '\0':
                if (IsPreprocessing() && !IsInMacro())
                    return false;
                else if (!allow_end_of_file_)
                    return false;
                HandleEof();
                continue;

            default:
                if (is_line_start && c < ' ') {
                    // Preserve old behavior where garbage characters at the
                    // start of the line were ignored. Except warn about it
                    // now.
                    report(227);
                    advance();
                    continue;
                }
                // No whitespace, new comments - we're done!
                return true;
        }
    }
}

bool Lexer::MaybeHandleLineContinuation() {
    // Save the position if we've mispredicted the continuation.
    auto saved_pos = char_stream();

    // Eat the backslash and adjacent whitespace.
    advance();
    SkipLineWhitespace();

    char c = peek();
    if (!IsNewline(c)) {
        // Mispredicted.
        backtrack(saved_pos);
        return false;
    }

    HandleNewline(c, '\\');
    return true;
}

// Returns true if the EOF resulted in a file change.
void Lexer::HandleEof() {
    assert(!more());

    if (prev_state_.empty() && !file_queue_.empty()) {
        auto file = ke::PopFront(&file_queue_);
        EnterFile(std::move(file), {});
        return;
    }

    if (prev_state_.empty()) {
        freading_ = false;
        if (!ifstack_.empty())
            report(1) << "#endif" << "-end of file-";
        return;
    }

    bool was_in_macro = !!state_.macro;
    if (was_in_macro) {
        auto p = macros_in_use_.find(state_.macro.get());
        if (p != macros_in_use_.end())
            macros_in_use_.erase(p);
    }

    state_ = ke::PopBack(&prev_state_);

    // Restore any saved tokens.
    if (state_.token_buffer) {
        ke::SaveAndSet<token_buffer_t*> switch_buffers(&token_buffer_, state_.token_buffer);

        assert(token_buffer_->depth == 0);
        for (auto&& saved : state_.saved_tokens) {
            auto tok = advance_token_ptr();
            *tok = std::move(saved);
        }
        for (size_t i = 0; i < state_.saved_tokens.size(); i++)
            lexpush();

        state_.token_buffer = nullptr;
        state_.saved_tokens.clear();
    }

    /* this condition held before including the file */
    if (!was_in_macro) {
        skiplevel_ = state_.entry_preproc_if_stack_size;
        while (skiplevel_ < ifstack_.size())
            ifstack_.pop_back();
        assert(skiplevel_ == ifstack_.size());

        assert(!IsSkipping());   /* idem ditto */
        SetFileDefines(state_.inpf);
    }
}

void Lexer::HandleNewline(char c, char continuation) {
    assert(peek() == c);

    if (advance() == '\r')
        match_char('\n');

    state_.fline++;
    if (continuation != '\\') {
        state_.tokline++;
        tokens_on_line_ = 0;
    }
    state_.line_start = char_stream();
}

void Lexer::HandleSingleLineComment() {
    char c = advance();
    assert(c == '/');

    c = advance();
    assert(c == '/');
    (void)c;

    char prev_c = c;
    while (true) {
        char c = peek();
        if (c == '\0' || IsNewline(c)) {
            if (prev_c == '\\')
                report(49); // invalid line continuation
            break;
        }
        if (!IsSpace(c))
            prev_c = c;
        advance();
    }
}

void Lexer::HandleMultiLineComment() {
    char c = advance();
    assert(c == '/');

    c = advance();
    assert(c == '*');
    (void)c;

    while (true) {
        if (match_char('*')) {
            if (match_char('/'))
                return;
            continue;
        }
        if (match_char('/')) {
            if (peek() == '*')
                report(216); // nested comment
            continue;
        }
        char c = peek();
        if (c == '\0') {
            report(1) << "*/" << "-end of file-";
            return;
        }
        if (IsNewline(c)) {
            // Line continuations are ignored inside comments. Make sure the
            // tokens-per-line count isn't reset.
            auto old_tokens_on_line = tokens_on_line_;
            HandleNewline(c, '\0');
            tokens_on_line_ = old_tokens_on_line;
            continue;
        }
        advance();
    }
}

void Lexer::packedstring(full_token_t* tok, char term) {
    std::string data;
    while (true) {
        char c = peek();
        if (c == term || c == 0)
            break;
        if (c == '\\') {
            if (MaybeHandleLineContinuation())
                continue;
        }
        if (IsNewline(c))
            break;
        packedstring_char(&data);
    }
    tok->atom = cc_.atom(data);
}

void Lexer::packedstring_char(std::string* data) {
    bool is_codepoint;
    cell ch = litchar(kLitcharUtf8, &is_codepoint);
    if (ch < 0)
        return;
    if (is_codepoint)
        UnicodeCodepointToUtf8(ch, data);
    else
        data->push_back(static_cast<char>(ch));
}

/*  lex(lexvalue,lexsym)        Lexical Analysis
 *
 *  lex() first deletes leading white space, then checks for multi-character
 *  operators, keywords (including most compiler directives), numbers,
 *  labels, symbols and literals (literal characters are converted to a number
 *  and are returned as such). If every check fails, the line must contain
 *  a single-character operator. So, lex() returns this character. In the other
 *  case (something did match), lex() returns the number of the token. All
 *  these tokens have been assigned numbers above 255.
 *
 *  Some tokens have "attributes":
 *     tNUMBER        the value of the number is return in "lexvalue".
 *     tRATIONAL      the value is in IEEE 754 encoding or in fixed point
 *                    encoding in "lexvalue".
 *     tSYMBOL        the first sNAMEMAX characters of the symbol are
 *                    stored in a buffer, a pointer to this buffer is
 *                    returned in "lexsym".
 *     tLABEL         the first sNAMEMAX characters of the label are
 *                    stored in a buffer, a pointer to this buffer is
 *                    returned in "lexsym".
 *     tSTRING        the string is stored in the literal pool, the index
 *                    in the literal pool to this string is stored in
 *                    "lexvalue".
 *
 *  lex() stores all information (the token found and possibly its attribute)
 *  in global variables. This allows a token to be examined twice. If "_pushed"
 *  is true, this information is returned.
 *
 *  Global references: lptr          (altered)
 *                     fline         (referred to only)
 *                     _pushed
 */

// lex() is called recursively, which messes up the lookahead buffer. To get
// around this we use two separate token buffers.

full_token_t*
Lexer::next_token()
{
    assert(token_buffer_->depth > 0);
    int cursor = token_buffer_->cursor + 1;
    if (cursor == MAX_TOKEN_DEPTH)
        cursor = 0;
    return &token_buffer_->tokens[cursor];
}

const char* sc_tokens[] = {"*=",
                           "/=",
                           "%=",
                           "+=",
                           "-=",
                           "<<=",
                           ">>>=",
                           ">>=",
                           "&=",
                           "^=",
                           "|=",
                           "||",
                           "&&",
                           "==",
                           "!=",
                           "<=",
                           ">=",
                           "<<",
                           ">>>",
                           ">>",
                           "++",
                           "--",
                           "...",
                           "..",
                           "::",
                           "acquire",
                           "as",
                           "assert",
                           "break",
                           "builtin",
                           "catch",
                           "case",
                           "cast_to",
                           "char",
                           "const",
                           "continue",
                           "decl",
                           "default",
                           "defined",
                           "delete",
                           "do",
                           "double",
                           "else",
                           "enum",
                           "exit",
                           "explicit",
                           "false",
                           "finally",
                           "for",
                           "foreach",
                           "forward",
                           "funcenum",
                           "functag",
                           "function",
                           "goto",
                           "if",
                           "implicit",
                           "import",
                           "in",
                           "int",
                           "int8",
                           "int16",
                           "int32",
                           "int64",
                           "interface",
                           "intn",
                           "INVALID_FUNCTION",
                           "let",
                           "methodmap",
                           "namespace",
                           "native",
                           "new",
                           "null",
                           "__nullable__",
                           "object",
                           "operator",
                           "package",
                           "private",
                           "protected",
                           "public",
                           "readonly",
                           "return",
                           "sealed",
                           "sizeof",
                           "static",
                           "static_assert",
                           "stock",
                           "struct",
                           "switch",
                           "this",
                           "throw",
                           "true",
                           "try",
                           "typedef",
                           "typeof",
                           "typeset",
                           "uint8",
                           "uint16",
                           "uint32",
                           "uint64",
                           "uintn",
                           "union",
                           "using",
                           "var",
                           "variant",
                           "view_as",
                           "virtual",
                           "void",
                           "volatile",
                           "while",
                           "with",
                           "#assert",
                           "#define",
                           "#else",
                           "#elseif",
                           "#endif",
                           "#endinput",
                           "#endscript",
                           "#error",
                           "#warning",
                           "#if",
                           "#include",
                           "#line",
                           "#pragma",
                           "#tryinclude",
                           "#undef",
                           ";",
                           ";",
                           "-integer value-",
                           "-float value-",
                           "-identifier-",
                           "-label-",
                           "-string-",
                           "-char-",
                           "#pragma unused",
                           "-include-path-",
                           "-end of line-",
                           "-declaration-",
                           "-macro-",
                           "-maybe-label-"};

Lexer::Lexer(CompileContext& cc)
  : cc_(cc)
{
    skiplevel_ = 0; /* preprocessor: not currently skipping */
    token_buffer_ = &normal_buffer_;

    const int kStart = tMIDDLE + 1;
    const char** tokptr = &sc_tokens[kStart - tFIRST];
    for (int i = kStart; i <= tLAST; i++, tokptr++) {
        Atom* atom = cc_.atom(*tokptr);
        assert(keywords_.count(atom) == 0);
        keywords_.emplace(atom, i);
    }
}

Lexer::~Lexer() {
    while (!token_caches_.empty()) {
        auto node = *token_caches_.begin();
        token_caches_.remove(node);
        delete node;
    }
}

void Lexer::AddFile(std::shared_ptr<SourceFile> sf) {
    file_queue_.emplace_back(std::move(sf));
}

void Lexer::Init() {
    assert(!file_queue_.empty());

    freading_ = true;

    auto sf = ke::PopFront(&file_queue_);
    EnterFile(std::move(sf), {});
}

void Lexer::Start() {
    defined_atom_ = cc_.atom("defined");
    line_atom_ = cc_.atom("__LINE__");
}

std::string get_token_string(int tok_id) {
    std::string str;
    if (tok_id < 256)
        return StringPrintf("%c", tok_id);
    if (tok_id == tEOL)
        return "<newline>";
    assert(tok_id >= tFIRST && tok_id <= tLAST_TOKEN_ID);
    return StringPrintf("%s", sc_tokens[tok_id - tFIRST]);
}

int Lexer::LexKeywordImpl(Atom* atom) {
    auto iter = keywords_.find(atom);
    if (iter != keywords_.end())
        return iter->second;
    return 0;
}

static inline bool
IsUnimplementedKeyword(int token)
{
    switch (token) {
        case tACQUIRE:
        case tAS:
        case tCATCH:
        case tCAST_TO:
        case tDOUBLE:
        case tEXPLICIT:
        case tFINALLY:
        case tFOREACH:
        case tIMPLICIT:
        case tIMPORT:
        case tIN:
        case tINT8:
        case tINT16:
        case tINT32:
        case tINT64:
        case tINTERFACE:
        case tINTN:
        case tLET:
        case tNAMESPACE:
        case tPACKAGE:
        case tPRIVATE:
        case tPROTECTED:
        case tREADONLY:
        case tSEALED:
        case tTHROW:
        case tTRY:
        case tTYPEOF:
        case tUINT8:
        case tUINT16:
        case tUINT32:
        case tUINT64:
        case tUINTN:
        case tUNION:
        case tVAR:
        case tVARIANT:
        case tVIRTUAL:
        case tVOLATILE:
        case tWITH:
            return true;
        default:
            return false;
    }
}

full_token_t*
Lexer::advance_token_ptr()
{
    assert(token_buffer_->depth == 0);
    token_buffer_->num_tokens++;
    token_buffer_->cursor++;
    if (token_buffer_->cursor == MAX_TOKEN_DEPTH)
        token_buffer_->cursor = 0;

    return current_token();
}

full_token_t*
Lexer::PushSynthesizedToken(TokenKind kind, const token_pos_t& pos)
{
    ke::SaveAndSet<token_buffer_t*> switch_buffer(&token_buffer_, &normal_buffer_);

    token_buffer_->num_tokens++;

    // Now fill it in.
    auto tok = current_token();
    tok->id = kind;
    tok->atom = nullptr;
    tok->start = token_pos_t(pos, state_.tokline);
    lexpush();
    return tok;
}

// Pops a token off the token buffer, making it the current token.
void
Lexer::lexpop()
{
    assert(token_buffer_->depth > 0);

    token_buffer_->depth--;
    token_buffer_->cursor++;
    if (token_buffer_->cursor == MAX_TOKEN_DEPTH)
        token_buffer_->cursor = 0;
}

int Lexer::lex() {
    if (token_buffer_->depth > 0) {
        lexpop();
        return current_token()->id;
    }

    if (using_injected_tokens_) {
        if (!injected_token_stream_.empty())
            return LexInjectedToken();
        return 0;
    }

    return LexNewToken();
}

bool Lexer::freading() const {
    if (using_injected_tokens_)
        return !injected_token_stream_.empty();
    return freading_;
}

int Lexer::LexNewToken() {
    full_token_t* tok = advance_token_ptr();
    *tok = {};

    do {
        if (!FindNextToken()) {
            if (IsPreprocessing() && more()) {
                // We hit the end of the line; preprocessor should not eat more
                // tokens without a continuation.
                FillTokenPos(&tok->start);
                return tok->id = tEOL;
            }

            // Always fill a valid location.
            FillTokenPos(&tok->start);
            return 0;
        }

        // Check for a synthesized token.
        if (token_buffer_->depth > 0) {
            lexpop();
            return current_token()->id;
        }

        tokens_on_line_++;

        FillTokenPos(&tok->start);
        LexIntoToken(tok);

        // Current token may be different if we're in the preproc buffer, so
        // grab it.
        tok = current_token();

        if (tok->id == tSTRING && !in_string_continuation_) {
            LexStringContinuation();
            tok = current_token();
        } else if (tok->id == tDEFINED) {
            LexDefinedKeyword();
            tok = current_token();
        }
    } while (tok->id == tENTERED_MACRO);

    return tok->id;
}

int Lexer::LexInjectedToken() {
    auto tok = advance_token_ptr();
    *tok = ke::PopFront(&injected_token_stream_);

    if (tok->id == tMAYBE_LABEL) {
        if (allow_tags_) {
            tok->id = tLABEL;
            [[maybe_unused]] auto tok = ke::PopFront(&injected_token_stream_);
            assert(tok.id == ':');
        } else {
            tok->id = tSYMBOL;
        }
    }
    return tok->id;
}

void Lexer::FillTokenPos(token_pos_t* pos) {
    uint32_t offset = state_.pos - state_.start;
    if (!state_.macro)
        *pos = token_pos_t(state_.loc_range.FilePos(offset), state_.tokline);
    else
        *pos = token_pos_t(state_.loc_range.MacroPos(offset), state_.tokline);
    assert(pos->valid());
}

void Lexer::LexIntoToken(full_token_t* tok) {
    char c = peek();
    switch (c) {
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9': {
            if (lex_number(tok))
                return;
            break;
        }

        case '*':
            advance();
            if (match_char('='))
                tok->id = taMULT;
            else
                tok->id = '*';
            return;

        case '/':
            advance();
            if (match_char('='))
                tok->id = taDIV;
            else
                tok->id = '/';
            return;

        case '%':
            advance();
            if (match_char('='))
                tok->id = taMOD;
            else
                tok->id = '%';
            return;

        case '+':
            advance();
            if (match_char('='))
                tok->id = taADD;
            else if (match_char('+'))
                tok->id = tINC;
            else
                tok->id = '+';
            return;

        case '-':
            advance();
            if (match_char('='))
                tok->id = taSUB;
            else if (match_char('-'))
                tok->id = tDEC;
            else
                tok->id = '-';
            return;

        case '<':
            advance();
            if (match_char('<')) {
                if (match_char('='))
                    tok->id = taSHL;
                else
                    tok->id = tSHL;
            } else if (match_char('=')) {
                tok->id = tlLE;
            } else {
                tok->id = '<';
            }
            return;

        case '>':
            advance();
            if (match_char('>')) {
                if (match_char('>')) {
                    if (match_char('='))
                        tok->id = taSHRU;
                    else
                        tok->id = tSHRU;
                } else if (match_char('=')) {
                    tok->id = taSHR;
                } else {
                    tok->id = tSHR;
                }
            } else if (match_char('=')) {
                tok->id = tlGE;
            } else {
                tok->id = '>';
            }
            return;

        case '&':
            advance();
            if (match_char('='))
                tok->id = taAND;
            else if (match_char('&'))
                tok->id = tlAND;
            else
                tok->id = '&';
            return;

        case '^':
            advance();
            if (match_char('='))
                tok->id = taXOR;
            else
                tok->id = '^';
            return;

        case '|':
            advance();
            if (match_char('='))
                tok->id = taOR;
            else if (match_char('|'))
                tok->id = tlOR;
            else
                tok->id = '|';
            return;

        case '=':
            advance();
            if (match_char('='))
                tok->id = tlEQ;
            else
                tok->id = '=';
            return;

        case '!':
            advance();
            if (match_char('='))
                tok->id = tlNE;
            else
                tok->id = '!';
            return;

        case '.':
            advance();
            if (match_char('.')) {
                if (match_char('.'))
                    tok->id = tELLIPS;
                else
                    tok->id = tDBLDOT;
            } else {
                tok->id = '.';
            }
            return;

        case ':':
            advance();
            if (match_char(':'))
                tok->id = tDBLCOLON;
            else
                tok->id = ':';
            return;

        case '"':
            LexStringLiteral(tok, 0);
            return;

        case '\'':
            advance(); /* skip quote */
            tok->id = tCHAR_LITERAL;
            tok->numeric_value = litchar(0);
            if (peek() == '\'') {
                advance(); /* skip final quote */
            } else {
                report(27); /* invalid character constant (must be one character) */

                // Eat tokens on the same line until we can close the malformed
                // string.
                while (more() && peek() != '\'')
                    litchar(0);
                if (more() && peek() == '\'')
                    advance();
            }
            return;

        case ';':
            // semicolon resets the error state.
            tok->id = ';';
            advance();
            cc_.reports()->ResetErrorFlag();
            return;
    }

    if (alpha(c) || c == '#') {
        LexSymbolOrKeyword(tok);
        return;
    }

    // Unmatched, return the next character.
    tok->id = advance();
}

bool Lexer::lex_number(full_token_t* tok) {
    cell value = 0;

    int base = 10;
    int ndigits = 0;
    if (match_char('0')) {
        if (match_char('b'))
            base = 2;
        else if (match_char('o'))
            base = 8;
        else if (match_char('x'))
            base = 16;
        else
            ndigits = 1;
    }

    AutoCountErrors errors;

    while (true) {
        char c = peek();
        if (c == '_') {
            advance();
            continue;
        }
        int digit = -1;
        if (c >= '0' && c <= '9')
            digit = c - '0';
        else if (c >= 'A' && c <= 'F')
            digit = c - 'A' + 10;
        else if (c >= 'a' && c <= 'f')
            digit = c - 'a' + 10;

        if (c != '_' && digit < 0)
            break;

        advance();

        if (digit >= base) {
            if (errors.ok())
                report(86);
            continue;
        }
        if (c == '_')
            continue;

        value = (value * base) + digit;
        ndigits++;
    }

    // If there was no leading 0, and we got no digits, then there was no actual
    // number to lex.
    if (base == 10 && !ndigits)
        return false;

    if (alphanum(peek()))
        report(53);
    else if (!ndigits)
        report(424);

    if (base == 10 && match_char('.')) {
        if (IsDigit(peek())) {
            lex_float(tok, value);
            return true;
        }
        backtrack();
    }

    tok->id = tNUMBER;
    tok->numeric_value = value;
    return true;
}

void Lexer::LexStringLiteral(full_token_t* tok, int flags) {
    tok->id = tSTRING;
    tok->atom = nullptr;

    assert(peek() == '\"' || peek() == '\'');

    if (match_char('\"')) {
        packedstring(tok, '\"');
        if (!match_char('\"'))
            report(37);
    } else {
        advance();

        std::string data;
        packedstring_char(&data);
        tok->atom = cc_.atom(data);

        /* invalid char declaration */
        if (!match_char('\''))
            report(27); /* invalid character constant (must be one character) */
    }
}

bool Lexer::LexKeyword(full_token_t* tok, Atom* atom) {
    int tok_id = LexKeywordImpl(atom);
    if (!tok_id)
        return false;

    if (IsUnimplementedKeyword(tok_id)) {
        // Try to gracefully error.
        report(173) << atom;
        tok->id = tSYMBOL;
        tok->atom = atom;
    } else if ((tok_id == tINT || tok_id == tVOID) && match_char(':')) {
        // Special case 'int:' to its old behavior: an implicit view_as<> cast
        // with Pawn's awful lowercase coercion semantics.
        switch (tok_id) {
            case tINT:
                report(238) << atom << atom;
                break;
            case tVOID:
                report(239) << atom << atom;
                break;
        }
        tok->id = tLABEL;
        tok->atom = atom;
    } else {
        tok->id = tok_id;
        cc_.reports()->ResetErrorFlag();
    }
    return true;
}

void Lexer::LexSymbolOrKeyword(full_token_t* tok) {
    unsigned char const* token_start = char_stream();
    char first_char = advance();
    assert(alpha(first_char) || first_char == '#');

    bool maybe_keyword = (first_char != PUBLIC_CHAR) && allow_keywords_;
    while (true) {
        char c = peek();
        if (IsDigit(c)) {
            // Only symbols have numbers, so this terminates a keyword if we
            // started with '#".
            if (first_char == '#')
                break;
            maybe_keyword = false;
        } else if (!isalpha(c) && c != '_') {
            break;
        }
        advance();
    }

    size_t len = char_stream() - token_start;
    if (len == 1 && first_char == PUBLIC_CHAR) {
        tok->id = PUBLIC_CHAR;
        return;
    }

    // Handle preprocessor keywords (ugh).
    Atom* atom = cc_.atom((const char *)token_start, len);
    if (atom == defined_atom_) {
        tok->id = tDEFINED;
        return;
    }

    if (atom == line_atom_) {
        tok->id = tNUMBER;
        tok->numeric_value = state_.fline;
        return;
    }

    if (allow_substitutions_) {
        if (auto macro = FindMacro(atom)) {
            if (EnterMacro(macro))
                return;
        }
    }

    // Handle language keywords or preprocessor entry points.
    if (first_char == '#' || maybe_keyword) {
        if (LexKeyword(tok, atom))
            return;
    }

    if (first_char != '#') {
        LexSymbol(tok, atom);
        return;
    }

    tok->id = 0;
    report(31);
}

void Lexer::LexSymbol(full_token_t* tok, Atom* atom) {
    tok->atom = atom;
    tok->id = tSYMBOL;

    if (peek() == ':' && peek2() != ':') {
        if (caching_tokens_) {
            tok->id = tMAYBE_LABEL;
        } else if (allow_tags_) {
            tok->id = tLABEL;
            advance();
        } else if (cc_.types()->find(atom)) {
            // This looks like a tag override (a tag with this name exists), but
            // tags are not allowed right now, so it is probably an error.
            report(220);
        }
    } else if (atom->str().size() == 1 && atom->str()[0] == '_') {
        // By itself, '_' is not a symbol but a placeholder. However, '_:' is
        // a label which is why we handle this after the label check.
        tok->id = '_';
    }
}

/*  lexpush
 *
 *  Pushes a token back, so the next call to lex() will return the token
 *  last examined, instead of a new token.
 *
 *  Only one token can be pushed back.
 *
 *  In fact, lex() already stores the information it finds into global
 *  variables, so all that is to be done is set a flag that informs lex()
 *  to read and return the information from these variables, rather than
 *  to read in a new token from the input file.
 */
void
Lexer::lexpush()
{
    assert(token_buffer_->depth < MAX_TOKEN_DEPTH);
    if (current_token()->id == 0 || current_token()->id == tEOL)
        return;
    token_buffer_->depth++;
    if (token_buffer_->cursor == 0)
        token_buffer_->cursor = MAX_TOKEN_DEPTH - 1;
    else
        token_buffer_->cursor--;
    assert(token_buffer_->depth <= token_buffer_->num_tokens);
}

/*  lexclr
 *
 *  Sets the variable "_pushed" to 0 to make sure lex() will read in a new
 *  symbol (a not continue with some old one). This is required upon return
 *  from Assembler mode, and in a few cases after detecting an syntax error.
 */
void
Lexer::lexclr(int clreol)
{
    token_buffer_->depth = 0;
    if (clreol) {
        while (lex_same_line() != tEOL)
            continue;
    }
}

// Return true if the symbol is ahead, false otherwise.
bool
Lexer::peek(int id)
{
    if (match(id)) {
        lexpush();
        return true;
    }
    return false;
}

/*  matchtoken
 *
 *  This routine is useful if only a simple check is needed. If the token
 *  differs from the one expected, it is pushed back.
 *  This function returns 1 for "token found" and 2 for "implied statement
 *  termination token" found --the statement termination is an end of line in
 *  an expression where there is no pending operation. Such an implied token
 *  (i.e. not present in the source code) should not be pushed back, which is
 *  why it is sometimes important to distinguish the two.
 */
bool
Lexer::match(int token)
{
    int tok = lex();

    if (token == tok)
        return true;
    if (token == tTERM) {
        if (tok == ';' || tok == tENDEXPR)
            return true;

        lexpush();

        if (!NeedSemicolon() &&
            (!freading_ || peek_same_line() == tEOL))
        {
            // Push "tok" back, because it is the token following the implicit statement
            // termination (newline) token.
            return true;
        }
    } else {
        lexpush();
    }
    return false;
}

/*  needtoken
 *
 *  This routine checks for a required token and gives an error message if
 *  it isn't there (and returns 0/FALSE in that case). Like function matchtoken(),
 *  this function returns 1 for "token found" and 2 for "statement termination
 *  token" found; see function matchtoken() for details.
 */
bool Lexer::need(int token) {
    if (match(token))
        return true;

    int got = token_buffer_->depth == 0 ? 0 : next_token()->id;
    NeedTokenError(token, got);
    return false;
}

void Lexer::NeedTokenError(int token, int got) {
    char s1[20], s2[20];
    if (token < 256)
        SafeSprintf(s1, sizeof(s1), "%c", (char)token); /* single character token */
    else
        SafeStrcpy(s1, sizeof(s1), sc_tokens[token - tFIRST]); /* multi-character symbol */
    if (!freading_)
        SafeStrcpy(s2, sizeof(s2), "-end of file-");
    else if (got < 256)
        SafeSprintf(s2, sizeof(s2), "%c", (char)got);
    else
        SafeStrcpy(s2, sizeof(s2), sc_tokens[got - tFIRST]);
    report(1) << s1 << s2; /* expected ..., but found ... */
}

// If the next token is on the current line, return that token. Otherwise,
// return tNEWLINE.
int Lexer::peek_same_line() {
    // We should not call this without having parsed at least one token.
    assert(token_buffer_->num_tokens > 0);

    // If there's tokens pushed back, then |fline| is the line of the furthest
    // token parsed. If fline == current token's line, we are guaranteed any
    // buffered token is still on the same line.
    if (token_buffer_->depth > 0 &&
        current_token()->start.line == state_.fline &&
        IsSameSourceFile(current_token()->start, next_token()->start))
    {
        return next_token()->id ? next_token()->id : tEOL;
    }

    // Make sure the next token is lexed, then buffer it.
    full_token_t next = lex_tok();
    if (next.id == 0 || next.id == tEOL)
        return tEOL;

    lexpush();

    // If the next token starts on the line the last token ends, then the next
    // token is considered on the same line.
    if (next.start.line == current_token()->start.line &&
        IsSameSourceFile(current_token()->start, next_token()->start))
    {
        return next.id;
    }

    return tEOL;
}

int
Lexer::lex_same_line()
{
    if (peek_same_line() == tEOL)
        return tEOL;

    return lex();
}

bool Lexer::match_same_line(int tok) {
    if (peek_same_line() != tok)
        return false;
    lex_same_line();
    return true;
}

bool Lexer::need_same_line(int tok) {
    int got = peek_same_line();
    if (tok == got) {
        lex_same_line();
        return true;
    }
    NeedTokenError(tok, got);
    return false;
}

int
Lexer::require_newline(TerminatorPolicy policy)
{
    if (policy != TerminatorPolicy::Newline) {
        // Semicolon must be on the same line.
        auto pos = current_token()->start;
        int next_tok_id = peek_same_line();
        if (next_tok_id == ';') {
            lexpop();
        } else if (policy == TerminatorPolicy::Semicolon && NeedSemicolon()) {
            report(pos, 1) << ";" << get_token_string(next_tok_id);
        }
    }

    int tokid = peek_same_line();
    if (tokid == tEOL || tokid == 0)
        return TRUE;

    // Eat an incorrect semicolon just so we can continue parsing.
    if (tokid == ';' && policy == TerminatorPolicy::Newline)
        lex_same_line();

    char s[20];
    if (tokid < 256)
        SafeSprintf(s, sizeof(s), "%c", (char)tokid);
    else
        SafeStrcpy(s, sizeof(s), sc_tokens[tokid - tFIRST]);
    report(155) << s;
    return FALSE;
}

void
litadd_str(const char* str, size_t len, std::vector<cell>* out)
{
    StringToCells(str, len, [out](cell val) -> void {
        out->emplace_back(val);
    });
}

/*  litchar
 *
 *  Return current literal character and increase the pointer to point
 *  just behind this literal character.
 *
 *  Note: standard "escape sequences" are suported, but the backslash may be
 *        replaced by another character; the syntax '\ddd' is supported,
 *        but ddd must be decimal!
 */
cell Lexer::litchar(int flags, bool* is_codepoint) {
    cell c = 0;
    bool tmp_codepoint;

    if (!is_codepoint)
        is_codepoint = &tmp_codepoint;
    *is_codepoint = false;

    if (!match_char(ctrlchar_)) { /* no escape character */
        cell raw = peek_unsigned();
        if ((flags & kLitcharUtf8) && !(flags & kLitcharSkipping)) {
            if (raw > 0x7f) {
                auto saved_pos = char_stream();
                auto c = get_utf8_char();
                if (c >= 0) {
                    *is_codepoint = true;
                    return c;
                }
                report(248);

                // Restore the character position and treat this as a raw byte.
                state_.pos = saved_pos;
            }
        } 

        assert(raw >= 0);
        advance();
        return raw;
    }

    if (match_char(ctrlchar_))
        return ctrlchar_;

    char ch = advance();
    switch (ch) {
        case 'a': /* \a == audible alarm */
            c = 7;
            break;
        case 'b': /* \b == backspace */
            c = 8;
            break;
        case 'e': /* \e == escape */
            c = 27;
            break;
        case 'f': /* \f == form feed */
            c = 12;
            break;
        case 'n': /* \n == NewLine character */
            c = 10;
            break;
        case 'r': /* \r == carriage return */
            c = 13;
            break;
        case 't': /* \t == horizontal TAB */
            c = 9;
            break;
        case 'v': /* \v == vertical TAB */
            c = 11;
            break;
        case 'x': {
            int digits = 0;
            c = 0;
            while (digits < 2) {
                char ch = peek();
                if (!ishex(ch))
                    break;
                if (IsDigit(ch))
                    c = (c << 4) + (ch - '0');
                else
                    c = (c << 4) + (tolower(ch) - 'a' + 10);
                advance();
                digits++;
            }
            match_char(';'); /* swallow a trailing ';' */
            break;
        }
        case 'u':
        case 'U': {
            int digits = (ch == 'u') ? 4 : 8;
            for (int i = 1; i <= digits; i++) {
                c <<= 4;
                char ch = peek();
                if (ch >= '0' && ch <= '9') {
                    c |= (ch - '0');
                } else if (ch >= 'a' && ch <= 'f') {
                    c |= 10 + (ch - 'a');
                } else if (ch >= 'A' && ch <= 'F') {
                    c |= 10 + (ch - 'A');
                } else {
                    report(27);
                    break;
                }
                advance();
            }
            *is_codepoint = true;
            break;
        }
        case '\'': /* \' == ' (single quote) */
        case '"':  /* \" == " (single quote) */
        case '%':  /* \% == % (percent) */
            c = ch;
            break;
        default:
            // Back up.
            backtrack();
            if (IsDigit(ch)) { /* \ddd */
                c = 0;
                int ndigits = 0;
                while (true) {
                    char ch = peek();
                    if (ch < '0' || ch > '9')
                        break;
                    c = c * 10 + (ch - '0');
                    advance();
                    ndigits++;
                }
                // max 3-digit codes only, save for nul terminator special case.
                if (ndigits > 3 && !(flags & kLitcharSkipping))
                    report(27);
                match_char(';'); /* swallow a trailing ';' */
                if (c > 0xff && !(flags & kLitcharSkipping)) {
                    report(27);
                    c = 0;
                }
            } else {
                report(27); /* invalid character constant */
            }
    }

    assert(c >= 0);
    return c;
}

/*  alpha
 *
 *  Test if character "c" is alphabetic ("a".."z"), an underscore ("_")
 *  or an "at" sign ("@"). The "@" is an extension to standard C.
 */
int
alpha(char c)
{
    return (isalpha(c) || c == '_' || c == PUBLIC_CHAR);
}

/*  alphanum
 *
 *  Test if character "c" is alphanumeric ("a".."z", "0".."9", "_" or "@")
 */
int
alphanum(char c)
{
    return (alpha(c) || IsDigit(c));
}

/*  ishex
 *
 *  Test if character "c" is a hexadecimal digit ("0".."9" or "a".."f").
 */
int
ishex(char c)
{
    return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
}

/*  isoctal
 *
 *  Test if character "c" is an octal digit ("0".."7").
 */
int
isoctal(char c)
{
    return (c >= '0' && c <= '7');
}

bool
Lexer::matchsymbol(Atom** name)
{
    if (lex() != tSYMBOL) {
        lexpush();
        return false;
    }
    *name = current_token()->atom;
    return true;
}

bool
Lexer::needsymbol(Atom** name)
{
    if (!need(tSYMBOL)) {
        *name = cc_.atom("__unknown__");
        return false;
    }
    *name = current_token()->atom;
    return true;
}

void Lexer::AddMacro(const char* pattern, const char* subst) {
    auto atom = cc_.atom(pattern);
    auto macro = std::make_shared<MacroEntry>();
    macro->pattern = atom;
    macro->substitute = cc_.atom(subst);
    macro->deprecated = false;

    macros_[atom] = std::move(macro);
}

std::shared_ptr<Lexer::MacroEntry> Lexer::FindMacro(Atom* atom) {
    auto p = macros_.find(atom);
    if (p == macros_.end())
        return nullptr;

    return p->second;
}

bool Lexer::DeleteMacro(Atom* atom) {
    auto p = macros_.find(atom);
    if (p == macros_.end())
        return false;

    macros_.erase(p);
    return true;
}

bool
Lexer::NeedSemicolon()
{
    if (cc_.options()->need_semicolon)
        return true;
    return state_.need_semicolon;
}

void Lexer::EnterFile(std::shared_ptr<SourceFile>&& sf, const token_pos_t& from) {
    auto& cc = CompileContext::get();

    state_.inpf = std::move(sf);
    state_.loc_range = cc_.sources()->EnterFile(state_.inpf, from);
    state_.need_semicolon = cc.options()->need_semicolon;
    state_.require_newdecls = cc.options()->require_newdecls;
    state_.fline = 1;
    state_.tokline = 1;
    state_.start = state_.inpf->data();
    state_.end = state_.start + state_.inpf->size();
    state_.pos = state_.start;
    state_.line_start = state_.pos;
    SkipUtf8Bom();
    SetFileDefines(state_.inpf);

    state_.inpf->set_included();

    tokens_on_line_ = 0;
}

void Lexer::PushLexerState() {
    prev_state_.emplace_back(std::move(state_));
}

cell Lexer::get_utf8_char() {
    unsigned char ch = advance();
    if (ch <= 0x7f)
        return ch;

    // First byte starts with 11, then up to 4 additional 1s, and then a zero.
    // By inverting we can find the position of the zero.
    unsigned char inverted = (~ch) & 0xff;

    if (!inverted)
        return -1;

    unsigned int indicator_bit = ke::FindLeftmostBit32(inverted);
    if (indicator_bit == 0 || indicator_bit > 5)
        return -1;

    unsigned int mask = (1 << indicator_bit) - 1;
    cell result = ch & mask;

    unsigned int extra_bytes = 6 - indicator_bit;
    for (unsigned int i = 1; i <= extra_bytes; i++) {
        unsigned char ch = peek();

        if ((ch & 0xc0) != 0x80) {
            result = -1;
            break;
        }

        result <<= 6;
        result |= (ch & 0x3f);
        advance();
    }

    return result;
}

void Lexer::LexStringContinuation() {
    ke::SaveAndSet<bool> stop_recursion(&in_string_continuation_, true);

    if (!peek(tELLIPS))
        return;

    auto initial = std::move(*current_token());
    assert(initial.id == tSTRING);

    std::string data = initial.data();
    while (match(tELLIPS)) {
        if (match(tCHAR_LITERAL)) {
            data.push_back(current_token()->value());
            continue;
        }
        if (!need(tSTRING)) {
            lexpush();
            break;
        }
        data += current_token()->data();
    }

    *current_token() = std::move(initial);
    current_token()->atom = cc_.atom(data);
}

bool Lexer::HasMacro(Atom* atom) {
    return !!FindMacro(atom);
}

void Lexer::LexDefinedKeyword() {
    auto initial = *current_token();
    Atom* symbol = nullptr;
    {
        ke::SaveAndSet<bool> stop_recursion(&allow_substitutions_, false);
        ke::SaveAndSet<token_buffer_t*> switch_buffers(&token_buffer_, &preproc_buffer_);

        assert(token_buffer_->depth == 0);

        int nparens = 0;
        while (match('('))
            nparens++;

        if (!needsymbol(&symbol))
            return;

        for (int i = 0; i < nparens; i++)
            need(')');
    }

    initial.id = tNUMBER;
    initial.numeric_value = HasMacro(symbol) ? 1 : 0;
    *current_token() = initial;
}

bool Lexer::EnterMacro(std::shared_ptr<MacroEntry> macro) {
    assert(allow_substitutions_);

    ke::SaveAndSet<bool> no_eof(&allow_end_of_file_, false);

    auto expansion_pos = current_token()->start;

    if (macros_in_use_.count(macro.get()))
        return false;

    std::unordered_map<int, std::string> macro_args;
    if (macro->args) {
        if (!match('('))
            return false;

        auto saved_pos = pos();

        for (const auto& argn : macro->args.get()) {
            auto arg_str = SkimMacroArgument();
            if (argn != macro->args.get().back()) {
                if (!need(','))
                    break;
            }
            macro_args.emplace(argn, std::move(arg_str));
        }
        need(')');

        if (macro_args.size() != macro->args.get().size()) {
            report(saved_pos, 429) << macro->args.get().size() << macro_args.size();
            return false;
        }
    }

    PushLexerState();

    auto& prev_state = prev_state_.back();

    Atom* text = nullptr;
    if (macro->args) {
        // Atomization is important here since it keeps the macro text alive
        // during lexing, since we do not pre-lex its tokens.
        //
        // We used to not atomize here, in which case it was stored on the
        // lexer state.
        text = cc_.atom(PerformMacroSubstitution(macro.get(), macro_args));
    } else {
        text = macro->substitute;
    }
    state_.start = reinterpret_cast<const unsigned char*>(text->chars());
    state_.end = state_.start + text->length();
    state_.line_start = state_.start;
    state_.pos = state_.start;
    state_.macro = macro;
    state_.inpf = prev_state.inpf;
    state_.fline = prev_state.fline;
    state_.tokline = prev_state.tokline;
    state_.loc_range = cc_.sources()->EnterMacro(macro->pos, expansion_pos, text);

    // Save any tokens we peeked ahead.
    prev_state.token_buffer = token_buffer_;
    while (token_buffer_->depth > 0) {
        lexpop();
        prev_state.saved_tokens.emplace_back(std::move(*current_token()));
    }

    macros_in_use_.emplace(macro.get());

    current_token()->id = tENTERED_MACRO;
    return true;
}

std::string Lexer::SkimMacroArgument() {
    std::string text;

    const unsigned char* start = nullptr;
    int nparens = 0;
    while (freading()) {
        char c = peek();
        if (c == '\0')
            break;
        if (c == '/' && peek2() == '/') {
            AddText(&text, &start, char_stream(), ' ');
            HandleSingleLineComment();
            continue;
        } else if (c == '/' && peek2() == '*') {
            AddText(&text, &start, char_stream(), ' ');
            HandleMultiLineComment();
            continue;
        } else if (IsNewline(c)) {
            HandleNewline(c, '\0');
            AddText(&text, &start, char_stream(), ' ');
            continue;
        } else if (c == '\\') {
            auto end = char_stream();
            if (MaybeHandleLineContinuation()) {
                AddText(&text, &start, end, ' ');
                continue;
            }
        } else if (c == '(') {
            nparens++;
        } else if (c == ')') {
            if (nparens == 0)
                break;
            nparens--;
        } else if (c == ',' && !nparens) {
            break;
        }

        if (!start && !IsSpace(c))
            start = char_stream();

        advance();
    }

    AddText(&text, &start, char_stream(), '\0');
    return text;
}

std::string Lexer::PerformMacroSubstitution(MacroEntry* macro,
                                            const std::unordered_map<int, std::string>& args)
{
    std::string out;

    size_t last_start = 0;
    const auto& substitute = macro->substitute->str();
    for (const auto& pos : macro->arg_positions) {
        assert(pos >= last_start);
        assert(substitute[pos] == '%');
        assert(IsDigit(substitute[pos + 1]));

        // Position where the last text run ends.
        size_t last_end = pos;

        // If #%n, chop the # from the text run.
        bool stringize = false;
        if (pos > 0 && substitute[pos - 1] == '#') {
            last_end--;
            stringize = true;
        }

        out += substitute.substr(last_start, last_end - last_start);
        last_start = pos + 2;

        char arg_pos = substitute[pos + 1] - '0';
        auto iter = args.find(arg_pos);
        if (iter == args.end()) {
            out.push_back(substitute[pos]);
            out.push_back(substitute[pos + 1]);
            continue;
        }
        if (stringize)
            out += '"' + iter->second + '"';
        else
            out += iter->second;
    }

    out += substitute.substr(last_start);
    return out;
}

void Lexer::SkipUtf8Bom() {
    if (state_.pos[0] == 0xef && state_.pos[1] == 0xbb && state_.pos[2] == 0xbf)
        state_.pos += 3;
}

bool Lexer::IsSameSourceFile(const token_pos_t& a, const token_pos_t& b) {
    // Almost always, we'll be looking at the most recent location. peek_same_line
    // is extremely hot so keep this fast-path fast.
    if (state_.loc_range.owns(a) && state_.loc_range.owns(b))
        return true;
    return cc_.sources()->IsSameSourceFile(a, b);
}

void Lexer::AssertCleanState() {
    assert(allow_keywords_);
    assert(allow_substitutions_);
    assert(!in_string_continuation_);
    assert(allow_tags_);
    assert(injected_token_stream_.empty());
    assert(!using_injected_tokens_);
}

TokenCache* Lexer::LexFunctionBody() {
    TokenCache* cache = new TokenCache;
    cache->require_newdecls = state_.require_newdecls;
    cache->need_semicolon = state_.need_semicolon;

    // To cache tokens we must be assured that the lexer state contains no
    // surprises, otherwise, the uncached stream may resolve incorrectly.
    AssertCleanState();

    assert(current_token()->id == '{');
    cache->tokens.emplace_back(std::move(*current_token()));

    ke::SaveAndSet<bool> caching_tokens(&caching_tokens_, true);

    int brace_balance = 1;
    while (freading_) {
        int tok = lex();
        if (tok == 0)
            break;
        cache->tokens.emplace_back(std::move(*current_token()));

        if (tok == '{') {
            brace_balance++;
        } else if (tok == '}') {
            brace_balance--;
            if (brace_balance == 0)
                break;
        }
    }

    cache->tokens.shrink_to_fit();
    token_caches_.append(cache);
    return cache;
}

void Lexer::InjectCachedTokens(TokenCache* cache) {
    AssertCleanState();

    injected_token_stream_ = std::move(cache->tokens);
    using_injected_tokens_ = true;
    token_caches_.remove(cache);
    delete cache;

    freading_ = true;
}

void Lexer::DiscardCachedTokens() {
    using_injected_tokens_ = false;
    injected_token_stream_.clear();
}

} // namespace cc
} // namespace sp
