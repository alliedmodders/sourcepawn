// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
//  Copyright (c) ITB CompuPhase, 1997-2006
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

#include <filesystem>

#include <amtl/am-deque.h>
#include <amtl/am-hashtable.h>
#include <amtl/am-string.h>
#include <amtl/am-vector.h>
#include <amtl/am-inlinelist.h>
#include <shared/string-pool.h>

#include "compile-options.h"
#include "sc.h"
#include "source-location.h"
#include "source-manager.h"

namespace sp {
namespace cc {

class CompileContext;
class Type;

struct full_token_t {
    int id = 0;
    union {
        Atom* atom = nullptr;
        int numeric_value; // Set on tRATIONAL, tNUMBER, and tCHAR_LITERAL.
    };
    int value();
    token_pos_t start;
    const std::string& data() const {
        return atom->str();
    }
};

#define MAX_TOKEN_DEPTH 4

struct token_buffer_t {
    // Total number of tokens parsed.
    int num_tokens = 0;

    // Number of tokens that we've rewound back to.
    int depth = 0;

    // Most recently fetched token.
    int cursor = 0;

    // Circular token buffer.
    full_token_t tokens[MAX_TOKEN_DEPTH];
};

/*  Tokens recognized by lex()
 *  Some of these constants are assigned as well to the variable "lastst" (see SC1.C)
 */
enum TokenKind {
    /* value of first multi-character operator */
    tFIRST = 256,
    /* multi-character operators */
    taMULT = tFIRST, /* *= */
    taDIV,           /* /= */
    taMOD,           /* %= */
    taADD,           /* += */
    taSUB,           /* -= */
    taSHL,           /* <<= */
    taSHRU,          /* >>>= */
    taSHR,           /* >>= */
    taAND,           /* &= */
    taXOR,           /* ^= */
    taOR,            /* |= */
    tlOR,            /* || */
    tlAND,           /* && */
    tlEQ,            /* == */
    tlNE,            /* != */
    tlLE,            /* <= */
    tlGE,            /* >= */
    tSHL,            /* << */
    tSHRU,           /* >>> */
    tSHR,            /* >> */
    tINC,            /* ++ */
    tDEC,            /* -- */
    tELLIPS,         /* ... */
    tDBLDOT,         /* .. */
    tDBLCOLON,       /* :: */
    /* value of last multi-character operator */
    tMIDDLE = tDBLCOLON,
    /* reserved words (statements) */
    tACQUIRE,
    tAS,
    tASSERT,
    tBREAK,
    tBUILTIN,
    tCATCH,
    tCASE,
    tCAST_TO,
    tCHAR,
    tCONST,
    tCONTINUE,
    tDECL,
    tDEFAULT,
    tDEFINED,
    tDELETE,
    tDO,
    tDOUBLE,
    tELSE,
    tENUM,
    tEXIT,
    tEXPLICIT,
    tFALSE,
    tFINALLY,
    tFOR,
    tFOREACH,
    tFORWARD,
    tFUNCENUM,
    tFUNCTAG,
    tFUNCTION,
    tGOTO,
    tIF,
    tIMPLICIT,
    tIMPORT,
    tIN,
    tINT,
    tINT8,
    tINT16,
    tINT32,
    tINT64,
    tINTERFACE,
    tINTN,
    tINVALID_FUNCTION,
    tLET,
    tMETHODMAP,
    tNAMESPACE,
    tNATIVE,
    tNEW,
    tNULL,
    tNULLABLE,
    tOBJECT,
    tOPERATOR,
    tPACKAGE,
    tPRIVATE,
    tPROTECTED,
    tPUBLIC,
    tREADONLY,
    tRETURN,
    tSEALED,
    tSIZEOF,
    tSTATIC,
    tSTATIC_ASSERT,
    tSTOCK,
    tSTRUCT,
    tSWITCH,
    tTHIS,
    tTHROW,
    tTRUE,
    tTRY,
    tTYPEDEF,
    tTYPEOF,
    tTYPESET,
    tUINT8,
    tUINT16,
    tUINT32,
    tUINT64,
    tUINTN,
    tUNION,
    tUSING,
    tVAR,
    tVARIANT,
    tVIEW_AS,
    tVIRTUAL,
    tVOID,
    tVOLATILE,
    tWHILE,
    tWITH,
    /* compiler directives */
    tpASSERT, /* #assert */
    tpDEFINE,
    tpELSE,   /* #else */
    tpELSEIF, /* #elseif */
    tpENDIF,
    tpENDINPUT,
    tpENDSCRPT,
    tpERROR,
    tpWARNING,
    tpIF, /* #if */
    tINCLUDE,
    tpLINE,
    tpPRAGMA,
    tpTRYINCLUDE,
    tpUNDEF,
    tLAST = tpUNDEF, /* value of last multi-character match-able token */
    /* semicolon is a special case, because it can be optional */
    tTERM,    /* semicolon or newline */
    tENDEXPR, /* forced end of expression */
    /* other recognized tokens */
    tNUMBER,   /* integer number */
    tRATIONAL, /* rational number */
    tSYMBOL,
    tLABEL,
    tSTRING,
    tCHAR_LITERAL,
    tSYN_PRAGMA_UNUSED,
    tSYN_INCLUDE_PATH,
    tEOL,            /* newline, only returned by peek_new_line() */
    tNEWDECL,        /* for declloc() */
    tENTERED_MACRO,  /* internal lexer command */
    tMAYBE_LABEL,    /* internal lexer command, followed by ':' */
    // Make sure to update the token list in lexer.cpp.
    tLAST_TOKEN_ID
};

inline int full_token_t::value() {
    assert(id == tRATIONAL || id == tNUMBER || id == tCHAR_LITERAL);
    return numeric_value;
}

static inline bool
IsChainedOp(int token)
{
    switch (token) {
        case tlGE:
        case tlLE:
        case '>':
        case '<':
            return true;
        default:
            return false;
    }
}

static inline bool
IsAssignOp(int token)
{
    switch (token) {
        case taMULT:
        case taDIV:
        case taMOD:
        case taADD:
        case taSUB:
        case taSHL:
        case taSHR:
        case taSHRU:
        case taAND:
        case taXOR:
        case taOR:
        case '=':
            return true;
        default:
            return false;
    }
}

void litadd_str(const char* str, size_t len, std::vector<cell>* out);
int alphanum(char c);
int ishex(char c);
int isoctal(char c);
int getlabel(void);
std::string get_token_string(int tok_id);
int alpha(char c);

enum class TerminatorPolicy {
    Newline,
    NewlineOrSemicolon,
    Semicolon
};

static constexpr int SKIPMODE = 1;     /* bit field in "#if" stack */
static constexpr int PARSEMODE = 2;    /* bit field in "#if" stack */
static constexpr int HANDLED_ELSE = 4; /* bit field in "#if" stack */

struct TokenCache : public ke::InlineListNode<TokenCache> {
    std::deque<full_token_t> tokens;
    bool require_newdecls;
    bool need_semicolon;
};

class Lexer
{
    friend class MacroProcessor;

  public:
    Lexer(CompileContext& cc);
    ~Lexer();

    void AddFile(std::shared_ptr<SourceFile> sf);
    void PlungeFile(std::shared_ptr<SourceFile> sf);

    int lex();
    int lex_same_line();
    bool match_same_line(int tok);
    bool need_same_line(int tok);
    bool peek(int id);
    bool match(int token);
    bool need(int token);
    bool matchsymbol(Atom** atom);
    bool needsymbol(Atom** atom);
    int require_newline(TerminatorPolicy policy);
    int peek_same_line();
    void lexpush();
    void lexclr(int clreol);

    void Init();
    void Start();
    bool PlungeFile(const token_pos_t& from, const std::string& name, int try_currentpath,
                    int try_includepaths);
    std::shared_ptr<SourceFile> OpenFile(const token_pos_t& from, const std::string& name);
    bool NeedSemicolon();
    void LexStringContinuation();
    void LexDefinedKeyword();
    bool HasMacro(Atom* atom);

    // Lexer must be at a '{' token. Lexes until it reaches a balanced '}' token,
    // and returns a pointer to the cached tokens.
    //
    // The opening '{' token, even though already lexed, will be re-added to the
    // stream. This is to avoid significantly changing parse_stmt.
    TokenCache* LexFunctionBody();

    // Consumes a TokenCache. The pointer is deleted after. Consumed tokens will
    // be replayed by lex().
    void InjectCachedTokens(TokenCache* cache);

    // Throw away tokens injected by InjectCachedTokens().
    void DiscardCachedTokens();

    full_token_t lex_tok() {
        lex();
        return *current_token();
    }
    full_token_t* current_token() {
        return &token_buffer_->tokens[token_buffer_->cursor];
    }
    const token_pos_t stream_loc() const;
    const token_pos_t& pos() { return current_token()->start; }
    std::string& deprecate() { return deprecate_; }
    bool& allow_tags() { return allow_tags_; }
    bool& require_newdecls() { return state_.require_newdecls; }
    bool& need_semicolon() { return state_.need_semicolon; }
    bool freading() const;
    int fcurrent() const { return state_.inpf->sources_index(); }
    unsigned fline() const { return state_.fline; }
    SourceFile* inpf() const { return state_.inpf.get(); }

    unsigned char const* char_stream() const { return state_.pos; }
    unsigned char const* line_start() const { return state_.line_start; }

  private:
    bool FindNextToken();
    void HandleNewline(char c, char continuation);
    void HandleSingleLineComment();
    void HandleMultiLineComment();
    void HandleDirectives();
    void HandleEof();
    void HandleSkippedSection();
    int LexNewToken();
    int LexInjectedToken();
    void LexIntoToken(full_token_t* tok);
    void LexSymbolOrKeyword(full_token_t* tok);
    int LexKeywordImpl(Atom* atom);
    bool LexKeyword(full_token_t* tok, Atom* atom);
    void LexStringLiteral(full_token_t* tok, int flags);
    void LexSymbol(full_token_t* tok, Atom* atom);
    bool MaybeHandleLineContinuation();
    bool PlungeQualifiedFile(const token_pos_t& from, const std::string& name);
    full_token_t* PushSynthesizedToken(TokenKind kind, const token_pos_t& pos);
    void SynthesizeIncludePathToken();
    void SetFileDefines(const std::shared_ptr<SourceFile> file);
    void EnterFile(std::shared_ptr<SourceFile>&& fp, const token_pos_t& from);
    void FillTokenPos(token_pos_t* pos);
    void SkipLineWhitespace();
    std::string SkimUntilEndOfLine(tr::vector<size_t>* macro_args = nullptr);
    std::string SkimMacroArgument();
    void CheckLineEmpty(bool allow_semi = false);
    void NeedTokenError(int expected, int got);
    void SkipUtf8Bom();
    void PushLexerState();
    bool IsSameSourceFile(const token_pos_t& a, const token_pos_t& b);
    void AssertCleanState();

    full_token_t* advance_token_ptr();
    full_token_t* next_token();
    void lexpop();
    int preproc_expr(cell* val, Type** type);
    void substallpatterns(unsigned char* line, int buffersize);
    bool substpattern(unsigned char* line, size_t buffersize, const char* pattern,
                      const char* substitution, int& patternLen, int& substLen);
    bool lex_number(full_token_t* tok);
    void lex_float(full_token_t* tok, cell whole);
    cell litchar(int flags, bool* is_codepoint = nullptr);
    void packedstring(full_token_t* tok, char term);
    void packedstring_char(std::string* data);

    bool IsSkipping() const {
        return skiplevel_ > 0 && (ifstack_[skiplevel_ - 1] & SKIPMODE) == SKIPMODE;
    }
    bool IsPreprocessing() const { return token_buffer_ == &preproc_buffer_; }

    bool match_char(char c) {
        if (peek() == c) {
            advance();
            return true;
        }
        return false;
    }
    char advance() {
        assert(state_.pos < state_.end);
        return *state_.pos++;
    }
    char peek() const {
        assert(state_.pos <= state_.end);
        return *state_.pos;
    }
    char peek2() const {
        assert(state_.pos < state_.end);
        return *(state_.pos + 1);
    }
    unsigned char peek_unsigned() const {
        assert(state_.pos <= state_.end);
        return *state_.pos;
    }
    bool more() const {
        return state_.pos < state_.end;
    }
    void backtrack() {
        assert(state_.pos > state_.start);
        state_.pos--;
    }
    void backtrack(const unsigned char* pos) {
        assert(pos >= state_.line_start);
        assert(pos <= state_.end);
        state_.pos = pos;
    }
    unsigned int column() const { return (unsigned)(state_.pos - state_.line_start); }
    cell get_utf8_char();

  private:
    struct MacroEntry {
        Atom* pattern = nullptr;
        ke::Maybe<tr::vector<int>> args;
        tr::vector<size_t> arg_positions;
        Atom* substitute = nullptr;
        std::string documentation;
        token_pos_t pos;
        bool deprecated;
    };
    std::shared_ptr<MacroEntry> FindMacro(Atom* atom);
    void AddMacro(const char* pattern, const char* subst);
    bool DeleteMacro(Atom* atom);
    bool EnterMacro(std::shared_ptr<MacroEntry> macro);
    bool IsInMacro() const { return state_.macro != nullptr; }
    std::string PerformMacroSubstitution(MacroEntry* macro,
                                         const std::unordered_map<int, std::string>& args);

  private:
    CompileContext& cc_;
    tr::unordered_map<Atom*, int> keywords_;
    std::vector<char> ifstack_;
    size_t iflevel_;             /* nesting level if #if/#else/#endif */
    size_t skiplevel_; /* level at which we started skipping (including nested #if .. #endif) */
    std::string deprecate_;
    bool allow_tags_ = true;
    bool freading_ = false;
    int ctrlchar_ = CTRL_CHAR;
    unsigned int tokens_on_line_ = 0;
    bool in_string_continuation_ = false;
    bool allow_substitutions_ = true;
    bool allow_end_of_file_ = true;
    bool allow_keywords_ = true;
    Atom* defined_atom_ = nullptr;
    Atom* line_atom_ = nullptr;

    token_buffer_t normal_buffer_;;
    token_buffer_t preproc_buffer_;
    token_buffer_t* token_buffer_;

    tr::unordered_map<Atom*, std::shared_ptr<MacroEntry>> macros_;
    std::unordered_set<MacroEntry*> macros_in_use_;

    struct LexerState {
        LexerState() {}
        LexerState(const LexerState&) = delete;
        LexerState(LexerState&&) = default;
        void operator =(const LexerState &) = delete;
        LexerState& operator =(LexerState&&) = default;

        std::shared_ptr<SourceFile> inpf;
        LocationRange loc_range;
        // Visual line in the file.
        int fline = 0;
        // Line # for token processing.
        int tokline = 0;
        bool need_semicolon = false;
        bool is_line_start = false;
        bool require_newdecls = false;
        size_t entry_preproc_if_stack_size = 0;
        const unsigned char* start = nullptr;
        const unsigned char* end = nullptr;
        const unsigned char* pos = nullptr;
        const unsigned char* line_start = nullptr;
        tr::vector<full_token_t> saved_tokens;
        token_buffer_t* token_buffer = nullptr;

        // Macro specific.
        std::shared_ptr<MacroEntry> macro;
    };

    LexerState state_;
    std::deque<std::shared_ptr<SourceFile>> file_queue_;
    tr::vector<LexerState> prev_state_;

    // Set if tokens are being lexed into a new token cache.
    bool caching_tokens_ = false;
    ke::InlineList<TokenCache> token_caches_;

    std::deque<full_token_t> injected_token_stream_;
    bool using_injected_tokens_ = false;
};

std::string StringizePath(const std::filesystem::path& in_path);

} // namespace cc
} // namespace sp
