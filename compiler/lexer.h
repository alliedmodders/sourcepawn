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

#include <amtl/am-hashtable.h>
#include <amtl/am-string.h>
#include <amtl/am-vector.h>
#include <shared/string-pool.h>

#include "compile-options.h"
#include "sc.h"

class CompileContext;
class Type;

struct token_pos_t {
    int file = 0;
    int line = 0;
    int col = 0;
};

struct full_token_t {
    int id = 0;
    int value = 0;
    std::string data;
    sp::Atom* atom = nullptr;
    token_pos_t start;
    token_pos_t end;
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
    tEXPR,           /* for assigment to "lastst" only (see SC1.C) */
    tSYN_PRAGMA_UNUSED,
    tSYN_INCLUDE_PATH,
    tENDLESS,        /* endless loop, for assigment to "lastst" only */
    tEMPTYBLOCK,     /* empty blocks for AM bug 4825 */
    tEOL,            /* newline, only returned by peek_new_line() */
    tNEWDECL,        /* for declloc() */
    tLAST_TOKEN_ID
};

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

class Lexer
{
    friend class MacroProcessor;

  public:
    Lexer(CompileContext& cc);

    int lex();
    int lex_same_line();
    bool peek(int id);
    bool match(int token);
    bool need(int token);
    bool matchsymbol(sp::Atom** atom);
    bool needsymbol(sp::Atom** atom);
    int require_newline(TerminatorPolicy policy);
    int peek_same_line();
    void lexpush();
    void lexclr(int clreol);

    void Init(std::shared_ptr<SourceFile> sf);
    void Start();
    bool PlungeFile(const char* name, int try_currentpath, int try_includepaths);
    void Preprocess(bool allow_synthesized_tokens);
    void AddMacro(const char* pattern, size_t length, const char* subst);
    bool NeedSemicolon();

    full_token_t lex_tok() {
        lex();
        return *current_token();
    }
    full_token_t* current_token() {
        return &token_buffer_->tokens[token_buffer_->cursor];
    }
    const token_pos_t& pos() { return current_token()->start; }
    std::vector<bool>& need_semicolon_stack() { return need_semicolon_stack_; }
    std::string& deprecate() { return deprecate_; }
    bool& allow_tags() { return allow_tags_; }
    int& require_newdecls() { return require_newdecls_stack_.back(); }
    int& stmtindent() { return stmtindent_; }
    bool& indent_nowarn() { return indent_nowarn_; }
    bool freading() const { return freading_; }
    int fcurrent() const { return fcurrent_; }
    unsigned fline() const { return fline_; }
    unsigned char* pline() { return pline_; }

    bool HasMacro(sp::Atom* name) { return FindMacro(name->chars(), name->length(), nullptr); }

    struct macro_t {
        const char* first;
        const char* second;
    };

    const unsigned char* lptr = nullptr;                  /* points to the current position in "pline" */

  private:
    void LexOnce(full_token_t* tok);
    void PreprocessInLex(bool allow_synthesized_tokens);
    void Readline(unsigned char* line);
    void StripComments(unsigned char* line);
    int DoCommand(bool allow_synthesized_tokens);
    int ScanEllipsis(const unsigned char* lptr);
    bool LexSymbolOrKeyword(full_token_t* tok);
    int LexKeywordImpl(const char* match, size_t length);
    bool LexKeyword(full_token_t* tok, const char* token_start, size_t len);
    void LexStringLiteral(full_token_t* tok);
    bool PlungeQualifiedFile(const char* name);
    full_token_t* PushSynthesizedToken(TokenKind kind, int col);
    void SynthesizeIncludePathToken();
    void SetFileDefines(std::string file);

    full_token_t* advance_token_ptr();
    full_token_t* next_token();
    void lexpop();
    int preproc_expr(cell* val, int* tag);
    void substallpatterns(unsigned char* line, int buffersize);
    bool substpattern(unsigned char* line, size_t buffersize, const char* pattern,
                      const char* substitution, int& patternLen, int& substLen);
    void lex_symbol(full_token_t* tok, const char* token_start, size_t len);
    bool lex_match_char(char c);
    bool lex_number(full_token_t* tok);
    cell litchar(const unsigned char** lptr, int flags);
    const unsigned char* skipstring(const unsigned char* string);
    const unsigned char* skippgroup(const unsigned char* string);
    void packedstring(const unsigned char* lptr, int flags, full_token_t* tok);

    bool IsSkipping() const {
        return skiplevel_ > 0 && (ifstack_[skiplevel_ - 1] & SKIPMODE) == SKIPMODE;
    }

    bool FindMacro(const char* name, size_t length, macro_t* macro);
    bool DeleteMacro(const char* name, size_t length);

  private:
    CompileContext& cc_;
    ke::HashMap<sp::CharsAndLength, int, KeywordTablePolicy> keywords_;
    short icomment_; /* currently in multiline comment? */
    std::vector<short> comment_stack_;
    std::vector<size_t> preproc_if_stack_;
    std::vector<char> ifstack_;
    size_t iflevel_;             /* nesting level if #if/#else/#endif */
    size_t skiplevel_; /* level at which we started skipping (including nested #if .. #endif) */
    int listline_ = -1; /* "current line" for the list file */
    int lexnewline_;
    std::string deprecate_;
    bool allow_tags_ = true;
    int stmtindent_ = 0;
    bool indent_nowarn_ = false;
    bool freading_ = false;
    int fcurrent_ = 0;
    unsigned fline_ = 0;
    unsigned char pline_[sLINEMAX + 1];         /* the line read from the input file */
    int ctrlchar_ = CTRL_CHAR;

    std::shared_ptr<SourceFile> inpf_;

    token_buffer_t normal_buffer_;;
    token_buffer_t preproc_buffer_;
    token_buffer_t* token_buffer_;

    struct MacroTablePolicy {
        static bool matches(const std::string& a, const std::string& b) {
            return a == b;
        }
        static bool matches(const sp::CharsAndLength& a, const std::string& b) {
            if (a.length() != b.length())
                return false;
            return strncmp(a.str(), b.c_str(), a.length()) == 0;
        }
        static uint32_t hash(const std::string& key) {
            return ke::HashCharSequence(key.c_str(), key.length());
        }
        static uint32_t hash(const sp::CharsAndLength& key) {
            return ke::HashCharSequence(key.str(), key.length());
        }
    };

    struct MacroEntry {
        std::string first;
        std::string second;
        std::string documentation;
        bool deprecated;
    };
    ke::HashMap<std::string, MacroEntry, MacroTablePolicy> macros_;

    std::vector<short> current_file_stack_;
    std::vector<int> current_line_stack_;
    std::vector<std::shared_ptr<SourceFile>> input_file_stack_;
    std::vector<bool> need_semicolon_stack_;
    std::vector<int> require_newdecls_stack_;
};
