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
#ifndef _include_sourcepawn_token_h_
#define _include_sourcepawn_token_h_

#include <assert.h>
#include <stdint.h>
#include <limits.h>

namespace ke {

// List of tokens the scanner can recognize.
//
// Some restrictions:
//   * (ASSIGN_ADD - TOK_PLUS) must be the delta between an operator and its
//     op-and-assign equivalent.
#define TOKENMAP(_)                                   \
  _(NONE,               "<none>")                     \
  _(ERROR,              "<error>")                    \
  _(NAME,               "<name>")                     \
  _(INTEGER_LITERAL,    "<integer>")                  \
  _(HEX_LITERAL,        "<hex>")                      \
  _(FLOAT_LITERAL,      "<float>")                    \
  _(STRING_LITERAL,     "<string>")                   \
  _(CHAR_LITERAL,       "<char>")                     \
  _(LABEL,              "<label>")                    \
  _(BOOL,               "bool")                       \
  _(BREAK,              "break")                      \
  _(CASE,               "case")                       \
  _(CHAR,               "char")                       \
  _(CLASS,              "class")                      \
  _(CONST,              "const")                      \
  _(CONTINUE,           "continue")                   \
  _(DECL,               "decl")                       \
  _(DEFAULT,            "default")                    \
  _(DEFINED,            "defined")                    \
  _(DO,                 "do")                         \
  _(ELSE,               "else")                       \
  _(ENUM,               "enum")                       \
  _(FALSE,              "false")                      \
  _(FLOAT,              "float")                      \
  _(FOR,                "for")                        \
  _(FORWARD,            "forward")                    \
  _(FUNCTAG,            "functag")                    \
  _(FUNCTION,           "function")                   \
  _(IF,                 "if")                         \
  _(INT,                "int")                        \
  _(METHODMAP,          "methodmap")                  \
  _(NATIVE,             "native")                     \
  _(NEW,                "new")                        \
  _(OBJECT,             "object")                     \
  _(PROPERTY,           "property")                   \
  _(PUBLIC,             "public")                     \
  _(RETURN,             "return")                     \
  _(SIZEOF,             "sizeof")                     \
  _(STATIC,             "static")                     \
  _(STOCK,              "stock")                      \
  _(STRUCT,             "struct")                     \
  _(SWITCH,             "switch")                     \
  _(THIS,               "this")                       \
  _(TRUE,               "true")                       \
  _(TYPEDEF,            "typedef")                    \
  _(UNION,              "union")                      \
  _(VOID,               "void")                       \
  _(WHILE,              "while")                      \
  _(NULLABLE,           "__nullable__")               \
  _(M_DEFINE,           "#define")                    \
  _(M_DEPRECATE,        "#deprecate")                 \
  _(M_ENDIF,            "#endif")                     \
  _(M_ENDINPUT,         "#endinput")                  \
  _(M_FILE,             "#file")                      \
  _(M_IF,               "#if")                        \
  _(M_INCLUDE,          "#include")                   \
  _(M_LEAVING,          "#leaving")                   \
  _(M_OPTIONAL_NEWDECLS, "#optional_newdecls")        \
  _(M_OPTIONAL_SEMI,    "#optional_semicolons")       \
  _(M_PRAGMA,           "#pragma")                    \
  _(M_REQUIRE_NEWDECLS, "#require_newdecls")          \
  _(M_REQUIRE_SEMI,     "#require_semicolons")        \
  _(M_TRYINCLUDE,       "#tryinclude")                \
  _(M_UNDEF,            "#undef")                     \
  _(ELLIPSES,           "...")                        \
  _(PLUS,               "+")                          \
  _(MINUS,              "-")                          \
  _(NEGATE,             "-")                          \
  _(STAR,               "*")                          \
  _(SLASH,              "/")                          \
  _(PERCENT,            "%")                          \
  _(AMPERSAND,          "&")                          \
  _(BITOR,              "|")                          \
  _(BITXOR,             "^")                          \
  _(SHR,                ">>")                         \
  _(USHR,               ">>>")                        \
  _(SHL,                "<<")                         \
  _(ASSIGN,             "=")                          \
  _(SEMICOLON,          ";")                          \
  _(LBRACE,             "{")                          \
  _(RBRACE,             "}")                          \
  _(LPAREN,             "(")                          \
  _(RPAREN,             ")")                          \
  _(LBRACKET,           "[")                          \
  _(RBRACKET,           "]")                          \
  _(ASSIGN_ADD,         "+=")                         \
  _(ASSIGN_SUB,         "-=")                         \
  _(ASSIGN_MUL,         "*=")                         \
  _(ASSIGN_DIV,         "/=")                         \
  _(ASSIGN_MOD,         "%=")                         \
  _(ASSIGN_BITAND,      "&=")                         \
  _(ASSIGN_BITOR,       "|=")                         \
  _(ASSIGN_BITXOR,      "^=")                         \
  _(ASSIGN_SHR,         ">>=")                        \
  _(ASSIGN_USHR,        ">>>=")                       \
  _(ASSIGN_SHL,         "<<=")                        \
  _(INCREMENT,          "++")                         \
  _(DECREMENT,          "--")                         \
  _(EQUALS,             "==")                         \
  _(NOTEQUALS,          "!=")                         \
  _(LT,                 "<")                          \
  _(LE,                 "<=")                         \
  _(GT,                 ">")                          \
  _(GE,                 ">=")                         \
  _(AND,                "&&")                         \
  _(OR,                 "||")                         \
  _(COMMA,              ",")                          \
  _(NOT,                "!")                          \
  _(TILDE,              "~")                          \
  _(QMARK,              "?")                          \
  _(COLON,              ":")                          \
  _(DOT,                ".")                          \
  _(NEWLINE,            "<newline>")                  \
  _(IMPLICIT_INT,       "<implicit-integer>")         \
  /* Mists of dreams drip along the nascent echo, and love no more. */ \
  _(EOL,                "<end-of-line>")              \
  _(EOF,                "<end-of-file>")

enum TokenKind
{
#   define _(name, str) TOK_##name,
    TOKENMAP(_)
#   undef _
    TOK_BITAND = TOK_AMPERSAND,
    TOKENS_TOTAL
};

#define KEYWORDMAP(_)                                 \
  _(BREAK)                                            \
  _(CASE)                                             \
  _(CHAR)                                             \
  _(CONST)                                            \
  _(CONTINUE)                                         \
  _(DECL)                                             \
  _(DEFAULT)                                          \
  _(DEFINED)                                          \
  _(DO)                                               \
  _(ELSE)                                             \
  _(ENUM)                                             \
  _(FALSE)                                            \
  _(FLOAT)                                            \
  _(FOR)                                              \
  _(FORWARD)                                          \
  _(FUNCTAG)                                          \
  _(FUNCTION)                                         \
  _(IF)                                               \
  _(INT)                                              \
  _(METHODMAP)                                        \
  _(NATIVE)                                           \
  _(NEW)                                              \
  _(OBJECT)                                           \
  _(PROPERTY)                                         \
  _(PUBLIC)                                           \
  _(RETURN)                                           \
  _(SIZEOF)                                           \
  _(STATIC)                                           \
  _(STOCK)                                            \
  _(STRUCT)                                           \
  _(SWITCH)                                           \
  _(THIS)                                             \
  _(TRUE)                                             \
  _(TYPEDEF)                                          \
  _(UNION)                                            \
  _(WHILE)                                            \
  _(VOID)                                             \
  _(M_DEFINE)                                         \
  _(M_DEPRECATE)                                      \
  _(M_ENDIF)                                          \
  _(M_ENDINPUT)                                       \
  _(M_FILE)                                           \
  _(M_IF)                                             \
  _(M_INCLUDE)                                        \
  _(M_LEAVING)                                        \
  _(M_OPTIONAL_NEWDECLS)                              \
  _(M_OPTIONAL_SEMI)                                  \
  _(M_PRAGMA)                                         \
  _(M_REQUIRE_NEWDECLS)                               \
  _(M_REQUIRE_SEMI)                                   \
  _(M_TRYINCLUDE)                                     \
  _(M_UNDEF)                                          \
  _(NULLABLE)

struct SourcePosition
{
    unsigned line;
    unsigned col;

    SourcePosition()
      : line(0),
        col(0)
    {
    }

    SourcePosition(unsigned line, unsigned col)
      : line(line),
        col(col)
    {
    }
};

class FileContext;

struct SourceLocation : public SourcePosition
{
  FileContext *file;

  SourceLocation(FileContext *file, const SourcePosition &pos)
   : SourcePosition(pos),
     file(file)
  {
  }
  SourceLocation()
   : file(nullptr)
  {
  }
};

struct SourceExtends
{
  FileContext *file;
  SourcePosition start;
  SourcePosition end;

  SourceExtends(FileContext *file, const SourcePosition &start, const SourcePosition &end)
   : file(file),
     start(start),
     end(end)
  {
  }
};

class Atom;

struct Token
{
  TokenKind kind;
  SourceLocation start;
  SourceLocation end;

  void setAtom(Atom *atom) {
    atom_ = atom;
  }
  Atom *atom() const {
    assert(atom_);
    return atom_;
  }
  void setCharValue(int32_t value) {
    assert(kind == TOK_CHAR_LITERAL);
    int_value_ = value;
  }
  int32_t charValue() const {
    assert(kind == TOK_CHAR_LITERAL);
    return int32_t(int_value_);
  }
  void setIntValue(int64_t value) {
    int_value_ = value;
  }
  int64_t intValue() const {
    assert(kind == TOK_INTEGER_LITERAL || kind == TOK_HEX_LITERAL);
    return int_value_;
  }
  int32_t int32Value() const {
    assert(intValue() <= int64_t(INT_MAX));
    return (int32_t)int_value_;
  }
  void setDoubleValue(double value) {
    double_value_ = value;
  }
  double doubleValue() const {
    assert(kind == TOK_FLOAT_LITERAL);
    return double_value_;
  }

 private:
  Atom *atom_;
  int64_t int_value_;
  double double_value_;
};

// Lighter-weight token object for names only.
struct NameToken
{
  SourceLocation start;
  Atom *atom;

  NameToken()
   : atom(nullptr)
  { }
  NameToken(const Token *tok)
   : start(tok->start),
     atom(tok->atom())
  { }
  NameToken(const Token &tok)
   : start(tok.start),
     atom(tok.atom())
  {
  }
};

static inline bool IsNewTypeToken(TokenKind kind)
{
  switch (kind) {
    case TOK_BOOL:
    case TOK_CHAR:
    case TOK_FLOAT:
    case TOK_INT:
    case TOK_OBJECT:
    case TOK_VOID:
      return true;
    default:
      return false;
  }
}

//  Name                Token   Order Prec Arity
#define OPERATOR_MAP(_)                                 \
  _(Not,                NOT,      0,  2,  1)            \
  _(Complement,         TILDE,    0,  2,  1)            \
  _(UnaryMinus,         NEGATE,   0,  2,  1)            \
  _(Mul,                STAR,     1,  3,  2)            \
  _(Div,                SLASH,    1,  3,  2)            \
  _(Modulo,             PERCENT,  1,  3,  2)            \
  _(Add,                PLUS,     1,  4,  2)            \
  _(Sub,                MINUS,    1,  4,  2)            \
  _(Shr,                SHR,      1,  5,  2)            \
  _(Ushr,               USHR,     1,  5,  2)            \
  _(Shl,                SHL,      1,  5,  2)            \
  _(BitAnd,             AMPERSAND,1,  6,  2)            \
  _(BitXor,             BITXOR,   1,  7,  2)            \
  _(BitOr,              BITOR,    1,  8,  2)            \
  _(Lt,                 LT,       1,  9,  2)            \
  _(Le,                 LE,       1,  9,  2)            \
  _(Gt,                 GT,       1,  9,  2)            \
  _(Ge,                 GE,       1,  9,  2)            \
  _(Eq,                 EQUALS,   1,  10, 2)            \
  _(Ne,                 NOTEQUALS,1,  10, 2)            \
  _(And,                AND,      1,  11, 2)            \
  _(Or,                 OR,       1,  12, 2)            \
  _(Ternary,            QMARK,    0,  13, 3)

enum Operator {
#define _(name, token, order, prec, arity) Operator_##name,
  OPERATOR_MAP(_)
#undef _
  Operators_Total
};

extern const char *TokenNames[];

} // namespace ke

#endif //_include_sourcepawn_token_h_

