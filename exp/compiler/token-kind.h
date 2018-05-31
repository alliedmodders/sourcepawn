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
#ifndef _include_sourcepawn_token_kind_h_
#define _include_sourcepawn_token_kind_h_

namespace sp {

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
  _(DELETE,             "delete")                     \
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
  _(INVALID_FUNCTION,   "INVALID_FUNCTION")           \
  _(METHODMAP,          "methodmap")                  \
  _(NATIVE,             "native")                     \
  _(NULL,               "null")                       \
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
  _(TYPESET,            "typeset")                    \
  _(UNION,              "union")                      \
  _(USING,              "using")                      \
  _(VIEW_AS,            "view_as")                    \
  _(VOID,               "void")                       \
  _(WHILE,              "while")                      \
  _(NULLABLE,           "__nullable__")               \
  _(M_DEFINE,           "#define")                    \
  _(M_DEPRECATE,        "#deprecate")                 \
  _(M_ELSE,             "#else")                      \
  _(M_ENDIF,            "#endif")                     \
  _(M_ENDINPUT,         "#endinput")                  \
  _(M_FILE,             "#file")                      \
  _(M_IF,               "#if")                        \
  _(M_INCLUDE,          "#include")                   \
  _(M_LEAVING,          "#leaving")                   \
  _(M_LINE,             "__LINE__")                   \
  _(M_OPTIONAL_NEWDECLS, "#optional_newdecls")        \
  _(M_OPTIONAL_SEMI,    "#optional_semicolons")       \
  _(M_PRAGMA,           "#pragma")                    \
  _(M_REQUIRE_NEWDECLS, "#require_newdecls")          \
  _(M_REQUIRE_SEMI,     "#require_semicolons")        \
  _(M_TRYINCLUDE,       "#tryinclude")                \
  _(M_UNDEF,            "#undef")                     \
  _(INTRINSICS,         "__intrinsics__")             \
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
  _(UNKNOWN,            "<unknown>")                  \
  _(NEWLINE,            "<newline>")                  \
  _(IMPLICIT_INT,       "<implicit-integer>")         \
  /* Mists of dreams drip along the nascent echo, and love no more. */ \
  _(EOL,                "<end-of-line>")              \
  _(EOF,                "<end-of-file>")              \
  _(COMMENT,            "<comment>")

enum TokenKind
{
#   define _(name, str) TOK_##name,
    TOKENMAP(_)
#   undef _
    TOK_BITAND = TOK_AMPERSAND,
    TOKENS_TOTAL
};

} // namespace sp

#endif // _include_sourcepawn_token_kind_h_
