// vim: set ts=8 sts=4 sw=4 tw=99 et:
//  Pawn compiler - Recursive descend expresion parser
//
//  Copyright (c) ITB CompuPhase, 1997-2005
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
//
//  Version: $Id$

#include "expressions.h"
#include "parse-node.h"
#include "sc.h"
#include "sctracker.h"

class Parser : public ExpressionParser
{
  public:
    Parser();
    ~Parser();

    int expression(value* lval);

    void parse();

    // Temporary until parser.cpp no longer is shimmed.
    Decl* parse_enum(int vclass);
    Stmt* parse_const(int vclass);
    Stmt* parse_stmt(int* lastindent, bool allow_decl);

    struct VarParams {
        int vclass;
        bool is_public = false;
        bool is_static = false;
        bool is_stock = false;
        bool autozero = true;
        bool is_arg = false;
    };
    Stmt* parse_var(declinfo_t* decl, const VarParams& params);
    void parse_post_dims(typeinfo_t* type);
    Expr* var_init(int vclass);

    static symbol* ParseInlineFunction(int tokid, const declinfo_t& decl, const int* this_tag);

    static bool sInPreprocessor;
    static bool sDetectedIllegalPreprocessorSymbols;
    static int sActive;

  private:
    typedef int (Parser::*HierFn)(value*);
    typedef Expr* (Parser::*NewHierFn)();

    Stmt* parse_unknown_decl(const token_t* tok);
    Stmt* parse_static_assert();
    Decl* parse_pstruct();
    Decl* parse_typedef();
    Decl* parse_typeset();
    Decl* parse_using();
    Decl* parse_enumstruct();
    Decl* parse_methodmap();
    bool parse_methodmap_method(MethodmapDecl* map);
    bool parse_methodmap_property(MethodmapDecl* map);
    bool parse_methodmap_property_accessor(MethodmapDecl* map, MethodmapProperty* prop);

    Stmt* parse_compound(bool sameline);
    Stmt* parse_local_decl(int tokid, bool autozero);
    Stmt* parse_if();
    Stmt* parse_for();
    Stmt* parse_switch();
    void parse_case(SwitchStmt* sw);
    Stmt* parse_pragma_unused();

    bool parse_function(FunctionInfo* info, int tokid);
    void parse_args(FunctionInfo* info);

    // Wrapper around hier14() that allows comma expressions without a wrapping
    // parens.
    Expr* parse_expr(bool parens);

    Expr* hier14();
    Expr* parse_view_as();
    Expr* plnge(int* opstr, NewHierFn hier);
    Expr* plnge_rel(int* opstr, NewHierFn hier);
    Expr* hier13();
    Expr* hier12();
    Expr* hier11();
    Expr* hier10();
    Expr* hier9();
    Expr* hier8();
    Expr* hier7();
    Expr* hier6();
    Expr* hier5();
    Expr* hier4();
    Expr* hier3();
    Expr* hier2();
    Expr* hier1();
    Expr* primary();
    Expr* constant();
    Expr* struct_init();
    Expr* parse_new_array(const token_pos_t& pos, int tag);
    CallExpr* parse_call(const token_pos_t& pos, int tok, Expr* target);

    bool consume_line();

  private:
    bool in_loop_ = false;
};

// Implemented in parser.cpp.
int parse_new_decl(declinfo_t* decl, const token_t* first, int flags);
functag_t* parse_function_type();
int parse_new_typeexpr(typeinfo_t* type, const token_t* first, int flags);
int parse_new_typename(const token_t* tok);
int reparse_old_decl(declinfo_t* decl, int flags);
int reparse_new_decl(declinfo_t* decl, int flags);
