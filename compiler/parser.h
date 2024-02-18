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
#include "stl/stl-deque.h"

namespace sp {
namespace cc {

class Semantics;

class Parser
{
  public:
    Parser(CompileContext& cc, Semantics* sema);
    ~Parser();

    static bool PreprocExpr(cell* val, Type** type);

    ParseTree* Parse();

  private:
    typedef int (Parser::*HierFn)(value*);
    typedef Expr* (Parser::*NewHierFn)();


    void ChangeStaticScope(std::vector<Stmt*>* stmts);

    Stmt* parse_unknown_decl(const full_token_t* tok);
    Decl* parse_enum(int vclass);
    Stmt* parse_const(int vclass);
    Stmt* parse_stmt(bool allow_decl);
    Stmt* parse_static_assert();
    Decl* parse_pstruct();
    Decl* parse_typedef();
    Decl* parse_typeset();
    Decl* parse_enumstruct();
    Decl* parse_methodmap();
    MethodmapMethodDecl* parse_methodmap_method(MethodmapDecl* map);
    MethodmapPropertyDecl* parse_methodmap_property(MethodmapDecl* map);
    bool parse_methodmap_property_accessor(MethodmapDecl* map, Atom* name, const typeinfo_t& type,
                                           MemberFunctionDecl** out_getter,
                                           MemberFunctionDecl** out_setter);

    struct VarParams {
        int vclass;
        bool is_public = false;
        bool is_static = false;
        bool is_stock = false;
        bool autozero = true;
        bool is_arg = false;
        bool struct_init = false;
    };
    Stmt* parse_var(declinfo_t* decl, VarParams& params);
    void parse_post_dims(typeinfo_t* type);
    Expr* var_init(int vclass);
    Decl* parse_inline_function(int tokid, const declinfo_t& decl);

    bool parse_decl(declinfo_t* decl, int flags);
    bool parse_old_decl(declinfo_t* decl, int flags);
    bool parse_new_decl(declinfo_t* decl, const full_token_t* first, int flags);
    void parse_post_array_dims(declinfo_t* decl, int flags);
    bool parse_new_typeexpr(typeinfo_t* type, const full_token_t* first, int flags);
    bool parse_new_typename(const full_token_t* tok, TypenameInfo* out);
    bool reparse_old_decl(declinfo_t* decl, int flags);
    bool reparse_new_decl(declinfo_t* decl, int flags);
    void fix_mispredicted_postdims(declinfo_t* decl);
    void rewrite_type_for_enum_struct(typeinfo_t* info);
    int operatorname(Atom** name);

    Stmt* parse_compound();
    Stmt* parse_local_decl(int tokid, bool autozero);
    Stmt* parse_if();
    Stmt* parse_for();
    Stmt* parse_switch();
    Stmt* parse_case(std::vector<Expr*>* exprs);
    Stmt* parse_pragma_unused();
    TypedefInfo* parse_function_type();

    bool parse_function(FunctionDecl* info, int tokid, bool has_this);
    void parse_args(FunctionDecl* info, std::vector<ArgDecl*>* args);

    // Wrapper around hier14() that allows comma expressions without a wrapping
    // parens.
    Expr* parse_expr(bool parens);

    Expr* hier14();
    Expr* parse_view_as();
    Expr* plnge(const int* opstr, NewHierFn hier);
    Expr* plnge_rel(const int* opstr, NewHierFn hier);
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
    Expr* parse_new_array(const token_pos_t& pos, const TypenameInfo& rt);
    CallExpr* parse_call(const token_pos_t& pos, int tok, Expr* target);
    int nextop(int* opidx, const int* list);

    bool consume_line();

  private:
    CompileContext& cc_;
    Semantics* sema_;
    bool in_loop_ = false;
    bool in_test_ = false;
    bool allow_empty_array_index_ = false;
    std::shared_ptr<Lexer> lexer_;
    TypeManager* types_ = nullptr;
    tr::deque<FunctionDecl*> delayed_functions_;
    tr::unordered_map<size_t, SymbolScope*> static_scopes_;
    int sources_index_ = -1;
};

} // namespace cc
} // namespace sp
