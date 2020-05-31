// vim: set ts=2 sw=2 tw=99 et:
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
#include "shared/string-pool.h"
#include "compiler/reporting.h"
#include "compiler/source-manager.h"
#include "compiler/compile-context.h"
#include "compiler/parser/preprocessor.h"
#include "compiler/parser/parser.h"
#include "compiler/parser/json-tools.h"
#include "compiler/sema/name-resolver.h"
#include <assert.h>
#include <amtl/experimental/am-argparser.h>

using namespace ke;
using namespace sp;

// Comment analyzer. This is designed to only work when we're ignoring
// #include directives.
class Comments : public CommentDelegate
{
  struct CommentLoc {
    unsigned line;
    unsigned offset;

    CommentLoc()
     : line(0),
       offset(0)
    {}
    CommentLoc(unsigned line, unsigned offset)
     : line(line),
       offset(offset)
    {}
  };
  struct CommentRange {
    CommentLoc start;
    CommentLoc end;

    CommentRange()
    {}
    CommentRange(const CommentLoc &start, const CommentLoc &end)
     : start(start),
       end(end)
    {}
  };

 public:
  Comments(CompileContext &cc)
   : cc_(cc)
  {}

  void HandleComment(CommentPos aWhere, const SourceRange &aRange)
  {
    std::vector<CommentRange> &where = (aWhere == CommentPos::Front)
                                  ? lead_comments_
                                  : tail_comments_;

    FullSourceRef start = cc_.source().decode(aRange.start);
    FullSourceRef end = cc_.source().decode(aRange.end);
    assert(start.file && end.file && start.file == end.file);

    CommentRange range(
      CommentLoc(start.line, start.offset),
      CommentLoc(end.line, end.offset));

    if (!where.empty()) {
      // Comments must be in order.
      assert(range.start.offset >= where.back().end.offset);
    }
    where.push_back(range);
  }

  static int cmp_ends_at_line(const void *aItem1, const void *aItem2) {
    unsigned line = reinterpret_cast<uintptr_t>(aItem1);
    const CommentRange *item2 = (const CommentRange *)aItem2;
    if (line < item2->end.line)
      return -1;
    if (line > item2->end.line)
      return 1;
    return 0;
  }

  static int cmp_starts_at_line(const void *aItem1, const void *aItem2) {
    unsigned line = reinterpret_cast<uintptr_t>(aItem1);
    const CommentRange *item2 = (const CommentRange *)aItem2;
    if (line < item2->start.line)
      return -1;
    if (line > item2->start.line)
      return 1;
    return 0;
  }

  bool findCommentFor(const SourceLocation &loc, unsigned *start, unsigned *end) {
    // Find a comment that ends one line above loc.
    FullSourceRef ref = cc_.source().decode(loc);
    assert(ref.file);

    void *found =
      bsearch(reinterpret_cast<void *>(ref.line - 1),
              lead_comments_.data(),
              lead_comments_.size(),
              sizeof(CommentRange),
              cmp_ends_at_line);
    if (!found) {
      found = bsearch(reinterpret_cast<void *>(ref.line),
                      tail_comments_.data(),
                      tail_comments_.size(),
                      sizeof(CommentRange),
                      cmp_starts_at_line);
      if (!found)
        return false;
    }

    CommentRange *range = reinterpret_cast<CommentRange *>(found);
    *start = range->start.offset;
    *end = range->end.offset;
    return true;
  }

 private:
  CompileContext &cc_;
  std::vector<CommentRange> lead_comments_;
  std::vector<CommentRange> tail_comments_;
};

class Analyzer : public PartialAstVisitor
{
 public:
  Analyzer(CompileContext &cc, Comments &comments)
   : cc_(cc),
     pool_(cc.pool()),
     comments_(comments)
  {
    atom_name_ = cc_.add("name");
    atom_kind_ = cc_.add("kind");
    atom_returnType_ = cc_.add("returnType");
    atom_type_ = cc_.add("type");
    atom_parameters_ = cc_.add("arguments");
    atom_doc_start_ = cc_.add("docStart");
    atom_doc_end_ = cc_.add("docEnd");
    atom_properties_ = cc_.add("properties");
    atom_methods_ = cc_.add("methods");
    atom_getter_ = cc_.add("getter");
    atom_setter_ = cc_.add("setter");
    atom_entries_ = cc_.add("entries");
    atom_constants_ = cc_.add("constants");
    atom_decl_ = cc_.add("decl");
    atom_parent_ = cc_.add("parent");
  }

  JsonObject *analyze(ParseTree *tree) {
    functions_ = new (pool_) JsonList();
    methodmaps_ = new (pool_) JsonList();
    enums_ = new (pool_) JsonList();
    constants_ = new (pool_) JsonList();
    typesets_ = new (pool_) JsonList();
    typedefs_ = new (pool_) JsonList();

    for (size_t i = 0; i < tree->statements()->size(); i++) {
      Statement *stmt = tree->statements()->at(i);
      stmt->accept(this);
    }

    JsonObject *obj = new (pool_) JsonObject();
    obj->add(cc_.add("functions"), functions_);
    obj->add(cc_.add("methodmaps"), methodmaps_);
    obj->add(cc_.add("enums"), enums_);
    obj->add(cc_.add("constants"), constants_);
    obj->add(cc_.add("typesets"), typesets_);
    obj->add(cc_.add("typedefs"), typedefs_);
    return obj;
  }

  void visitMethodmapDecl(MethodmapDecl *node) override {
    JsonObject *obj = new (pool_) JsonObject();
    obj->add(atom_name_, toJson(node->name()));
    startDoc(obj, "class", node->name(), node->loc());

    if (node->parent())
      obj->add(atom_parent_, new (pool_) JsonString(node->parent()->name()));

    SaveAndSet<JsonList *> new_props(&props_, new (pool_) JsonList());
    SaveAndSet<JsonList *> new_methods(&methods_, new (pool_) JsonList());
    for (size_t i = 0; i < node->body()->size(); i++)
      node->body()->at(i)->accept(this);

    obj->add(atom_methods_, methods_);
    obj->add(atom_properties_, props_);

    methodmaps_->add(obj);
  }

  void visitMethodDecl(MethodDecl *node) override {
    JsonObject *obj = new (pool_) JsonObject();
    obj->add(atom_name_, toJson(node->name()));
    startDoc(obj, "method", node->name(), node->loc());

    FunctionNode *fun = node->method();
    if (fun->signature()->native())
        obj->add(atom_kind_, toJson("native"));
    else
        obj->add(atom_kind_, toJson("stock"));
    obj->add(atom_returnType_, toJson(fun->signature()->returnType()));
    obj->add(atom_parameters_, toJson(fun->signature()->parameters()));
    methods_->add(obj);
  }
  void visitPropertyDecl(PropertyDecl *node) override {
    JsonObject *obj = new (pool_) JsonObject();
    obj->add(atom_name_, toJson(node->name()));
    startDoc(obj, "property", node->name(), node->loc());

    obj->add(atom_type_, toJson(node->te()));
    obj->add(atom_getter_, new (pool_) JsonBool(!!node->getter()));
    obj->add(atom_setter_, new (pool_) JsonBool(!!node->setter()));
    props_->add(obj);
  }

  void visitTypesetDecl(TypesetDecl *decl) override {
    JsonObject *obj = new (pool_) JsonObject();
    obj->add(atom_name_, toJson(decl->name()));
    startDoc(obj, "typeset", decl->name(), decl->loc());

    JsonList *list = new (pool_) JsonList();
    for (size_t i = 0; i < decl->types()->size(); i++) {
      const TypesetDecl::Entry &entry = decl->types()->at(i);
      JsonObject *te = new (pool_) JsonObject();
      te->add(atom_type_, toJson(entry.te));
      unsigned start, end;
      if (comments_.findCommentFor(entry.loc, &start, &end)) {
        te->add(atom_doc_start_, new (pool_) JsonInt(start));
        te->add(atom_doc_end_, new (pool_) JsonInt(end));
      }
      list->add(te);
    }
    obj->add(cc_.add("types"), list);

    typesets_->add(obj);
  }

  void visitTypedefDecl(TypedefDecl *decl) override {
    JsonObject *obj = new (pool_) JsonObject();
    obj->add(atom_name_, toJson(decl->name()));
    startDoc(obj, "typedef", decl->name(), decl->loc());

    obj->add(atom_type_, toJson(decl->te()));

    typedefs_->add(obj);
  }

  void visitEnumStatement(EnumStatement *node) override {
    if (!node->name()) {
      for (size_t i = 0; i < node->entries()->size(); i++) {
        EnumConstant *cs = node->entries()->at(i);

        JsonObject *val = new (pool_) JsonObject();
        val->add(atom_name_, toJson(cs->name()));
        startDoc(val, "enum value", cs->name(), cs->loc());

        constants_->add(val);
      }
      return;
    }

    JsonObject *obj = new (pool_) JsonObject();
    obj->add(atom_name_, toJson(node->name()));
    startDoc(obj, "enum", node->name(), node->loc());

    JsonList *list = new (pool_) JsonList();
    for (size_t i = 0; i < node->entries()->size(); i++) {
      EnumConstant *cs = node->entries()->at(i);

      JsonObject *val = new (pool_) JsonObject();
      val->add(atom_name_, toJson(cs->name()));
      startDoc(val, "enum value", cs->name(), cs->loc());

      list->add(val);
    }
    obj->add(atom_entries_, list);

    enums_->add(obj);
  }

  void visitFunctionStatement(FunctionStatement *node) override {
    JsonObject *obj = new (pool_) JsonObject();
    obj->add(atom_name_, toJson(node->name()));
    startDoc(obj, "function", node->name(), node->loc());

    if (node->token() == TOK_FORWARD)
      obj->add(atom_kind_, toJson("forward"));
    else if (node->token() == TOK_NATIVE)
      obj->add(atom_kind_, toJson("native"));
    else
      obj->add(atom_kind_, toJson("stock"));

    obj->add(atom_returnType_, toJson(node->signature()->returnType()));
    obj->add(atom_parameters_, toJson(node->signature()->parameters()));

    functions_->add(obj);
  }

 private:
  void startDoc(JsonObject *obj, const char *type, Atom *name, const SourceLocation &loc) {
    unsigned start, end;
    if (!comments_.findCommentFor(loc, &start, &end)) {
      cc_.report(loc, rmsg::missing_comment)
        << type << name;
      return;
    }

    assert(start < INT_MAX);
    assert(end < INT_MAX);

    obj->add(atom_doc_start_, new (pool_) JsonInt(start));
    obj->add(atom_doc_end_, new (pool_) JsonInt(end));
  }

  JsonString *toJson(const TypeSpecifier *spec, Atom *name = nullptr) {
    return toJson(BuildTypeName(spec, name, TypeDiagFlags::Names).c_str());
  }
  JsonString *toJson(Type *type, Atom *name = nullptr) {
    return toJson(BuildTypeName(type, name, TypeDiagFlags::Names).c_str());
  }

  JsonString *toJson(const TypeExpr &te, Atom *name = nullptr) {
    if (te.spec())
      return toJson(te.spec(), name);
    return toJson(te.resolved(), name);
  }

  static inline bool isByRef(const TypeExpr& te) {
    return te.resolved()
           ? te.resolved()->isReference()
           : te.spec()->isByRef();
  }
  static inline bool isConst(const TypeExpr& te) {
    return te.resolved()
           ? te.resolved()->isConst()
           : te.spec()->isConst();
  }

  JsonString *toJson(VarDecl *decl, bool named) {
    // :TODO: add a BuildTypeName(VarDecl) helper.
    TypeDiagFlags flags = TypeDiagFlags::Names;
    if (isByRef(decl->te()))
      flags |= TypeDiagFlags::IsByRef;
    if (isConst(decl->te()))
      flags |= TypeDiagFlags::IsConst;
    return toJson(BuildTypeName(
      decl->te(),
      named ? decl->name() : nullptr,
      flags).c_str());
  }

  JsonList *toJson(const ParameterList *params) {
    JsonList *list = new (pool_) JsonList();
    for (size_t i = 0; i < params->size(); i++) {
      VarDecl *decl = params->at(i);
      JsonObject *obj = new (pool_) JsonObject();

      obj->add(atom_type_, toJson(decl, false));

      if (decl->name()) {
        obj->add(atom_name_, toJson(decl->name()));
        obj->add(atom_decl_, toJson(decl, true));
      } else {
        obj->add(atom_name_, toJson("..."));

        AutoString builder = BuildTypeName(decl->te(), nullptr, TypeDiagFlags::Names);
        builder = builder + " ...";
        obj->add(atom_decl_, toJson(builder.ptr()));
      }
      list->add(obj);
    }
    return list;
  }

  JsonString *toJson(Atom *name) {
    return new (pool_) JsonString(name);
  }
  JsonString *toJson(const char *str) {
    return new (pool_) JsonString(cc_.add(str));
  }

 private:
  CompileContext &cc_;
  PoolAllocator &pool_;
  Comments &comments_;

  Atom *atom_name_;
  Atom *atom_kind_;
  Atom *atom_returnType_;
  Atom *atom_type_;
  Atom *atom_parameters_;
  Atom *atom_doc_start_;
  Atom *atom_doc_end_;
  Atom *atom_properties_;
  Atom *atom_methods_;
  Atom *atom_getter_;
  Atom *atom_setter_;
  Atom *atom_entries_;
  Atom *atom_constants_;
  Atom *atom_decl_;
  Atom *atom_parent_;
  JsonList *functions_;
  JsonList *methodmaps_;
  JsonList *enums_;
  JsonList *constants_;
  JsonList *typesets_;
  JsonList *typedefs_;

  JsonList *props_;
  JsonList *methods_;
};

static JsonObject *
Run(CompileContext &cc, const char *path)
{
  Comments comments(cc);
  ParseTree *tree = nullptr;
  {
    Preprocessor pp(cc);

    pp.disableIncludes();
    pp.setCommentDelegate(&comments);

    {
      ReportingContext rc(cc, SourceLocation());
      RefPtr<SourceFile> file = cc.source().open(rc, path);
      if (!file)
        return nullptr;
      if (!pp.enter(file))
        return nullptr;
    }

    NameResolver nr(cc);
    Parser parser(cc, pp, nr);

    tree = parser.parse();
    if (!tree || !cc.phasePassed())
      return nullptr;
  }

  Analyzer analyzer(cc, comments);
  return analyzer.analyze(tree);
}

int main(int argc, char **argv)
{
  args::Parser parser("Documentation generator.");

  args::StringOption filename(parser,
    "filename",
    "SourcePawn file to scan for documentation.");

  StringPool strings;
  ReportManager reports;
  SourceManager source(strings, reports);

  if (!parser.parse(argc, argv)) {
    parser.usage(stderr, argc, argv);
    return 1;
  }

  PoolAllocator pool;
  {
    PoolScope scope(pool);

    CompileContext cc(pool, strings, reports, source);

    cc.SkipResolution();

    const char* file = filename.value().c_str();
    JsonObject *obj = Run(cc, file);
    if (!obj) {
      reports.PrintMessages();
      return 1;
    }

    JsonRenderer renderer(std::cout);
    renderer.Render(obj);

    if (reports.HasMessages())
      reports.PrintMessages();
  }

  return 0;
}
