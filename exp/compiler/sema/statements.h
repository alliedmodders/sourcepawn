// vim: set ts=2 sw=2 tw=99 et:
// 
// Copyright (C) 2012-2014 AlliedModders LLC and David Anderson
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
#ifndef _include_sourcepawn_sema_statements_h_
#define _include_sourcepawn_sema_statements_h_

namespace sp {
namespace sema {

class Expr;

class Statement : public PoolObject
{
public:
  explicit Statement(AstNode* node)
    : node_(node)
  {}

private:
  AstNode* node_;
};

typedef PoolList<Statement *> Statements;

class Block : public Statement
{
public:
  explicit Block(AstNode* node, Statements* statements)
    : Statement(node),
      statements_(statements)
  {}

private:
  Statements* statements_;
};

class FunctionDef : public Statement
{
public:
  explicit FunctionDef(AstNode* node, Block* body)
    : Statement(node),
      body_(body)
  {}

private:
  Block* body_;
};

class Return : public Statement
{
public:
  explicit Return(AstNode* node, Expr* expr)
    : Statement(node),
      expr_(expr)
  {}

private:
  Expr* expr_;
};

} // namespace sema
} // namespace sp

#endif // _include_sourcepawn_sema_statements_h_
