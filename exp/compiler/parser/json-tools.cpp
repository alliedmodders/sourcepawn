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
#include "json-tools.h"
#include <stdio.h>

using namespace ke;
using namespace sp;

JsonRenderer::JsonRenderer(std::ostream& output)
 : out_(output),
   indent_(0)
{
}

void
JsonRenderer::Render(JsonValue* value)
{
  value->Render(this);
}

void JsonRenderer::RenderNull(JsonNull* val)
{
  out_ << "null";
}

void JsonRenderer::RenderBool(JsonBool* val)
{
  if (val->value())
    out_ << "true";
  else
    out_ << "false";
}

void JsonRenderer::RenderInt(JsonInt* val)
{
  out_ << val->value();
}

void JsonRenderer::RenderString(JsonString* val)
{
  out_ << "\"";
  for (size_t i = 0; i < val->atom()->length(); i++) {
    char c = val->atom()->chars()[i];
    switch (c) {
      case '\0':
        out_ << "\\0";
        continue;
      case '\\':
        out_ << "\\";
        continue;
      case '"':
      case '\r':
      case '\n':
        out_ << "\\";
        break;
    }
    out_ << c;
  }
  out_ << "\"";
}

void
JsonRenderer::indent()
{
  indent_ += 2;
}

void
JsonRenderer::dedent()
{
  assert(indent_ >= 2);
  indent_ -= 2;
}

void
JsonRenderer::prefix()
{
  for (size_t i = 0; i < indent_; i++)
    out_ << " ";
}

void JsonRenderer::RenderList(JsonList* val)
{
  if (!val->items().size()) {
    out_ << "[]";
    return;
  }

  out_ << "[\n";
  indent();
  for (size_t i = 0; i < val->items().size(); i++) {
    prefix();
    val->items()[i]->Render(this);
    if (i != val->items().size() - 1)
      out_ << ",";
    out_ << "\n";
  }
  dedent();
  prefix();
  out_ << "]";
}

void JsonRenderer::RenderObject(JsonObject* val)
{
  if (!val->keys().size()) {
    out_ << "{}";
    return;
  }

  out_ << "{\n";
  indent();
  for (size_t i = 0; i < val->keys().size(); i++) {
    prefix();
    out_ << "\"";
    out_ << val->keys()[i]->chars();
    out_ << "\": ";
    val->values()[i]->Render(this);
    if (i != val->keys().size() - 1)
      out_ << ",";
    out_ << "\n";
  }
  dedent();
  prefix();
  out_ << "}";
}
