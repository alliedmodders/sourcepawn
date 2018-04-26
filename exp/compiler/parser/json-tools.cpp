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

JsonRenderer::JsonRenderer(FILE* fp)
 : fp_(fp),
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
  fprintf(fp_, "null");
}

void JsonRenderer::RenderBool(JsonBool* val)
{
  if (val->value())
    fprintf(fp_, "true");
  else
    fprintf(fp_, "false");
}

void JsonRenderer::RenderInt(JsonInt* val)
{
  fprintf(fp_, "%d", val->value());
}

void JsonRenderer::RenderString(JsonString* val)
{
  fprintf(fp_, "\"");
  for (size_t i = 0; i < val->atom()->length(); i++) {
    char c = val->atom()->chars()[i];
    switch (c) {
      case '\0':
        fprintf(fp_, "\\0");
        continue;
      case '\\':
        fprintf(fp_, "\\");
        continue;
      case '"':
      case '\r':
      case '\n':
        fprintf(fp_, "\\");
        break;
    }
    fprintf(fp_, "%c", c);
  }
  fprintf(fp_, "\"");
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
    fprintf(fp_, " ");
}

void JsonRenderer::RenderList(JsonList* val)
{
  if (!val->items().length()) {
    fprintf(fp_, "[]");
    return;
  }

  fprintf(fp_, "[\n");
  indent();
  for (size_t i = 0; i < val->items().length(); i++) {
    prefix();
    val->items()[i]->Render(this);
    if (i != val->items().length() - 1)
      fprintf(fp_, ",");
    fprintf(fp_, "\n");
  }
  dedent();
  prefix();
  fprintf(fp_, "]");
}

void JsonRenderer::RenderObject(JsonObject* val)
{
  if (!val->keys().length()) {
    fprintf(fp_, "{}");
    return;
  }

  fprintf(fp_, "{\n");
  indent();
  for (size_t i = 0; i < val->keys().length(); i++) {
    prefix();
    fprintf(fp_, "\"%s\": ", val->keys()[i]->chars());
    val->values()[i]->Render(this);
    if (i != val->keys().length() - 1)
      fprintf(fp_, ",");
    fprintf(fp_, "\n");
  }
  dedent();
  prefix();
  fprintf(fp_, "}");
}
