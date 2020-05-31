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
#ifndef _include_spcomp_json_tools_h_
#define _include_spcomp_json_tools_h_

#include <iostream>
#include <sstream>
#include "shared/string-pool.h"
#include "pool-allocator.h"
#include "boxed-value.h"
#include <stdio.h>

namespace sp {

using namespace ke;

class JsonRenderer;

class JsonValue : public PoolObject
{
 public:
  virtual void Render(JsonRenderer* renderer) = 0;
};

class JsonNull;
class JsonBool;
class JsonInt;
class JsonString;
class JsonList;
class JsonObject;

class JsonRenderer
{
 public:
  JsonRenderer(std::ostream& output);

  void Render(JsonValue* value);

  std::ostream& stream() {
    return out_;
  }


  void RenderNull(JsonNull* val);
  void RenderBool(JsonBool* val);
  void RenderInt(JsonInt* val);
  void RenderString(JsonString* val);
  void RenderList(JsonList* val);
  void RenderObject(JsonObject* val);

 private:
  void indent();
  void dedent();
  void prefix();

 private:
  std::ostream& out_;
  size_t indent_;
};

class JsonNull : public JsonValue
{
 public:
  void Render(JsonRenderer* renderer) override {
    renderer->RenderNull(this);
  }
};

class JsonBool : public JsonValue
{
 public:
  JsonBool(bool value)
   : value_(value)
  {}

  void Render(JsonRenderer* renderer) override {
    renderer->RenderBool(this);
  }

  bool value() const {
    return value_;
  }

 private:
  bool value_;
};

class JsonInt : public JsonValue
{
 public:
  JsonInt(int value)
   : value_(value)
  {}

  void Render(JsonRenderer* renderer) override {
    renderer->RenderInt(this);
  }

  int value() const {
    return value_;
  }

 private:
  int value_;
};

class JsonString : public JsonValue
{
 public:
  JsonString(Atom* atom)
   : atom_(atom)
   {}

  void Render(JsonRenderer* renderer) override {
    renderer->RenderString(this);
  }

  Atom* atom() const {
    return atom_;
  }

 private:
  Atom* atom_;
};

class JsonObject : public JsonValue
{
 public:
  PoolList<Atom*>& keys() {
    return keys_;
  }
  PoolList<JsonValue*>& values() {
    return values_;
  }

  void add(Atom* key, JsonValue* value) {
    keys_.push_back(key);
    values_.push_back(value);
  }

  void Render(JsonRenderer* renderer) override {
    renderer->RenderObject(this);
  }

 private:
  PoolList<Atom*> keys_;
  PoolList<JsonValue*> values_;
};

class JsonList : public JsonValue
{
 public:
  PoolList<JsonValue*>& items() {
    return items_;
  }
  void add(JsonValue* value) {
    items_.push_back(value);
  }
  size_t length() const {
    return items_.size();
  }

  void Render(JsonRenderer* renderer) override {
    renderer->RenderList(this);
  }

 private:
  PoolList<JsonValue*> items_;
};

} // namespace ke

#endif // _include_spcomp_json_tools_h_
