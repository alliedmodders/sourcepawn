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

#include "string-pool.h"
#include "pool-allocator.h"
#include "boxed-value.h"

namespace ke {

class JsonValue : public PoolObject
{
 public:
};

class JsonNull : public JsonValue
{
 public:
};

class JsonBool : public JsonValue
{
 public:
  JsonBool(bool value)
   : value_(value)
  {}

 private:
  bool value_;
};

class JsonInt : public JsonValue
{
 public:
  JsonInt(int value)
   : value_(value)
  {}

 private:
  int value_;
};

class JsonString : public JsonValue
{
 public:
  JsonString(Atom *atom);

 private:
  Atom *atom_;
};

class JsonObject : public JsonValue
{
 public:
  PoolList<Atom *> &keys() {
    return keys_;
  }
  PoolList<JsonValue *> &values() {
    return values_;
  }

  void add(Atom *key, JsonValue *value) {
    keys_.append(key);
    values_.append(value);
  }

 private:
  PoolList<Atom *> keys_;
  PoolList<JsonValue *> values_;
};

class JsonList : public JsonValue
{
 public:
  PoolList<JsonValue *> &items() {
    return items_;
  }
  void add(JsonValue *value) {
    items_.append(value);
  }
  size_t length() const {
    return items_.length();
  }

 private:
  PoolList<JsonValue *> items_;
};

} // namespace ke

#endif // _include_spcomp_json_tools_h_
