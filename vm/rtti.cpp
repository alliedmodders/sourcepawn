// vim: set sts=2 ts=8 sw=2 tw=99 et:
//
// Copyright (C) 2004-2015 AlliedModers LLC
//
// This file is part of SourcePawn. SourcePawn is licensed under the GNU
// General Public License, version 3.0 (GPL). If a copy of the GPL was not
// provided with this file, you can obtain it here:
//   http://www.gnu.org/licenses/gpl.html
//
#include "rtti.h"
#include <smx/smx-typeinfo.h>

using namespace ke;
using namespace sp;

RttiData::RttiData()
 : image_(nullptr),
   rtti_data_(nullptr),
   rtti_data_size_(0)
{
  rtti_cache_.init();
}

RttiData::RttiData(SmxV1Image* image, const uint8_t* blob, size_t size)
 : image_(image),
   rtti_data_(blob),
   rtti_data_size_(size)
{
  rtti_cache_.init();
}

Rtti*
RttiData::typeFromTypeId(uint32_t type_id)
{
  if (!rtti_data_)
    return nullptr;

  // See if that type was already decoded before.
  TypeCacheMap::Insert r = rtti_cache_.findForAdd(type_id);
  if (r.found()) {
    return r->value;
  }

  uint8_t kind = type_id & kMaxTypeIdKind;
  uint32_t payload = (type_id >> 4) & kMaxTypeIdPayload;

  if (kind == kTypeId_Inline) {
    uint8_t bytes[4];
    bytes[0] = (payload >> 0) & 0xff;
    bytes[1] = (payload >> 8) & 0xff;
    bytes[2] = (payload >> 16) & 0xff;
    bytes[3] = (payload >> 24) & 0xff;

    AutoPtr<RttiParser> parser(new RttiParser(bytes, 4, 0));
    Rtti* rtti_type = parser->decodeNew();
    rtti_cache_.add(r, type_id, rtti_type);
    return rtti_type;
  } else if (kind == kTypeId_Complex) {
    AutoPtr<RttiParser> parser(new RttiParser(rtti_data_, rtti_data_size_, payload));
    Rtti* rtti_type = parser->decodeNew();
    rtti_cache_.add(r, type_id, rtti_type);
    return rtti_type;
  }
  return nullptr;
}

Rtti*
RttiData::functionTypeFromOffset(uint32_t offset)
{
  if (!rtti_data_)
    return nullptr;

  // See if that type was already decoded before.
  TypeCacheMap::Insert r = rtti_cache_.findForAdd(offset);
  if (r.found()) {
    return r->value;
  }

  AutoPtr<RttiParser> parser(new RttiParser(rtti_data_, rtti_data_size_, offset));
  Rtti* rtti_type = parser->decodeFunction();
  rtti_cache_.add(r, offset, rtti_type);
  return rtti_type;
}

Rtti*
RttiData::typesetTypeFromOffset(uint32_t offset)
{
  if (!rtti_data_)
    return nullptr;

  // See if that type was already decoded before.
  TypeCacheMap::Insert r = rtti_cache_.findForAdd(offset);
  if (r.found()) {
    return r->value;
  }

  AutoPtr<RttiParser> parser(new RttiParser(rtti_data_, rtti_data_size_, offset));
  Rtti* rtti_type = parser->decodeTypeset();
  rtti_cache_.add(r, offset, rtti_type);
  return rtti_type;
}

const uint8_t*
RttiData::blob()
{
  return rtti_data_;
}

size_t
RttiData::size()
{
  return rtti_data_size_;
}

RttiParser::RttiParser(const uint8_t* bytes, uint32_t length, uint32_t offset)
 : bytes_(bytes),
   length_(length),
   offset_(offset),
   is_const_(false)
{
}

// Decode a type, but reset the |is_const| indicator for non-
// dependent type.
Rtti*
RttiParser::decodeNew()
{
  bool was_const = is_const_;
  is_const_ = false;
  
  Rtti* result = decode();
  if (is_const_)
    result->setConst();

  is_const_ = was_const;
  return result;
}

Rtti*
RttiParser::decode()
{
  is_const_ = match(cb::kConst) || is_const_;

  // TODO: error on offset_ >= length_
  uint8_t type = bytes_[offset_++];
  switch (type) {
  case cb::kBool:
  case cb::kInt32:
  case cb::kFloat32:
  case cb::kChar8:
  case cb::kAny:
  case cb::kTopFunction:
    return new Rtti(type);

  case cb::kFixedArray:
  {
    uint32_t size = decodeUint32();
    Rtti* inner = decode();
    return new Rtti(type, size, inner);
  }
  case cb::kArray:
  {
    Rtti* inner = decode();
    return new Rtti(type, inner);
  }
  case cb::kEnum:
  case cb::kTypedef:
  case cb::kTypeset:
  case cb::kClassdef:
  case cb::kEnumStruct:
  {
    uint32_t index = decodeUint32();
    return new Rtti(type, index);
  }
  case cb::kFunction:
    return decodeFunction();
  }
  return nullptr;
}

Rtti*
RttiParser::decodeFunction()
{
  // TODO: error on offset_ >= length_
  uint8_t argc = bytes_[offset_++];

  bool variadic = false;
  if (bytes_[offset_] == cb::kVariadic) {
    variadic = true;
    offset_++;
  }

  Rtti* returnType;
  if (bytes_[offset_] == cb::kVoid) {
    returnType = new Rtti(cb::kVoid);
    offset_++;
  } else {
    returnType = decodeNew();
  }

  Rtti* functionType = new Rtti(returnType, variadic);
  for (uint8_t i = 0; i < argc; i++) {
    bool is_by_ref = match(cb::kByRef);
    Rtti* arg_type = decodeNew();
    if (is_by_ref)
      arg_type->setByRef();
    functionType->addArgument(arg_type);
  }
  return functionType;
}

Rtti*
RttiParser::decodeTypeset()
{
  uint32_t count = decodeUint32();
  Rtti* typeset = new Rtti(cb::kTypeset);

  for (uint32_t i = 0; i < count; i++) {
    typeset->addSignature(decodeNew());
  }
  return typeset;
}

bool
RttiParser::match(uint8_t b)
{
  // TODO: error on offset_ >= length_
  if (offset_ >= length_)
    return false;

  if (bytes_[offset_] != b)
    return false;

  offset_++;
  return true;
}

uint32_t
RttiParser::decodeUint32()
{
  uint32_t value = 0;
  uint32_t shift = 0;
  // TODO: error on offset_ >= length_
  while (offset_ < length_) {
    uint8_t b = bytes_[offset_++];
    value |= (b & 0x7f) << shift;
    if ((b & 0x80) == 0)
      break;
    shift += 7;
  }
  return value;
}

Rtti::Rtti(uint8_t type)
 : type_(type),
   index_(0),
   inner_(nullptr),
   is_const_(false),
   is_by_ref_(false),
   is_variadic_(false)
{
}

Rtti::Rtti(uint8_t type, uint32_t size)
 : type_(type),
   index_(size),
   inner_(nullptr),
   is_const_(false),
   is_by_ref_(false),
   is_variadic_(false)
{
}

Rtti::Rtti(uint8_t type, Rtti* inner)
 : type_(type),
   index_(0),
   inner_(inner),
   is_const_(false),
   is_by_ref_(false),
   is_variadic_(false)
{
}

Rtti::Rtti(uint8_t type, uint32_t size, Rtti* inner)
 : type_(type),
   index_(size),
   inner_(inner),
   is_const_(false),
   is_by_ref_(false),
   is_variadic_(false)
{
}

Rtti::Rtti(Rtti* return_type, bool is_variadic)
 : type_(cb::kFunction),
   index_(0),
   inner_(return_type),
   is_const_(false),
   is_by_ref_(false),
   is_variadic_(is_variadic)
{
}

void
Rtti::setConst()
{
  is_const_ = true;
}

void
Rtti::setByRef()
{
  is_by_ref_ = true;
}

void
Rtti::addArgument(Rtti* arg)
{
  assert(type_ == cb::kFunction);
  args_.append(arg);
}

void
Rtti::addSignature(Rtti* arg)
{
  assert(type_ == cb::kTypeset);
  args_.append(arg);
}
