// vim: set sts=2 ts=8 sw=2 tw=99 et:
//
// Copyright (C) 2004-2015 AlliedModers LLC
//
// This file is part of SourcePawn. SourcePawn is licensed under the GNU
// General Public License, version 3.0 (GPL). If a copy of the GPL was not
// provided with this file, you can obtain it here:
//   http://www.gnu.org/licenses/gpl.html
//
#ifndef _include_sourcepawn_rtti_h_
#define _include_sourcepawn_rtti_h_

#include <sp_vm_types.h>
#include <amtl/am-vector.h>
#include <amtl/am-hashmap.h>
#include <amtl/am-autoptr.h>

namespace sp {

  class Rtti;
  class SmxV1Image;

  template <typename T>
  struct IntegerPolicy
  {
    static inline uint32_t hash(const T key)
    {
      return ke::HashInt32(key);
    }
    static inline bool matches(const T find, const T &key)
    {
      return (key == find);
    }
  };

  typedef ke::HashMap<uint32_t, ke::AutoPtr<Rtti>, IntegerPolicy<uint32_t>> TypeCacheMap;
  class RttiData {
  public:
    RttiData();
    RttiData(SmxV1Image* image, const uint8_t* blob, size_t size);

    Rtti* typeFromTypeId(uint32_t type_id);
    Rtti* functionTypeFromOffset(uint32_t offset);
    Rtti* typesetTypeFromOffset(uint32_t offset);

    const uint8_t* blob();
    size_t size();

  private:
    TypeCacheMap rtti_cache_;
    const uint8_t* rtti_data_;
    size_t rtti_data_size_;
    SmxV1Image* image_;
  };

  class RttiParser {
  public:
    RttiParser(const uint8_t* bytes, uint32_t length, uint32_t offset);

    Rtti* decodeNew();
    Rtti* decodeFunction();
    Rtti* decodeTypeset();

  private:
    Rtti* decode();
    bool match(uint8_t b);
    uint32_t decodeUint32();

  private:
    const uint8_t* bytes_;
    uint32_t length_;
    uint32_t offset_;
    bool is_const_;
  };

  class Rtti {
  public:
    Rtti(uint8_t type);
    Rtti(uint8_t type, uint32_t index);
    Rtti(uint8_t type, Rtti* inner);
    Rtti(uint8_t type, uint32_t index, Rtti* inner);
    Rtti(Rtti* return_type, bool variadic);

    void setConst();
    void setByRef();
    void addArgument(Rtti* arg);
    void addSignature(Rtti* signature);

  public:
    bool isConst() {
      return is_const_;
    }
    bool isByRef() {
      return is_by_ref_;
    }
    uint8_t type() {
      return type_;
    }
    const uint32_t index() {
      return index_;
    }
    Rtti* inner() {
      return inner_;
    }
    bool isVariadic() {
      return is_variadic_;
    }

  private:
    uint8_t type_;
    uint32_t index_;
    ke::AutoPtr<Rtti> inner_;
    bool is_const_;

    // Arguments
    bool is_by_ref_;

    // Function type only
    ke::Vector<ke::AutoPtr<Rtti>> args_;
    bool is_variadic_;
  };

} // namespace sp

#endif // _include_sourcepawn_rtti_h_
