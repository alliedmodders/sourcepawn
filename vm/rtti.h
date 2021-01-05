// vim: set sts=2 ts=8 sw=2 tw=99 et:
//
// Copyright (C) 2004-2019 AlliedModers LLC
//
// This file is part of SourcePawn. SourcePawn is licensed under the GNU
// General Public License, version 3.0 (GPL). If a copy of the GPL was not
// provided with this file, you can obtain it here:
//   http://www.gnu.org/licenses/gpl.html
//
#ifndef _include_sourcepawn_rtti_h_
#define _include_sourcepawn_rtti_h_

#include <memory>
#include <vector>

#include <sp_vm_types.h>
#include <amtl/am-hashmap.h>

namespace sp {

  class Rtti;
  class SmxV1Image;

  class RttiData {
  public:
    RttiData();
    RttiData(const uint8_t* blob, uint32_t size);

    const Rtti* typeFromTypeId(uint32_t type_id);
    const Rtti* functionTypeFromOffset(uint32_t offset);
    const Rtti* typesetTypeFromOffset(uint32_t offset);

    const uint8_t* blob();
    size_t size();

    bool validateType(uint32_t type_id);
    bool validateFunctionOffset(uint32_t offset);
    bool validateTypesetOffset(uint32_t offset);

  private:
    const uint8_t* rtti_data_;
    uint32_t rtti_data_size_;
  };

  class RttiParser {
  public:
    RttiParser(const uint8_t* bytes, uint32_t length, uint32_t offset);

    Rtti* decodeNew();
    Rtti* decodeFunction();
    Rtti* decodeTypeset();

    bool validate();
    bool validateFunction();
    bool validateTypeset();

  private:
    Rtti* decode();
    bool match(uint8_t b);
    uint32_t decodeUint32();
    bool tryDecodeUint32();

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
    bool isConst() const {
      return is_const_;
    }
    bool isByRef() const {
      return is_by_ref_;
    }
    uint8_t type() const {
      return type_;
    }
    const uint32_t index() const {
      return index_;
    }
    const Rtti* inner() const {
      return inner_.get();
    }
    bool isVariadic() const {
      return is_variadic_;
    }

  private:
    uint8_t type_;
    uint32_t index_;
    std::unique_ptr<const Rtti> inner_;
    bool is_const_;

    // Arguments
    bool is_by_ref_;

    // Function type only
    std::vector<std::unique_ptr<const Rtti>> args_;
    bool is_variadic_;
  };

} // namespace sp

#endif // _include_sourcepawn_rtti_h_
