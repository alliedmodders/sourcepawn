// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// Copyright (C) 2004-2021 AlliedModers LLC
//
// This file is part of SourcePawn. SourcePawn is licensed under the GNU
// General Public License, version 3.0 (GPL). If a copy of the GPL was not
// provided with this file, you can obtain it here:
//   http://www.gnu.org/licenses/gpl.html
//
#ifndef _include_sourcepawn_rtti_h_
#define _include_sourcepawn_rtti_h_

#include <array>
#include <memory>
#include <vector>

#include <amtl/am-hashmap.h>
#include <sp_vm_types.h>

namespace sp {
namespace debug {

class Rtti;

class RttiData
{
  public:
    RttiData();
    RttiData(const uint8_t* blob, uint32_t size);

    const Rtti* typeFromTypeId(uint32_t type_id) const;
    const Rtti* functionTypeFromOffset(uint32_t offset) const;
    const Rtti* typesetTypeFromOffset(uint32_t offset) const;

    const uint8_t* blob() const;
    size_t size() const;

    bool validateType(uint32_t type_id) const;
    bool validateFunctionOffset(uint32_t offset) const;
    bool validateTypesetOffset(uint32_t offset) const;
    bool validateLocalSlots(uint32_t offset) const;

  private:
    const uint8_t* rtti_data_;
    uint32_t rtti_data_size_;
};

// Prefer this over RttiParser/Rtti, since it does not call malloc().
class FastRtti final {
  public:
    FastRtti(const uint8_t* data, size_t size, uint32_t offset)
      : data_(data),
        size_(size),
        offset_(offset)
    {}
    explicit FastRtti(uint32_t type_id_payload)
      : data_(inline_bytes_.data()),
        size_(inline_bytes_.size()),
        offset_(0)
    {
        inline_bytes_[0] = (type_id_payload >> 0) & 0xff;
        inline_bytes_[1] = (type_id_payload >> 8) & 0xff;
        inline_bytes_[2] = (type_id_payload >> 16) & 0xff;
        inline_bytes_[3] = (type_id_payload >> 24) & 0xff;
    }

    bool ReadFunctionSignatureArgCount(uint32_t* out);
    bool ReadLocalSlotCount(uint16_t* out);
    bool GetByte(uint8_t* out) const;
    bool GetNextByte(uint8_t* out);
    bool SkipNextType();

    void NextByte() { offset_++; }

    bool ReadCompactUint32(uint32_t* out);

  private:
    std::array<uint8_t, 4> inline_bytes_{};
    const uint8_t* data_;
    size_t size_;
    uint32_t offset_;
};

// Do not use in performance critical code.
class RttiParser
{
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

class Rtti
{
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

} // namespace debug
} // namespace sp

#endif // _include_sourcepawn_rtti_h_
