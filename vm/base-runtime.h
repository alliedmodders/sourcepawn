// vim: set sts=2 ts=8 sw=2 tw=99 et:
//
// Copyright (C) 2026 AlliedModders LLC
//
// This file is part of SourcePawn. SourcePawn is free software: you can
// redistribute it and/or modify it under the terms of the GNU General Public
// License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
//
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.
//
#pragma once

#include <memory>
#include <string>

#include <amtl/am-refcounting.h>
#include <amtl/am-inlinelist.h>
#include <sp_vm_api.h>
#include "base-method-info.h"
#include "smx-image.h"

namespace sp {

class Environment;

namespace v2 {
class Runtime;
} // namespace v2

struct ARRAY_HANDLE;
typedef ARRAY_HANDLE* ARRAY_PTR;

class BaseRuntime : public SourcePawn::IPluginRuntime
{
  public:
    explicit BaseRuntime(std::shared_ptr<SmxImage> image);
    virtual ~BaseRuntime();

    SmxImage* image() const { return image_.get(); }

    typedef SmxImage::Code Code;
    typedef SmxImage::Data Data;

    const Code& code() const { return code_; }
    const Data& data() const { return data_; }

    virtual ke::RefPtr<BaseMethodInfo> GetMethodFromFrameId(uint32_t frame_id) const = 0;
    virtual v2::Runtime* AsV2() { return nullptr; }
    virtual bool Initialize() = 0;

    const char* Name() const { return name_.c_str(); }

    void SetNames(const char* full, const char* shortname) {
        full_name_ = full;
        name_ = shortname;
    }

    BaseRuntime* GetBaseContext() { return this; }

    virtual void InstallBuiltinNatives() = 0;
    virtual const sp_native_t* GetNative(uint32_t index) = 0;
    virtual uint32_t GetNativesNum() = 0;
    virtual int FindNativeByName(const char* name, uint32_t* index) = 0;
    virtual int UpdateNativeBinding(uint32_t index, SPVM_NATIVE_FUNC pfn, uint32_t flags,
                                    void* data) = 0;
    virtual int UpdateNativeBindingObject(uint32_t index, SourcePawn::INativeCallback* callback,
                                          uint32_t flags, void* data) = 0;
    virtual int FindPublicByName(const char* name, uint32_t* index) = 0;
    virtual int GetPublicByIndex(uint32_t index, sp_public_t** publicptr) = 0;
    virtual uint32_t GetPublicsNum() = 0;
    virtual bool PerformFullValidation() = 0;
    virtual bool CallGlobalCtor() { return true; }

    /**
     * @brief Returns the local parameter stack, starting from the
     * cell that contains the number of parameters passed.
     *
     * Local parameters are the parameters passed to the function
     * from which a native was called (and thus this can only be
     * called inside a native).
     *
     * Note: this is only supported in v1 runtimes.
     *
     * @return        Parameter stack.
     */
    virtual cell_t* GetLocalParams() = 0;

    // IPluginRuntime implementation
    void SetKey(int k, void* value);
    bool GetKey(int k, void** value) override;
    void ReportError(const char* fmt, ...) override;
    void ReportErrorVA(const char* fmt, va_list ap) override;
    void ReportFatalError(const char* fmt, ...) override;
    void ReportFatalErrorVA(const char* fmt, va_list ap) override;
    void ReportErrorNumber(int error) override;
    cell_t ThrowNativeErrorEx(int error, const char* msg, ...) override;
    cell_t ThrowNativeError(const char* msg, ...) override;
    int GetLastNativeError() override;
    cell_t BlamePluginError(SourcePawn::IPluginFunction* pf, const char* msg, ...) override;
    SourcePawn::IFrameIterator* CreateFrameIterator() override;
    void DestroyFrameIterator(SourcePawn::IFrameIterator* it) override;
    bool IsPaused() override;

    // Convert a parameter address to an ARRAY_PTR handle.
    //
    // @param base      Array base.
    // @param out       Array pointer handle.
    virtual int ParamToArrayPtr(cell_t base, ARRAY_PTR* out) = 0;

    // Return the data vector for an array.
    //
    // For character arrays, the pointer should be casted to a uint8_t* or char*.
    // For int64 arrays, the pointer should be casted to an int64_t* or uint64_t*.
    // For all other types, the pointer should be casted to a cell_t*.
    //
    // If UsesDirectArrays() is false, note that |data[i]| will not yield an
    // interior array pointer if the array has interior arrays. Instead, the
    // formula is:
    //
    //      array_base + (i * sizeof(cell_t)) + data[i]
    //
    // @param handle    Array pointer handle.
    // @param size      Optional pointer to store size of the array. If zero,
    //                  and the return pointer is not null, then the array
    //                  length is not supported.
    // @return          Pointer to the data vector for the array, or null if
    //                  the array has no data vector (zero length).
    virtual void* GetArrayData(ARRAY_PTR handle, uint32_t* size = nullptr) = 0;

    // Convert an internal address representing a heap-allocated array to an
    // ARRAY_PTR.
    virtual int LocalToArrayPtr(cell_t addr, ARRAY_PTR* out) = 0;

    const char* GetFilename() override { return full_name_.c_str(); }
    virtual BaseRuntime* GetBaseRuntime() override { return this; }
    SourcePawn::IPluginRuntime* GetRuntime() override { return this; }
    SourcePawn::ISourcePawnEngine* GetEnvironment() override;
    unsigned char* GetCodeHash();
    unsigned char* GetDataHash();
    void SetPauseState(bool paused);

  protected:
    Environment* env_;
    std::shared_ptr<sp::SmxImage> image_;
    std::string name_;
    std::string full_name_;
    Code code_;
    Data data_;

    // Checksumming.
    bool computed_code_hash_;
    bool computed_data_hash_;
    unsigned char code_hash_[16];
    unsigned char data_hash_[16];

    void* m_keys[4];
    bool m_keys_set[4];

    // Pause state.
    bool paused_ = false;
};

} // namespace sp
