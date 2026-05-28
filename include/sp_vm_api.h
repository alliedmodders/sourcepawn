// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
// Copyright (C) 2006-2015 AlliedModders LLC
//
// This file is part of SourcePawn. SourcePawn is free software: you can
// redistribute it and/or modify it under the terms of the GNU General Public
// License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
//
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.
//
#ifndef _INCLUDE_SOURCEPAWN_VM_API_H_
#define _INCLUDE_SOURCEPAWN_VM_API_H_

/**
 * @file sp_vm_api.h
 * @brief Contains all of the object structures used in the SourcePawn API.
 */

#include <assert.h>
#include <stdio.h>
#include "sp_vm_types.h"
#include "sp_callable.h"

/** SourcePawn Engine API Versions */

namespace SourceMod {
struct IdentityToken_t;
} // namespace SourceMod
namespace sp {
class BaseRuntime;
class Environment;
} // namespace sp

namespace SourcePawn {
class ExceptionHandler;
class IPluginRuntime;
class ISourcePawnEnvironment;
class IVirtualMachine;
typedef ISourcePawnEnvironment ISourcePawnEngine;
typedef ISourcePawnEnvironment ISourcePawnEngine2;

/**
   * @brief Pseudo-NULL reference types.
   */
enum SP_NULL_TYPE {
    SP_NULL_VECTOR = 0, /**< Float[3] reference */
    SP_NULL_STRING = 1, /**< const String[1] reference */
};

enum {
  // If this flag is set, any metadata files generated will be deleted at the end of the process.
  // This is useful when paired with lightweight metadata, as it allows attaching to a running
  // process without filling up the disk. Enabled by default.
  JIT_DEBUG_DELETE_ON_EXIT = (1 << 0),

  // Enables output of function name mappings for perf. Linux only, enabled by default.
  JIT_DEBUG_PERF_BASIC = (1 << 1),

  // Enables output of full function metadata for perf in the jitdump format.
  // Includes names, bytecode, and source file mappings. Linux only.
  JIT_DEBUG_PERF_JITDUMP = (1 << 2),
};

// @brief Interface for a refcounted objects.
class IRefcountedObject
{
  public:
    // Virtual destructor.
    virtual ~IRefcountedObject() {}

    // @brief Increase the object's reference count.
    virtual void AddRef() = 0;

    // @brief Decrease the object's reference count, freeing any resources once
    // the reference count reaches zero.
    virtual void Release() = 0;
};

// @brief Interface for a native callback.
class INativeCallback : public IRefcountedObject
{
  public:
    // @brief Called when a plugin invokes this native. The signature is the same
    // as a normal native callback.
    //
    // @param ctx       Plugin context.
    // @param params    Parameter vector.
    // @return          Return value (ignored if an error is thrown).
    virtual cell_t Invoke(IPluginContext* ctx, const cell_t* params) = 0;
};

 /**
   * @brief Encapsulates a function call in a plugin.
   *
   * NOTE: This object should not be deleted.  It lives for the lifetime of the plugin.
   */
class IPluginFunction
{
  public:
    /**
     * @brief Executes the function, resets the pushed parameter list, and
     * performs any copybacks.
     *
     * The exception state is reset upon entering and leaving this
     * function. Callers that want to propagate exceptions from Execute()
     * should use Invoke(). ReportError is not preferred since it would
     * lose any custom exception messages.
     *
     * @param result    Pointer to store return value in.
     * @return        Error code, if any.
     */
    virtual int Execute(cell_t* result) = 0;

    /**
     * @brief Deprecated, do not use.
     *
     * @return        GetDefaultContext() of parent runtime.
     */
    virtual IPluginContext* GetParentContext() = 0;

    /**
     * @brief Returns whether the parent plugin is paused.
     *
     * @return        True if runnable, false otherwise.
     */
    virtual bool IsRunnable() = 0;

    /**
     * @brief Returns the function ID of this function.
     *
     * Note: This was added in API version 4.
     *
     * @return        Function id.
     */
    virtual funcid_t GetFunctionID() = 0;

    /**
     * @brief Returns parent plugin's runtime
     *
     * @return        IPluginRuntime pointer.
     */
    virtual IPluginRuntime* GetParentRuntime() = 0;

    /**
     * @brief Returns a name to identify this function for debugging purposes.
     *
     * @return       String name.
     */
    virtual const char* DebugName() = 0;

    // These functions are provided for source-level compatibility, but are
    // deprecated. Invoke() should be used instead. Errors are never returned.
    virtual void Cancel() = 0;
    virtual int PushCell(cell_t cell) = 0;
    virtual int PushCellByRef(cell_t* cell, int flags = SM_PARAM_COPYBACK) = 0;
    virtual int PushFloat(float number) = 0;
    virtual int PushFloatByRef(float* number, int flags = SM_PARAM_COPYBACK) = 0;
    virtual int PushArray(cell_t* inarray, unsigned int cells, int flags = 0) = 0;
    virtual int PushString(const char* string) = 0;
    virtual int PushStringEx(char* buffer, size_t length, int sz_flags, int cp_flags) = 0;
    virtual int PushInt64(int64_t value) = 0;
    virtual bool Invoke(cell_t* rval = nullptr) = 0;

    /**
     * @brief Executes the function, resets the pushed parameter list, and
     * performs any copybacks.
     *
     * Unlike Execute(), this does not reset the exception state. It is
     * illegal to call Invoke() while an exception is unhandled. If it
     * returns false, an exception has been thrown, and must either be
     * handled via ExceptionHandler or propagated to its caller.
     *
     * @param result    Pointer to store return value in.
     * @return          True on success, false on error.
     */
    virtual bool Invoke(const sp::CallArgs& args, cell_t* rval = nullptr) = 0;
};

/**
   * @brief Allows inspecting the stack frames of the SourcePawn environment.
   *
   * Invoking VM functions while iterating frames will cause the iterator
   * to become corrupt.
   *
   * Frames iterate in most-recent to least-recent order.
   */
class IFrameIterator
{
  public:
    /**
     * @brief Returns whether or not there are more frames to read.
     *
     * @return          True if there are more frames to read, false otherwise.
     */
    virtual bool Done() const = 0;

    /**
     * @brief Advances to the next frame.
     *
     * Note that the iterator starts at either a valid frame or no frame.
     */
    virtual void Next() = 0;

    /**
     * @brief Resets the iterator to the top of the stack.
     */
    virtual void Reset() = 0;

    /**
     * @brief Returns the context owning the current frame, if any.
     *
     * @return          Context, or null.
     */
    virtual IPluginContext* Context() const = 0;

    /**
     * @brief Returns whether or not the current frame is a native frame. If it
     * is, line numbers and file paths are not available.
     *
     * @return          True if a native frame, false otherwise.
     */
    virtual bool IsNativeFrame() const = 0;

    /**
     * @brief Returns true if the frame is a scripted frame.
     *
     * @return          True if a scripted frame, false otherwise.
     */
    virtual bool IsScriptedFrame() const = 0;

    /**
     * @brief Returns the line number of the current frame, or 0 if none is
     * available.
     *
     * @return          Line number on success, 0 on failure.
     */
    virtual unsigned LineNumber() const = 0;

    /**
     * @brief Returns the function name of the current frame, or null if
     * none could be computed.
     *
     * @return          Function name on success, null on failure.
     */
    virtual const char* FunctionName() const = 0;

    /**
     * @brief Returns the file path of the function of the current frame,
     * or none could be computed.
     *
     * @return          File path on success, null on failure.
     */
    virtual const char* FilePath() const = 0;

    /**
     * @brief Returns true if the frame is an internal frame and should not be
     * used for display.
     *
     * @return          True if an internal frame, false otherwise.
     */
    virtual bool IsInternalFrame() const = 0;
};

/**
   * @brief Interface to managing a debug context at runtime.
   */
struct ARRAY_HANDLE;
typedef ARRAY_HANDLE* ARRAY_PTR;

/**
   * @brief Interface to managing a runtime plugin.
   */
class IPluginRuntime
{
  public:
    /**
     * @brief Virtual destructor (you may call delete).
     */
    virtual ~IPluginRuntime() {}

    /**
     * @brief Gets public variable info by index.
     *
     * @param index      Public variable index number.
     * @param pubvar    Optionally filled with pointer to pubvar structure.
     */
    virtual int GetPubvarByIndex(uint32_t index, sp_pubvar_t** pubvar) = 0;

    /**
     * @brief Finds a public variable by name.
     *
     * @param name      Name of pubvar
     * @param index      Optionally filled with pubvar index number.
     */
    virtual int FindPubvarByName(const char* name, uint32_t* index) = 0;

    /**
     * @brief Gets the addresses of a public variable.
     *
     * @param index      Index of public variable.
     * @param local_addr    Address to store local address in.
     * @param phys_addr    Address to store physically relocated in.
     */
    virtual int GetPubvarAddrs(uint32_t index, cell_t* local_addr, cell_t** phys_addr) = 0;

    /**
     * @brief Returns the number of public variables.
     *
     * @return        Number of public variables.
     */
    virtual uint32_t GetPubVarsNum() = 0;

    /**
     * @brief Returns a function by name.
     *
     * @param public_name    Name of the function.
     * @return          A new IPluginFunction pointer, NULL if not found.
     */
    virtual IPluginFunction* GetFunctionByName(const char* public_name) = 0;

    /**
     * @brief Deprecated. Use GetFunctionByIdOrNull or GetFunctionByIdOrError instead.
     *
     * @param func_id      Function ID.
     * @return          A new IPluginFunction pointer, NULL if not found.
     */
    virtual IPluginFunction* GetFunctionById(funcid_t func_id) = 0;

    /**
     * @brief Returns true if the plugin is in debug mode.
     *
     * @return        True if in debug mode, false otherwise.
     */
    virtual bool IsDebugging() = 0;

    /**
     * @brief Returns whether or not the plugin is paused (runnable).
     *
     * @return        Pause state (true = paused, false = not).
     */
    virtual bool IsPaused() = 0;

    /**
     * @brief Returns the estimated memory usage of this plugin.
     *
     * @return        Memory usage, in bytes.
     */
    virtual size_t GetMemUsage() = 0;

    /**
     * @brief Return the file or location this plugin was loaded from.
     */
    virtual const char* GetFilename() = 0;

    /**
     * @brief Returns whether the plugin was compiled with direct array support.
     *
     * Direct arrays were introduced in SourcePawn 1.11. Any plugin with this
     * feature will use absolute addressing for indirection vectors. Eg, for an
     * array of array of cells (int x[][]), you can extract x[3][5] by doing:
     *
     *     cell_t addr_of_x = ...;
     *     cell_t* phys_x;
     *     LocalToPhysAddr(addr_of_x, &phys_x);
     *     LocalToPhysAddr(phys_x[3], &phys_x);
     *     cell_t value = phys_x[5];
     *
     * This code does not work on plugins compiled prior to 1.11, because arrays
     * had relative indirection vectors.
     */
    virtual bool UsesDirectArrays() = 0;

    /**
     * @brief Converts a plugin reference to a physical address
     *
     * @param local_addr  Local address in plugin.
     * @param phys_addr    Optionally filled with relocated physical address.
     */
    virtual int LocalToPhysAddr(cell_t local_addr, cell_t** phys_addr) = 0;

    /**
     * @brief Converts a local address to a physical string.
     *
     * @param local_addr  Local address in plugin.
     * @param addr      Destination output pointer.
     */
    virtual int LocalToString(cell_t local_addr, char** addr) = 0;

    /**
     * @brief Converts a physical string to a local address.
     *
     * @param local_addr  Local address in plugin.
     * @param bytes      Number of chars to write, including NULL terminator.
     * @param source    Source string to copy.
     */
    virtual int StringToLocal(cell_t local_addr, size_t bytes, const char* source) = 0;

    /**
     * @brief Converts a physical UTF-8 string to a local address.
     * This function is the same as the ANSI version, except it will copy the maximum number
     * of characters possible without accidentally chopping a multi-byte character.
     *
     * @param local_addr    Local address in plugin.
     * @param maxbytes    Number of bytes to write, including NULL terminator.
     * @param source      Source string to copy.
     * @param wrtnbytes    Optionally set to the number of actual bytes written.
     */
    virtual int StringToLocalUTF8(cell_t local_addr, size_t maxbytes, const char* source,
                                  size_t* wrtnbytes) = 0;

    /**
     * @brief Throws a error and halts any current execution.
     *
     * This function is deprecated. Use ReportError() instead.
     *
     * @param error    The error number to set.
     * @param msg    Custom error message format.  NULL to use default.
     * @param ...    Message format arguments, if any.
     * @return      0 for convenience.
     */
    virtual cell_t ThrowNativeErrorEx(int error, const char* msg, ...) = 0;

    /**
     * @brief Throws a generic native error and halts any current execution.
     *
     * This function is deprecated. Use ReportError() instead.
     *
     * @param msg    Custom error message format.  NULL to set no message.
     * @param ...    Message format arguments, if any.
     * @return      0 for convenience.
     */
    virtual cell_t ThrowNativeError(const char* msg, ...) = 0;

    /**
     * @brief Returns a NULL reference based on one of the available NULL
     * reference types.
     *
     * @param type    NULL reference type.
     * @return      cell_t address to compare to.
     */
    virtual cell_t* GetNullRef(SP_NULL_TYPE type) = 0;

    /**
     * @brief Converts a local address to a physical string, and allows
     * for NULL_STRING to be set.
     *
     * @param local_addr  Local address in plugin.
     * @param addr      Destination output pointer.
     */
    virtual int LocalToStringNULL(cell_t local_addr, char** addr) = 0;

    /**
     * @brief Returns if there is currently an execution in progress.
     *
     * @return        True if in exec, false otherwise.
     */
    virtual bool IsInExec() = 0;

    /**
     * @brief Returns the parent runtime of a context.
     *
     * @return        Parent runtime.
     */
    virtual IPluginRuntime* GetRuntime() = 0;

    /**
     * @brief Returns whether a context is in an error state.
     *
     * This function is deprecated. Use DetectExceptions instead.
     *
     * This should only be used inside natives to determine whether
     * a prior call failed. The return value should only be compared
     * against SP_ERROR_NONE.
     */
    virtual int GetLastNativeError() = 0;

    /**
     * @brief Retrieves a previously set "key."
     *
     * @param key      Key number (values allowed: 1 through 4).
     * @param value      Pointer to store value.
     * @return        True on success, false on failure.
     */
    virtual bool GetKey(int k, void** value) = 0;

    /**
     * @brief Report an error.
     *
     * @param message      Error message format.
     * @param ...          Formatting arguments.
     */
    virtual void ReportError(const char* fmt, ...) = 0;

    /**
     * @brief Report an error with variadic arguments.
     *
     * @param message      Error message format.
     * @param ap           Formatting arguments.
     */
    virtual void ReportErrorVA(const char* fmt, va_list ap) = 0;

    /**
     * @brief Report a fatal error. Fatal errors cannot be caught by any
     * exception handler.
     *
     * @param message      Error message format.
     * @param ...          Formatting arguments.
     */
    virtual void ReportFatalError(const char* fmt, ...) = 0;

    /**
     * @brief Report a fatal error with variadic arguments. Fatal errors
     * cannot be caught by any exception handler.
     *
     * @param message      Error message format.
     * @param ap           Formatting arguments.
     */
    virtual void ReportFatalErrorVA(const char* fmt, va_list ap) = 0;

    /**
     * @brief Report an error by its builtin number.
     *
     * @param number       Error number.
     */
    virtual void ReportErrorNumber(int error) = 0;

    /**
     * @brief Report an error caused by a plugin, specifying a function
     * as the cause.
     */
    virtual cell_t BlamePluginError(IPluginFunction* pf, const char* msg, ...) = 0;

    /**
     * @brief Returns an IFrameIterator for the current call stack. Must
     * be freed by DestroyFrameIterator()
     *
     * Note: It is illegal to re-enter the VM while a frame iterator is
     * active. The iterator must be processed and destroyed before continuing
     * to run SourcePawn plugins.
     */
    virtual IFrameIterator* CreateFrameIterator() = 0;

    /**
     * @brief Frees an IFrameIterator object. Paired with CreateFrameIterator()
     */
    virtual void DestroyFrameIterator(IFrameIterator* it) = 0;

    /**
     * @brief Allocate a 2D array on the heap.
     *
     * Calls to HeapAlloc2dArray should be made within a heap scope (see
     * EnterHeapScope and LeaveHeapScope, or AutoEnterHeapScope).
     *
     * If |init| is not specified, the resulting array will be zeroed.
     *
     * The address stored in |local_addr| must not be passed to LocalToPhysAddr.
     * Instead, LocalToArrayPtr() should be used instead.
     *
     * If an error occurs, it is automatically reported.
     *
     * @param length        Number of elements in the array.
     * @param stride        Size (in cells) of each element in the array.
     * @param local_addr    Out address of the resulting array.
     * @param init          If non-null, array to copy. The length and stride
     *                      must be the same.
     */
    virtual bool HeapAlloc2dArray(unsigned int length, unsigned int stride, cell_t* local_addr,
                                  const cell_t* init) = 0;

    /**
     * @brief Save the current position of the heap so it can be restored later.
     *
     * This must be paired with a call to LeaveHeapScope.
     */
    virtual void EnterHeapScope() = 0;

    /**
     * @brief Free any heap allocations made since the last call to
     * EnterHeapScope.
     */
    virtual void LeaveHeapScope() = 0;

    /**
     * @brief Returns the value of a null function. If kCodeFeatureNullFunction
     * is set, this returns 0. If unset, this returns -1, the legacy value.
     */
    virtual cell_t GetNullFunctionValue() = 0;

    /**
     * @brief Returns whether the value is a null function.
     */
    virtual bool IsNullFunctionId(funcid_t func) = 0;

    /**
     * @brief Convert a funcid_t to an IPluginFunction, reporting an error if
     * the function is invalid. funcid_t can be INVALID_FUNCTION.
     *
     * If |func| is non-null and valid, |out| is set to the function and true
     * is returned.
     *
     * If |func| is non-null and invalid, |out| is set to null, false is
     * returned, and an error is reported.
     *
     * If |func| is GetNullFunctionValue(), |out| is set to null and true is
     * returned.
     */
    virtual bool GetFunctionByIdOrNull(funcid_t func, IPluginFunction** out) = 0;

    /**
     * @brief Convert a funcid_t to an IPluginFunction, reporting an error if
     * the function is invalid or null.
     */
    virtual IPluginFunction* GetFunctionByIdOrError(funcid_t func) = 0;

    /**
     * @brief Convert a local address to an ARRAY_PTR handle.
     *
     * @param base      Array base.
     * @param out       Array pointer handle.
     */
    virtual int LocalToArrayPtr(cell_t base, ARRAY_PTR* out) = 0;

    /**
     * @brief Return the data vector for an array.
     *
     * For character arrays, the pointer should be casted to a uint8_t* or char*.
     * For int64 arrays, the pointer should be casted to an int64_t* or uint64_t*.
     * For all other types, the pointer should be casted to a cell_t*.
     *
     * If UsesDirectArrays() is false, note that |data[i]| will not yield an
     * interior array pointer if the array has interior arrays. Instead, the
     * formula is:
     *
     *      array_base + (i * sizeof(cell_t)) + data[i]
     *
     * @param handle    Array pointer handle.
     * @param size      Optional pointer to store size of the array. If zero,
     *                  and the return pointer is not null, then the array
     *                  length is not supported.
     * @return          Pointer to the data vector for the array, or null if
     *                  the array has no data vector (zero length).
     */
    virtual void* GetArrayData(ARRAY_PTR handle, uint32_t* size = nullptr) = 0;

    /**
     * Downcast for embedder convenience.
     */
    virtual sp::BaseRuntime* GetBaseRuntime() = 0;

    /**
     * Return the parent environment.
     */
    virtual ISourcePawnEnvironment* GetEnvironment() = 0;

  public:
    // Source-level compatibility helper.
    SourceMod::IdentityToken_t* GetIdentity() {
        SourceMod::IdentityToken_t* token;
        if (!GetKey(1, (void**)&token))
            return nullptr;
        return token;
    }

    // Source-level compatibility helper.
    IPluginRuntime* GetDefaultContext() { return this; }
};

class AutoEnterHeapScope
{
  public:
    explicit AutoEnterHeapScope(IPluginContext* cx)
      : cx_(cx)
    {
        cx->EnterHeapScope();
    }
    AutoEnterHeapScope(AutoEnterHeapScope&& other)
      : cx_(other.cx_)
    {
        other.cx_ = nullptr;
    }
    ~AutoEnterHeapScope() {
        if (cx_)
            cx_->LeaveHeapScope();
    }

    AutoEnterHeapScope(const AutoEnterHeapScope& other) = delete;
    void operator =(const AutoEnterHeapScope& other) = delete;
    void operator =(AutoEnterHeapScope&& other) = delete;
  private:
    IPluginContext* cx_;
};

/**
   * @brief Information about a reported error.
   */
class IErrorReport
{
  public:
    /**
     * @brief Return the message of the error report.
     *
     * @return          Message string.
     */
    virtual const char* Message() const = 0;

    /**
     * @brief True if the error is fatal and cannot be handled (though
     * reporting can be suppressed).
     *
     * @return          True if fatal, false otherwise.
     */
    virtual bool IsFatal() const = 0;

    /**
     * @brief Return the plugin context that caused the error.
     *
     * @return          Plugin context.
     */
    virtual IPluginContext* Context() const = 0;

    /**
     * @brief Return the specific plugin function that caused the error.
     *
     * @return          Blamed function.
     */
    virtual IPluginFunction* Blame() const = 0;

    /**
     * @brief Return the error code of the error report.
     *
     * @return           SP_ERROR_* integer code.
     */
    virtual int Code() const = 0;
};

/**
   * @brief Provides callbacks for debug information.
   */
class IDebugListener
{
  public:
    /**
     * @brief Called on debug spew.
     *
     * @param msg    Message text.
     * @param fmt    Message formatting arguments (printf-style).
     */
    virtual void OnDebugSpew(const char* msg, ...) = 0;

    /**
     * @brief Called when an error is reported and no exception
     * handler was available.
     *
     * @param report  Error report object.
     * @param iter      Stack frame iterator.
     */
    virtual void ReportError(const IErrorReport& report, IFrameIterator& iter) = 0;
};

/**
   * @brief Encapsulates a profiling tool that may be attached to SourcePawn.
   */
class IProfilingTool
{
  public:
    /**
     * @brief Return the name of the profiling tool.
     *
     * @return      Profiling tool name.
     */
    virtual const char* Name() = 0;

    /**
     * @brief Description of the profiler.
     *
     * @return      Description.
     */
    virtual const char* Description() = 0;

    /**
     * @brief Called to render help text.
     *
     * @param  render     Function to render one line of text.
     */
    virtual void RenderHelp(void (*render)(const char* fmt, ...)) = 0;

    /**
     * @brief Initiate a start command.
     *
     * Initiate start commands through a profiling tool, returning whether
     * or not the command is supported. If starting, SourceMod will generate
     * events even if it cannot signal the external profiler.
     */
    virtual bool Start() = 0;

    /**
     * @brief Initiate a stop command.
     *
     * @param render      Function to render any help messages.
     */
    virtual void Stop(void (*render)(const char* fmt, ...)) = 0;

    /**
     * @brief Dump profiling information.
     *
     * Informs the profiling tool to dump any current profiling information
     * it has accumulated. The format and location of the output is profiling
     * tool specific.
     */
    virtual void Dump() = 0;

    /**
     * @brief Returns whether or not the profiler is currently profiling.
     *
     * @return      True if active, false otherwise.
     */
    virtual bool IsActive() = 0;

    /**
     * @brief Returns whether the profiler is attached.
     *
     * @return      True if attached, false otherwise.
     */
    virtual bool IsAttached() = 0;

    /**
     * @brief Enters the scope of an event.
     *
     * LeaveScope() mus be called exactly once for each call to EnterScope().
     *
     * @param group       A named budget group, or NULL for the default.
     * @param name        Event name.
     */
    virtual void EnterScope(const char* group, const char* name) = 0;

    /**
     * @brief Leave a profiling scope. This must be called exactly once for
     * each call to EnterScope().
     */
    virtual void LeaveScope() = 0;
};

struct sp_plugin_s;
typedef struct sp_plugin_s sp_plugin_t;

/**
   * @brief This class is the API for SourcePawn.
   */
class ISourcePawnEnvironment
{
  public:
    // Helper functions for source-level compatibility.
    ISourcePawnEnvironment* Environment() { return this; }
    ISourcePawnEngine* APIv1() { return this; }
    ISourcePawnEngine2* APIv2() { return this; }

    // Return the API version of this SourcePawn VM.
    virtual uint32_t GetApiVersion() = 0;

    /**
     * @brief Allocates executable memory.
     *
     * @param size    Size of memory to allocate.
     * @return      Pointer to memory, NULL if allocation failed.
     */
    virtual void* AllocatePageMemory(size_t size) = 0;

    /**
     * @brief Sets the input memory permissions to read+write.
     *
     * @param ptr    Memory block.
     */
    virtual void SetReadWrite(void* ptr) = 0;

    /**
     * @brief Sets the input memory permissions to read+execute.
     *
     * @param ptr    Memory block.
     */
    virtual void SetReadExecute(void* ptr) = 0;

    /**
     * @brief Frees executable memory.
     *
     * @param ptr    Address to free.
     */
    virtual void FreePageMemory(void* ptr) = 0;

    /**
     * @brief Returns the string representation of an error message.
     *
     * This function is deprecated and should not be used. The exception
     * handling API should be used instead.
     *
     * @param err    Error code.
     * @return      Error string, or NULL if not found.
     */
    virtual const char* GetErrorString(int err) = 0;

    // @brief Enters an exception handling scope. This is intended to be
    // used on the stack and must have a corresponding call to
    // LeaveExceptionHandlingScope. When in an exception handling scope,
    // exceptions are not immediately reported. Instead the caller is
    // responsible for propagation them or clearing them.
    virtual void EnterExceptionHandlingScope(ExceptionHandler* handler) = 0;

    // @brief Leaves the most recent exception handling scope. The handler
    // is provided as a sanity check.
    virtual void LeaveExceptionHandlingScope(ExceptionHandler* handler) = 0;

    // @brief Returns whether or not an exception is currently pending.
    virtual bool HasPendingException(const ExceptionHandler* handler) = 0;

    // @brief Returns the message of the pending exception.
    virtual const char* GetPendingExceptionMessage(const ExceptionHandler* handler) = 0;

    // @brief Returns the code of the pending exception.
    virtual int GetPendingExceptionCode(const ExceptionHandler* handler) = 0;
};

// @brief A helper class for handling exceptions.
//
// ExceptionHandlers can be used to detect, catch, and re-throw VM errors
// within C++ code.
//
// When throwing errors, SourcePawn creates an exception object. The
// exception object is global state. As long as an exception is present,
// all scripted code should immediately abort and return to their callers,
// all native code should exit, all code should propagate any error states
// until the exception is handled.
//
// In some cases, an error code is not available. For example, if a native
// detects an exception, it does not have an error status to propagate. It
// should simply return instead. The return value will be ignored; the VM
// knows to abort the script.
class ExceptionHandler
{
    friend class sp::Environment;

  public:
    ExceptionHandler(ISourcePawnEnvironment* api)
     : env_(api),
       catch_(true)
    {
        env_->EnterExceptionHandlingScope(this);
    }
    ExceptionHandler(IPluginContext* ctx)
     : env_(ctx->GetEnvironment()),
       catch_(true)
    {
        env_->EnterExceptionHandlingScope(this);
    }
    ~ExceptionHandler() {
        env_->LeaveExceptionHandlingScope(this);
    }

    // Propagates the exception instead of catching it. After calling this,
    // no more SourcePawn code should be executed until the exception is
    // handled. Callers should return immediately.
    void Rethrow() {
        assert(catch_ && HasException());
        catch_ = false;
    }

    bool HasException() const {
        return env_->HasPendingException(this);
    }

    const char* Message() const {
        return env_->GetPendingExceptionMessage(this);
    }

    int Code() const {
        return env_->GetPendingExceptionCode(this);
    }

  private:
    // Don't allow heap construction.
    ExceptionHandler(const ExceptionHandler& other);
    void operator =(const ExceptionHandler& other);
    void* operator new(size_t size);
    void operator delete(void*, size_t);

  private:
    ISourcePawnEnvironment* env_;
    ExceptionHandler* next_;

  protected:
    // If true, the exception will be swallowed.
    bool catch_;
};

// @brief An implementation of ExceptionHandler that simply collects
// whether an exception was thrown.
class DetectExceptions : public ExceptionHandler
{
  public:
    DetectExceptions(ISourcePawnEngine2* api)
     : ExceptionHandler(api)
    {
        catch_ = false;
    }
    DetectExceptions(IPluginContext* ctx)
     : ExceptionHandler(ctx)
    {
        catch_ = false;
    }
};

} // namespace SourcePawn

#endif //_INCLUDE_SOURCEPAWN_VM_API_H_
