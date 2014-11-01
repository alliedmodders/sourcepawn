// vim: set sts=2 ts=8 sw=2 tw=99 et :
// =============================================================================
// SourcePawn
// Copyright (C) 2004-2009 AlliedModders LLC.  All rights reserved.
// =============================================================================
// 
// This program is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License, version 3.0, as published by the
// Free Software Foundation.
// 
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
// 
// You should have received a copy of the GNU General Public License along with
// this program.  If not, see <http://www.gnu.org/licenses/>.
// 
// As a special exception, AlliedModders LLC gives you permission to link the
// code of this program (as well as its derivative works) to "Half-Life 2," the
// "Source Engine," the "SourcePawn JIT," and any Game MODs that run on software
// by the Valve Corporation.  You must obey the GNU General Public License in
// all respects for all other code used.  Additionally, AlliedModders LLC grants
// this exception to all derivative works.  AlliedModders LLC defines further
// exceptions, found in LICENSE.txt (as of this writing, version JULY-31-2007),
// or <http://www.sourcemod.net/license.php>.

#ifndef _include_sourcepawn_api_h_
#define _include_sourcepawn_api_h_

#include <am-vector.h>

namespace sp {

class Plugin;
struct sp_nativeinfo_s;

// A native definition.
struct NativeDef
{
  // Function pointer.
  void *fnptr;

  // Name to bind as.
  const char *name;

  // Type signature of the function. This should be encoded as a 0-terminated
  // string using the following name-mangling scheme:
  //  i: integer
  //  b: boolean
  //  f: 32-bit float
  //  c: 1-byte character
  //  v: void, only for return values.
  // 
  // There are three suffixes available per-character:
  //  '&': Indicates a reference to a value on the stack.
  //  '*': Indicates an old-style array; length is not available, and the
  //       pointer may not be stored on the heap.
  //  '[': Indicates a new-style array. May be repeated.
  //  '#': Indicates a variadic array. Only allowed on the last parameter.
  //
  // The format for the string should be:
  //    <ReturnType>(<Params>*)
  // I.e. "int Format(char[] buffer, int maxlength, object args...)" would be:
  //    "i(c*io#)
  //
  // New-style arrays are not yet available; old-style are from SourcePawn 1.
  // When binding, SourcePawn will bind new and old-style arrays to '*', but
  // only actual new-style arrays will bind to '['. Also note that SourcePawn 1
  // will not generate new-style bindings for multi-dimensional arrays, so '*'
  // cannot be repeated. However, '[' can be repeated any number of times.
  //
  // The SourcePawn 1 compiler will not emit new-style bindings for old-style
  // variadic functions. That is, X(...) cannot bind to any new-native signature.
  // X(int y...) however, will.
  //
  // The JIT will compute a "cdecl" or native ABI calling sequence based on the
  // provided signature. The C++ signature must be compatible. Here are some
  // examples:
  //   i(c*io#) -> int (*)(Context *, StackArray<char>, int, Arguments<Object> args);
  //   v(oo[)   -> void (*)(Context *, Handle<Object>, Handle<Array<Object>>);
  //   o[[()    -> Array<Array<Object>> (*)(Context *);
  //   i(bi&)    -> int (*)(Context *, bool, StackRef<int>);
  const char *signature;
};

// A Context contains information relevant to a running SourcePawn environment.
class Context
{
 public:
  // Return the topmost active plugin, or null if none exists.
  virtual Plugin *Plugin() = 0;
};

// Interface for a function object.
class Function
{
 public:
  bool Call(void *rval, ...);
};

// A Plugin is similar to a shared library; it is a container for code,
// modules, types, and data.
class Plugin
{
 public:
  // Finds a public function given a name and signature. The binding rules are
  // similar to natives.
  virtual Function *FindPublicFunction(const char *name, const char *signature) = 0;
};

// Can be implemented by the host application to catch and report errors.
class IErrorReporter
{
 public:
};

// A Runtime encapsulates everything that's needed to run SourcePawn code.
class SourcePawn
{
 public:
  // Sets the error reporter.
  virtual void SetErrorReporter(IErrorReporter *reporter) = 0;

  // Registers old-style natives (those that do not have type informaton).
  // The list is terminated by an entry with null fields.
  virtual bool RegisterOldNatives(struct sp_nativeinfo_s *natives) = 0;

  // Unregisters old-style natives. The list is terminated by an entry with
  // null fields.
  virtual bool UnregisterOldNatives(struct sp_nativeinfo_s *natives) = 0;

  // Registers new-style natives that can only be bound to a native with type
  // information.
  virtual bool RegisterNatives(const NativeDef *natives, size_t length) = 0;

  // Unregisters new-style natives.
  virtual bool UnregisterNatives(const NativeDef *natives, size_t length) = 0;

  // Loads a plugin via a path, executing any global code.
  virtual Handle<Plugin> LoadFromPath(const char *path) = 0;

  // Unloads a plugin. This may not remove the plugin; rather, it makes it
  // vulnerable to garbage collection.
  virtual void Detach(Handle<Plugin> plugin) = 0;

  // Sets a watchdog timer timeout.
  virtual bool InstallWatchdogTimer(size_t timeout_ms) = 0;

  // Call a function. |rval| must be nullptr if the return value is void,
  // otherwise it must be a pointer to a value of the returned type of the
  // function's signature.
  virtual bool CallFunction(Function *fun, void *rval, va_list ap) = 0;
};

} // namespace sp

#endif // _include_sourcepawn_api_h_
