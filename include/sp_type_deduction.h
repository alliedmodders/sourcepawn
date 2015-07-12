// vim: set ts=4 sw=4 tw=99 et:
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
#ifndef _include_sourcepawn_include_type_deduction_h_
#define _include_sourcepawn_include_type_deduction_h_

#include <smx/smx-v2.h>
#include <am-utility.h>
#include <sp_native_api.h>

namespace SourcePawn {

class IPluginContext;

namespace deduce {

template <typename T>
struct is_plugin_context {
  static const bool value = false;
};
template <>
struct is_plugin_context<SourcePawn::IPluginContext *> {
  static const bool value = true;
};

template <typename... T>
struct starts_with_plugin_context;
template <typename First, typename... Rest>
struct starts_with_plugin_context<First, Rest...> {
  static const bool value = is_plugin_context<First>::value;
};
template <>
struct starts_with_plugin_context<> {
  static const bool value = false;
};

template <typename T>
struct is_lvalue_ref {
  static const bool value = false;
};
template <typename T>
struct is_lvalue_ref<T&> {
  static const bool value = true;
};

template <typename T>
struct is_rvalue_ref {
  static const bool value = false;
};
template <typename T>
struct is_rvalue_ref<T&&> {
  static const bool value = true;
};

template <typename T>
struct is_volatile {
  static const bool value = false;
};
template <typename T>
struct is_volatile<T volatile> {
  static const bool value = true;
};

// Helpers.
template <typename T>
struct remove_volatile {
  typedef T type;
};
template <typename T>
struct remove_volatile<T volatile> {
  typedef T type;
};
template <typename T>
struct remove_const {
  typedef T type;
};
template <typename T>
struct remove_const<T const> {
  typedef T type;
};
template <typename T>
struct remove_cv {
  typedef typename remove_volatile<typename remove_const<T>::type>::type type;
};

template <typename T>
struct is_pointer_helper {
  static const bool value = false;
  static size_t depth() {
    return 0;
  }
};
template <typename T>
struct is_pointer_helper<T*> {
  static const bool value = true;
  static size_t depth() {
    return 1 + typename is_pointer_helper<typename remove_cv<T>::type>::depth();
  }
};
template <typename T>
struct is_pointer : public is_pointer_helper<typename remove_cv<T>::type>
{};

template <typename T>
struct remove_pointer_helper {
  typedef T type;
};
template <typename T>
struct remove_pointer_helper<T *> {
  typedef T type;
};
template <typename T>
struct remove_pointer : public remove_pointer_helper<typename remove_cv<T>::type>
{};

template <typename T>
struct remove_lvalue_ref {
  typedef T type;
};
template <typename T>
struct remove_lvalue_ref<T&> {
  typedef T type;
};

/////////////////////////////////
// Normal parameter deduction. //
/////////////////////////////////

template <typename T>
struct arg_info_helper
{
  static const bool legal = false;
  static const bool can_use_as_ref = false;
};

template <>
struct arg_info_helper<int>
{
  static const bool legal = true;
  static const bool can_use_as_ref = true;
  static const uint8_t key = uint8_t(sp::TypeSpec::int32);
};

template <>
struct arg_info_helper<float>
{
  static const bool legal = true;
  static const bool can_use_as_ref = true;
  static const uint8_t key = uint8_t(sp::TypeSpec::float32);
};

template <>
struct arg_info_helper<void>
{
  static const bool legal = true;
  static const bool can_use_as_ref = false;
  static const uint8_t key = uint8_t(sp::TypeSpec::void_t);
};

template <typename T>
struct arg_info : public arg_info_helper<typename remove_cv<T>::type>
{};

// Special case for "const T *" which is really "ptr(const T)". We have to
// strip away the pointer, strip away cv-qualifiers, then add the pointer
// back.
template <typename T>
struct arg_info<T*> : public arg_info_helper<typename remove_cv<T>::type *>
{};

// Verify that a type will be successfully handled by the Sourcepawn native
// marshaller. Do not disable these asserts.
template <typename T>
static inline void check_arg_type_encodable()
{
  typedef typename remove_lvalue_ref<T>::type BaseType;
  // If any of these asserts fail, check the compiler output for Current/T
  // to see which argument type is causing problems. You may need to add
  // support to sp_type_deduction.h if the type is trivial. Complex types
  // will need additional JIT/VM support.
  static_assert(!is_volatile<T>::value, "SourcePawn does not support volatile.");
  static_assert(!is_volatile<typename remove_pointer<T>::type>::value, "SourcePawn does not support volatile.");
  static_assert(!is_rvalue_ref<T>::value, "SourcePawn does not have rvalue-references.");
  static_assert(!is_lvalue_ref<T>::value || arg_info<BaseType>::can_use_as_ref,
                "SourcePawn does not support this type as a reference parameter.");
  static_assert(arg_info<BaseType>::legal, "SourcePawn does not recognize or support this argument type.");
}

// Argument verification.
template <typename... Args>
struct arg_verifier;

template <typename Current, typename... Rest>
struct arg_verifier<Current, Rest...> {
  static void verify() {
    check_arg_type_encodable<Current>();
    arg_verifier<Rest...>::verify();
  }
};
template <>
struct arg_verifier<> {
  static void verify() {
  }
};

// Helper for building compile-time byte arrays.
template <uint8_t... Bytes>
struct sig_buffer {
  static const uint8_t bytes[sizeof...(Bytes)];
  static const size_t length = sizeof...(Bytes);
};
template <uint8_t... Bytes>
const uint8_t sig_buffer<Bytes...>::bytes[sizeof...(Bytes)] = { Bytes... };

// Helper for boxing up parameter packs, for specialization.
template <typename... T>
struct arg_pack;

// Templates for argument encoding.
template <typename T, uint8_t... Bytes>
struct arg_encoder_base {
  typedef sig_buffer<> res;
};

template <char... Bytes>
struct arg_encoder_base<arg_pack<>, Bytes...> {
  typedef sig_buffer<Bytes...> res;
};

template <typename First, typename... Rest, uint8_t... Bytes>
struct arg_encoder_base<arg_pack<First, Rest...>, Bytes...> {
  typedef typename arg_encoder_base<arg_pack<Rest...>,
                                    Bytes...,
                                    arg_info<First>::key>::res res;
};

template <typename First, typename... Rest, uint8_t... Bytes>
struct arg_encoder_base<arg_pack<First&, Rest...>, Bytes...> {
  typedef typename arg_encoder_base<arg_pack<Rest...>,
                                    Bytes...,
                                    uint8_t(sp::TypeSpec::byref),
                                    arg_info<First>::key>::res res;
};

template <typename... T>
struct arg_encoder {
  typedef typename arg_encoder_base<arg_pack<T...>>::res res;
};

// Entry point to signature deduction. There are two specializations, one for
// signatures with IPluginContext* and one for without.
template <typename RetVal, typename... Rest>
struct sig_encoder
{
  typedef arg_encoder<RetVal, Rest...> encoder;
  typedef arg_verifier<RetVal, Rest...> verifier;
  static const uint8_t flags = 0;
};

template <typename RetVal, typename... Rest>
struct sig_encoder<RetVal, IPluginContext*, Rest...>
{
  typedef arg_encoder<RetVal, Rest...> encoder;
  typedef arg_verifier<RetVal, Rest...> verifier;
  static const uint8_t flags = NativeSpecFlags::HasPluginContext;
};

// Use the old-style signature.
template <>
struct sig_encoder<cell_t, IPluginContext*, const CellArgs&> {
  typedef arg_encoder<void> encoder;
  typedef arg_verifier<> verifier;
  static const uint8_t flags = NativeSpecFlags::UsesLegacySignature;
};

// Verify the signature and return its encoding.
template <typename R, typename... Args>
const NativeSpec* Build(R (func)(Args...))
{
  typedef sig_encoder<R, Args...> sig;

  sig::verifier::verify();

  static NativeSpec spec = {
    sig::encoder::res::bytes,
    sig::encoder::res::length,
    reinterpret_cast<void *>(func),
    sig::flags,
  };

  return &spec;
}

} // namespace deduce

template <typename R, typename... Args>
const NativeSpec* Deduce(R (func)(Args...))
{
  return deduce::Build(func);
}

} // namespace SourcePawn

#endif // _include_sourcepawn_include_type_deduction_h_
