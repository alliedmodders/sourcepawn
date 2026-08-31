// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
//
#pragma once

#include <stddef.h>

namespace sp {

template <typename T>
struct MemberFunctionArgCount;

template <typename R, typename C, typename... Args>
struct MemberFunctionArgCount<R (C::*)(Args...)>
{
    static constexpr size_t value = sizeof...(Args);
};

} // namespace sp
