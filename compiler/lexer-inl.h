// vim: set ts=8 sts=2 sw=2 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
// Copyright (c) ITB CompuPhase, 1997-2006

#pragma once

#include <stddef.h>

#include "errors.h"
#include "sc.h"

template <typename T>
static void
StringToCells(const char* str, size_t len, const T& litadd)
{
    ucell val = 0;
    int byte = 0;
    for (size_t i = 0; i < len; i++) {
        val |= (unsigned char)str[i] << (8 * byte);
        if (byte == sizeof(ucell) - 1) {
            litadd(val);
            val = 0;
            byte = 0;
        } else {
            byte++;
        }
    }
    if (byte != 0) {
        // There are zeroes to terminate |val|.
        litadd(val);
    } else {
        // Add a full cell of zeroes.
        litadd(0);
    }
}
