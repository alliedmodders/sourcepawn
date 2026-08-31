// vim: set sts=2 ts=8 sw=2 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2006-2026 AlliedModders LLC
//
#include "api.h"
#include <assert.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include "environment.h"

#include "legacy/plugin-runtime.h"
#include "v2/runtime.h"
using namespace sp;
using namespace SourcePawn;

size_t
sp::UTIL_FormatVA(char* buffer, size_t maxlength, const char* fmt, va_list ap) {
    size_t len = vsnprintf(buffer, maxlength, fmt, ap);

    if (len >= maxlength) {
        buffer[maxlength - 1] = '\0';
        return maxlength - 1;
    }
    return len;
}

size_t
sp::UTIL_Format(char* buffer, size_t maxlength, const char* fmt, ...) {
    va_list ap;

    va_start(ap, fmt);
    size_t len = UTIL_FormatVA(buffer, maxlength, fmt, ap);
    va_end(ap);

    return len;
}
