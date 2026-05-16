// vim: set sts=2 ts=8 sw=2 tw=99 et:
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
#include "api.h"
#include <assert.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include "environment.h"

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
