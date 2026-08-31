// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
// Copyright (c) ITB CompuPhase, 1997-2006
//
#pragma once

#include <string>

#include "source-file.h"

void UnicodeCodepointToUtf8(uint32_t codepoint, std::string* out);

static inline bool IsSpace(char c) {
    // msvcrt doesn't like negative values.
    return c > 0 && ::isspace(c);
}

static inline bool IsNewline(char c) {
    return c == '\r' || c == '\n';
}

static inline bool IsDigit(char c) {
    return c >= '0' && c <= '9';
}
