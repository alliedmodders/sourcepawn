// vim: set sts=4 sw=4 tw=99 ts=8 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
// Copyright (c) ITB CompuPhase, 2004-2006
//
#include "sci18n.h"

#include <assert.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define _SILENCE_CXX17_CODECVT_HEADER_DEPRECATION_WARNING

#if defined(__clang__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdeprecated-declarations"
#endif

#include <codecvt>
#include <locale>

#include <amtl/am-bits.h>
#include "errors.h"
#include "sc.h"

void UnicodeCodepointToUtf8(uint32_t codepoint, std::string* out) {
    if (codepoint <= 0x7F) {
        *out += static_cast<char>(codepoint);
    } else if (codepoint <= 0x7FF) {
        *out += static_cast<char>(0xC0 | ((codepoint >> 6) & 0x1F));
        *out += static_cast<char>(0x80 | (codepoint & 0x3F));
    } else if (codepoint <= 0xFFFF) {
        *out += static_cast<char>(0xE0 | ((codepoint >> 12) & 0x0F));
        *out += static_cast<char>(0x80 | ((codepoint >> 6) & 0x3F));
        *out += static_cast<char>(0x80 | (codepoint & 0x3F));
    } else if (codepoint <= 0x10FFFF) {
        *out += static_cast<char>(0xF0 | ((codepoint >> 18) & 0x07));
        *out += static_cast<char>(0x80 | ((codepoint >> 12) & 0x3F));
        *out += static_cast<char>(0x80 | ((codepoint >> 6) & 0x3F));
        *out += static_cast<char>(0x80 | (codepoint & 0x3F));
    }
}
