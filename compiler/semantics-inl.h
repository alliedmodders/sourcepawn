// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2021-2026 AlliedModders LLC
// Copyright (c) ITB CompuPhase, 1997-2005
//
#pragma once

#include "tokens.h"

namespace sp {
namespace cc {

static inline int NormalizeBinaryToken(int token) {
    switch (token) {
        case tlEQ:
        case tlNE:
        case tlLE:
        case tlGE:
        case '<':
        case '>':
        case '|':
        case '^':
        case '&':
        case '*':
        case '/':
        case '%':
        case '+':
        case '-':
        case tSHL:
        case tSHR:
        case tSHRU:
            return token;
        case taMULT:
            return '*';
        case taDIV:
            return '/';
        case taMOD:
            return '%';
        case taADD:
            return '+';
        case taSUB:
            return '-';
        case taSHL:
            return tSHL;
        case taSHR:
            return tSHR;
        case taSHRU:
            return tSHRU;
        case taAND:
            return '&';
        case taXOR:
            return '^';
        case taOR:
            return '|';
        case '=':
        case tlOR:
        case tlAND:
            return 0;
        default:
            assert(false);
            return 0;
    }
}

static inline bool IsBitwise(int token) {
    switch (token) {
        case '|':
        case '^':
        case '&':
        case tSHL:
        case tSHR:
        case tSHRU:
            return true;
        default:
            return false;
    }
}

static inline bool IsCompare(int token) {
    switch (token) {
        case tlEQ:
        case tlNE:
        case tlLE:
        case tlGE:
        case '>':
        case '<':
            return true;
        default:
            return false;
    }
}

static inline bool IsArithmetic(int token) {
    switch (token) {
        case tlEQ:
        case tlNE:
        case tlLE:
        case tlGE:
        case '<':
        case '>':
        case '*':
        case '/':
        case '%':
        case '+':
        case '-':
        case taMULT:
        case taDIV:
        case taMOD:
        case taADD:
        case taSUB:
            return true;
        default:
            return false;
    }
}

} // namespace cc
} // namespace sp
