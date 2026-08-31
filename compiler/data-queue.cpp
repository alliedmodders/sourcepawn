// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
// Copyright (c) ITB CompuPhase, 1997-2006
//
#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h> /* for _MAX_PATH */
#include <string.h>

#include <amtl/am-raii.h>

#include "array-helpers.h"
#include "compile-context.h"
#include "compile-options.h"
#include "data-queue.h"
#include "errors.h"
#include "lexer.h"
#include "lexer-inl.h"
#include "sc.h"
#include "symbols.h"

namespace sp {
namespace cc {

DataQueue::DataQueue()
{
}

void
DataQueue::Add(cell value)
{
    union {
        cell value;
        char bytes[sizeof(cell)];
    } u;
    u.value = value;
    buffer_.append(u.bytes, sizeof(u.bytes));
}

void
DataQueue::Add(const char* text, size_t length)
{
    buffer_.append(text, length);
}

void
DataQueue::Add(tr::vector<cell>&& cells)
{
    if (cells.empty())
        return;

    buffer_.append(reinterpret_cast<const char*>(cells.data()), cells.size() * sizeof(cell));
    cells.clear();
}

void
DataQueue::AddZeroes(cell count)
{
    buffer_.resize(count * sizeof(cell), 0);
}

} // namespace cc
} // namespace sp
