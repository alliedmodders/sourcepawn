// vim: set ts=8 sts=4 sw=4 tw=99 et:
/*  Pawn compiler - code generation (unoptimized "assembler" code)
 *
 *  Copyright (c) ITB CompuPhase, 1997-2006
 *
 *  This software is provided "as-is", without any express or implied warranty.
 *  In no event will the authors be held liable for any damages arising from
 *  the use of this software.
 *
 *  Permission is granted to anyone to use this software for any purpose,
 *  including commercial applications, and to alter it and redistribute it
 *  freely, subject to the following restrictions:
 *
 *  1.  The origin of this software must not be misrepresented; you must not
 *      claim that you wrote the original software. If you use this software in
 *      a product, an acknowledgment in the product documentation would be
 *      appreciated but is not required.
 *  2.  Altered source versions must be plainly marked as such, and must not be
 *      misrepresented as being the original software.
 *  3.  This notice may not be removed or altered from any source distribution.
 *
 *  Version: $Id$
 */
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
#include "sctracker.h"
#include "symbols.h"

namespace sp {
namespace cc {

DataQueue::DataQueue()
{
}

void
DataQueue::Add(cell value)
{
    buffer_.emplace_back(value);
}

void
DataQueue::Add(const char* text, size_t length)
{
    StringToCells(text, length, [this](cell value) -> void {
        buffer_.emplace_back(value);
    });
}

void
DataQueue::Add(tr::vector<cell>&& cells)
{
    if (cells.empty())
        return;

    for (const auto& value : cells)
        buffer_.emplace_back(value);
    cells.clear();
}

void
DataQueue::AddZeroes(cell count)
{
    buffer_.resize(buffer_.size() + count, 0);
}

} // namespace cc
} // namespace sp
