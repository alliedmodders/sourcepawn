// vim: set ts=8 sts=2 sw=2 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
//
#include "v2/runtime-helpers.h"

#include <math.h>

#include "environment.h"
#include "v2/runtime.h"

namespace sp::v2 {

using namespace SourcePawn;

int
Int64Div(int64_t* pri, int64_t* alt, int64_t* pri_dest) {
    if (*pri == 0)
        return SP_ERROR_DIVIDE_BY_ZERO;

    // -INT_MIN / -1 is an overflow.
    if (*pri == -1 && *alt == std::numeric_limits<int64_t>::min())
        return SP_ERROR_INTEGER_OVERFLOW;

    *pri_dest = *alt / *pri;
    return SP_ERROR_NONE;
}

int
Int64Mod(int64_t* pri, int64_t* alt, int64_t* pri_dest) {
    if (*pri == 0)
        return SP_ERROR_DIVIDE_BY_ZERO;

    // -INT_MIN / -1 is an overflow.
    if (*pri == -1 && *alt == std::numeric_limits<int64_t>::min())
        return SP_ERROR_INTEGER_OVERFLOW;

    *pri_dest = *alt % *pri;
    return SP_ERROR_NONE;
}

double DoubleMod(double left, double right) {
    return ::fmod(left, right);
}

void ReportOutOfBoundsError(cell_t index, cell_t bounds) {
    if (bounds == INT_MAX) {
        // This is an internal protection against negative indices on arrays with
        // unknown size.
        Environment::get()->ReportErrorFmt(SP_ERROR_ARRAY_BOUNDS,
                                           "Array index out-of-bounds (index %d)", index);
    } else {
        Environment::get()->ReportErrorFmt(SP_ERROR_ARRAY_BOUNDS,
                                           "Array index out-of-bounds (index %d, limit %d)", index,
                                           size_t(bounds));
    }
}

cell_t NativeInvokeThunk(Runtime* ctx, NativeEntry* entry, const cell_t* params) {
    if (entry->status != SP_NATIVE_BOUND)
        return ctx->ThrowNativeErrorEx(SP_ERROR_INVALID_NATIVE, "Native is not bound");

    if (entry->legacy_fn)
        return entry->legacy_fn(ctx, params);
    return entry->callback->Invoke(ctx, params);
}

} // namespace sp::v2
