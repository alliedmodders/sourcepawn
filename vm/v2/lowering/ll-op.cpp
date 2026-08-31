#include <assert.h>
#include <sp_vm_types.h>
#include "type-desc.h"
#include "v2/lowering/ll-op.h"

namespace sp::v2 {

static_assert(LL_SUB_F32 - LL_ADD_F32 == LL_SUB_I32 - LL_ADD_I32);
static_assert(LL_SUB_I64 - LL_ADD_I64 == LL_SUB_I32 - LL_ADD_I32);
static_assert(LL_SUB_F64 - LL_ADD_F64 == LL_SUB_I32 - LL_ADD_I32);
static_assert(LL_TEST_F32 - LL_NEG_F32 == LL_TEST_I32 - LL_NEG_I32);
static_assert(LL_TEST_I64 - LL_NEG_I64 == LL_TEST_I32 - LL_NEG_I32);
static_assert(LL_TEST_F64 - LL_NEG_F64 == LL_TEST_I32 - LL_NEG_I32);

} // namespace sp::v2
