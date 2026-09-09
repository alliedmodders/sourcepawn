// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
//
#pragma once

#include <optional>

#include "types.h"

namespace sp {
namespace cc {

namespace ir {
class Value;
} // namespace ir

class BinaryExpr;
class Type;

struct ConstVal {
    ConstVal(Type* type, cell value) : type(type), i32(value) {}
    ConstVal(Type* type, bool value) : type(type), i32(value ? 1 : 0) {}
    ConstVal(Type* type, int64_t value) : type(type), i64(value) {}
    ConstVal(Type* type, float value) : type(type), f32(value) {}
    ConstVal(Type* type, double value) : type(type), f64(value) {}

    cell cell_bits() const {
        assert(!type->isInt64() && !type->isDouble());
        return i32;
    }
    cell get_cell() const {
        assert(!type->isWideType() && !type->isHeapItem());
        return i32;
    }
    cell get_i32() const {
        assert(!type->isFloat() && !type->isWideType() && !type->isHeapItem());
        return i32;
    }
    cell get_intptr() const {
        assert(type->isIntPtr());
        return i32;
    }
    float get_float() const {
        assert(type->isFloat());
        return f32;
    }
    double get_double() const {
        assert(type->isDouble());
        return f64;
    }
    int64_t get_int64() const {
        assert(type->isInt64());
        return i64;
    }

    Type* type;
    union {
        float f32;
        cell_t i32;
        double f64;
        int64_t i64;
    };
};

std::optional<ConstVal> TryFoldBinary(BinaryExpr* expr, ir::Value* left, ir::Value* right,
                                      Type* type);

std::optional<bool> FoldToConstantBool(ir::Value* cond);

std::optional<ConstVal> TryFoldCast(ir::Value* from, Type* to);
bool EvalConst(ir::Value* node, cell* value, Type** type);

} // namespace cc
} // namespace sp
