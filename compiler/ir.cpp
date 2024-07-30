// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
//  Copyright (c) AlliedModders 2024
//
//  This software is provided "as-is", without any express or implied warranty.
//  In no event will the authors be held liable for any damages arising from
//  the use of this software.
//
//  Permission is granted to anyone to use this software for any purpose,
//  including commercial applications, and to alter it and redistribute it
//  freely, subject to the following restrictions:
//
//  1.  The origin of this software must not be misrepresented; you must not
//      claim that you wrote the original software. If you use this software in
//      a product, an acknowledgment in the product documentation would be
//      appreciated but is not required.
//  2.  Altered source versions must be plainly marked as such, and must not be
//      misrepresented as being the original software.
//  3.  This notice may not be removed or altered from any source distribution.

#include "ir.h"

namespace sp {
namespace cc {
namespace ir {

bool Value::HasSideEffects() {
    switch (kind_) {
        case IrKind::Store:
        case IrKind::StoreWithTemp:
        case IrKind::IncDec:
        case IrKind::IncDecUserOp:
        case IrKind::CallOp:
            return true;
        case IrKind::PropertyRef: {
            auto ir = to<PropertyRef>();
            return ir->val()->HasSideEffects();
        }
        case IrKind::StackRef:
            return false;
        case IrKind::TempValueRef: {
            auto ir = to<TempValueRef>();
            return ir->val()->HasSideEffects();
        }
        case IrKind::FieldRef: {
            auto ir = to<FieldRef>();
            return ir->base()->HasSideEffects();
        }
        case IrKind::IndexOp: {
            auto ir = to<IndexOp>();
            return ir->base()->HasSideEffects() || ir->index()->HasSideEffects();
        }
        case IrKind::Load: {
            auto ir = to<Load>();
            return ir->lval()->HasSideEffects();
        }
        case IrKind::TernaryOp: {
            auto ir = to<TernaryOp>();
            return ir->select()->HasSideEffects() ||
                   ir->on_true()->HasSideEffects() ||
                   ir->on_false()->HasSideEffects();
        }
        case IrKind::BinaryOp: {
            auto ir = to<BinaryOp>();
            return ir->left()->HasSideEffects() ||
                   ir->right()->HasSideEffects();
        }
        case IrKind::CommaOp: {
            auto ir = to<CommaOp>();
            for (const auto& val : ir->values()) {
                if (val->HasSideEffects())
                    return true;
            }
            return false;
        }
        case IrKind::ArrayInitializer: {
            auto ir = to<ArrayInitializer>();
            for (const auto& val : ir->values()) {
                if (val->HasSideEffects())
                    return true;
            }
            return false;
        }
        case IrKind::UnaryOp: {
            auto ir = to<UnaryOp>();
            return ir->val()->HasSideEffects();
        }
        case IrKind::VariableRef:
        case IrKind::ConstVal:
            return false;
        default:
            assert(false);
            return false;
    }
}

void Function::AddReferenceTo(Function* other) {
    if (!refers_to_) {
        auto& cc = CompileContext::get();
        refers_to_ = cc.allocator().alloc<PoolForwardList<Function*>>();
    }
    for (Function* decl : *refers_to_) {
        if (decl == other)
            return;
    }
    refers_to_->emplace_front(other);
}

bool Lvalue::HasComplexAddressCalculation() {
    assert(IsAddressable());

    switch (kind()) {
        case IrKind::VariableRef:
        case IrKind::StackRef:
            return false;
        case IrKind::IndexOp:
        case IrKind::FieldRef:
            // FieldRef technically isn't complex in v1 ops, but it might be someday, if we
            // introduce an equivalent to idxaddr for fields.
            return true;
        default:
            assert(false);
            return false;
    }
}

void PropertyRef::BindGetter(ir::Function* getter) {
    assert(!getter_ || getter_ == getter);
    getter_ = getter;
}

void PropertyRef::BindSetter(ir::Function* setter) {
    assert(!setter_ || setter_ == setter);
    setter_ = setter;
}

bool ArrayInitializer::HasEllipses() const {
    if (!expr())
        return false;
    if (auto init = expr()->as<ArrayExpr>())
        return init->ellipses();
    return false;
}

} // namespace ir
} // namespace cc
} // namespace sp
