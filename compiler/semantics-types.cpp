/* vim: set sts=4 ts=8 sw=4 tw=99 et: */
//
//  Copyright (c) AlliedModders LLC, 2024
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

#include "semantics.h"

#include <amtl/am-raii.h>
#include "errors.h"
#include "sctracker.h"
#include "semantics-inl.h"

namespace sp {
namespace cc {

static const std::vector<std::pair<BuiltinType, BuiltinType>> NumericOperands{
    {BuiltinType::Int, BuiltinType::Int},
    {BuiltinType::Int64, BuiltinType::Int64},
    {BuiltinType::Float, BuiltinType::Float}
};
static const std::vector<std::pair<BuiltinType, BuiltinType>> BitwiseOperands{
    {BuiltinType::Int, BuiltinType::Int},
    {BuiltinType::Int64, BuiltinType::Int64}
};

enum class CkCompare {
    Worse,
    Same,
    Better,
};

static inline CkCompare CompareConversion(ConversionKind candidate, ConversionKind other,
                                          Type* candidate_to, Type* other_to, CvtContext why)
{
    if (static_cast<uint32_t>(candidate) < static_cast<uint32_t>(other))
        return CkCompare::Worse;
    if (static_cast<uint32_t>(candidate) > static_cast<uint32_t>(other))
        return CkCompare::Better;

    // Find which conversion is closer to "from".
    //
    // Eg, say we have two options: int64, or float, and our source is int. We
    // want to check int64 -> float and float -> int64. Since int64 -> float works,
    // and float -> int64 does not, we can conclude that "int" is closer to "int64".
    auto candidate_ck = FindConversion(candidate_to, other_to, why);
    auto other_ck = FindConversion(other_to, candidate_to, why);
    if (HasImplicitConversion(candidate_ck) && !HasImplicitConversion(other_ck))
        return CkCompare::Better;
    if (!HasImplicitConversion(candidate_ck) && HasImplicitConversion(other_ck))
        return CkCompare::Worse;
    return CkCompare::Same;
}

auto Semantics::FindBinaryOperator(int token, Type* left_type, Type* right_type)
    -> std::optional<BinaryOperator>
{
    if (token == tlEQ || token == tlNE) {
        // Equality is handled totally separately.
        return FindEqualityOperator(left_type, right_type);
    }

    const std::vector<std::pair<BuiltinType, BuiltinType>>* operand_list;
    if (IsBitwise(token))
        operand_list = &BitwiseOperands;
    else
        operand_list = &NumericOperands;

    auto& cc = CompileContext::get();

    BinaryOperator out;

    // Handle legacy cases specially, stuff like enum + enum.
    if ((left_type->coercesFromInt() && !left_type->isInt()) ||
        (right_type->coercesFromInt() && !right_type->isInt()))
    {
        // Use the left-hand type as canonical.
        auto ck = FindConversion(right_type, left_type, CvtContext::Operator);
        if (HasImplicitConversion(ck)) {
            out.left = {ConversionKind::None, left_type};
            out.right = {ck, left_type};
            return {out};
        }
    }

    // We use an algorithm similar to C#: when comparing overload A to B, B is rejected
    // if any conversion is "worse" than needed for A. Then, for B to be chosen over A,
    // it must have at least one conversion that is "better" than needed for A.
    //
    // The ordering of priority for conversions is in ConversionKind. The most ideal case
    // is that no conversion is needed.
    //
    // If two conversions are tied, then we swap the conversion direction. Eg, int -> int64
    // and int -> float are both Numeric conversions. However, int64 -> int is 
    unsigned int nmatches = 0;
    for (const auto& [left, right] : *operand_list) {
        auto want_left_type = cc.types()->GetBuiltin(left);
        auto left_ck = FindConversion(left_type, want_left_type, CvtContext::Operator);
        if (!HasImplicitConversion(left_ck))
            continue;

        auto want_right_type = cc.types()->GetBuiltin(right);
        auto right_ck = FindConversion(right_type, want_right_type, CvtContext::Operator);
        if (!HasImplicitConversion(right_ck))
            continue;

        // Note that we pass potentially null pointers to CompareConversion.
        // This is ok, since they are only null if no best ck exists. In that
        // case, we have a better ck by default, and the pointer won't be used.
        auto left_cmp = CompareConversion(left_ck, out.left.ck, want_left_type, out.left.type,
                                          CvtContext::Operator);
        auto right_cmp = CompareConversion(right_ck, out.right.ck, want_right_type, out.right.type,
                                          CvtContext::Operator);
        if (left_cmp == CkCompare::Worse || right_cmp == CkCompare::Worse)
            continue;
        if (left_cmp == CkCompare::Better || right_cmp == CkCompare::Better) {
            nmatches = 0;
            out.left = {left_ck, want_left_type};
            out.right = {right_ck, want_right_type};
        }
        nmatches++;
    }

    // Ambiguous matches should be impossible right now.
    assert(nmatches <= 1);

    if (nmatches == 0)
        return {};

    assert(out.left.type == out.right.type);
    return {out};
}

auto Semantics::FindEqualityOperator(Type* left_type, Type* right_type)
    -> std::optional<BinaryOperator>
{
    auto rtl_ck = FindConversion(right_type, left_type, CvtContext::Operator);
    auto ltr_ck = FindConversion(left_type, right_type, CvtContext::Operator);

    if (!HasImplicitConversion(ltr_ck) && !HasImplicitConversion(rtl_ck))
        return {};

    BinaryOperator out;
    out.left = {ConversionKind::None, left_type};
    out.right = {ConversionKind::None, right_type};

    // If the right-to-left conversion is better than the left-to-right, we rewrite
    // the right-hand side.
    //
    // Eg, for "float == int", ltr = illegal, rtl = numeric.
    //
    // Therefore, we we choose to convert the right-hand side.
    if (static_cast<uint32_t>(rtl_ck) >= static_cast<uint32_t>(ltr_ck))
        out.right = {rtl_ck, left_type};
    else
        out.left = {ltr_ck, right_type};
    return out;
}

template <typename T>
static void ReportConversionDiagnosticImpl(T location, QualType formal, QualType actual) {
    auto diag_ck = FindConversion(*formal, *actual, CvtContext::Assignment);
    if (diag_ck == ConversionKind::Numeric) {
        report(location, 462) << actual << formal;
    } else if (actual->isVoid()) {
        report(location, 466);
    } else if (actual->isNull()) {
        report(location, 148) << formal;
    } else if (formal->isArray() && !formal->isFixedArray() && actual->isFlatArray()) {
        report(location, 473) << actual << formal;
    } else {
        report(location, 450) << actual << formal;
    }
}

void Semantics::ReportConversionDiagnostic(const token_pos_t& pos, QualType formal, QualType actual) {
    ReportConversionDiagnosticImpl(pos, formal, actual);
}

void Semantics::ReportConversionDiagnostic(ParseNode* node, QualType formal, QualType actual) {
    ReportConversionDiagnosticImpl(node, formal, actual);
}

Expr* Semantics::TryConversion(Expr* expr, QualType formal, CvtContext why) {
    auto ck = FindConversion(expr->val().type(), *formal, why);
    if (HasImplicitConversion(ck)) {
        if (!IsNopConversion(ck))
            return BuildConversion(expr, ck, *formal);
        if (ck == ConversionKind::TagMismatch)
            report(expr->pos(), 213) << formal << expr->val().type();
        return expr;
    }
    ReportConversionDiagnostic(expr->pos(), formal, expr->val().type());
    return nullptr;
}

bool Semantics::CheckCoercion(const token_pos_t& pos, QualType formal, QualType actual,
                              CvtContext why)
{
    auto ck = FindConversion(*actual, *formal, why);
    if (!HasImplicitConversion(ck)) {
        ReportConversionDiagnostic(pos, formal, actual);
        return false;
    }
    if (ck == ConversionKind::TagMismatch)
        report(pos, 213) << formal << actual;
    return true;
}

bool Semantics::CheckCoercion(ParseNode* node, QualType formal, QualType actual,
                              CvtContext why)
{
    return CheckCoercion(node->pos(), formal, actual, why);
}

} // namespace cc
} // namespace sp
