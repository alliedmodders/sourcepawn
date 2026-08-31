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
#pragma once

#include "errors.h"
#include "parse-node.h"
#include "types.h"

namespace sp {
namespace cc {

class TypeChecker {
  public:
    enum Context {
        Assignment,
        Argument,
        Return,
        Generic,
    };

    enum Flags {
        None = 0,
        Commutative = 0x1,
        FuncArg = 0x2,
        EnumAssign = 0x4,
        AllowCoerce = 0x8,
        Ternary = 0x10,
    };

    TypeChecker(ParseNode* node, QualType formal, QualType actual, Context why,
                int flags = None)
      : TypeChecker(node->pos(), formal, actual, why, flags)
    {}
    TypeChecker(ParseNode* node, Type* formal, Type* actual, Context why,
                int flags = None)
      : TypeChecker(node->pos(), QualType(formal), QualType(actual), why, flags)
    {}
    TypeChecker(const token_pos_t& pos, Type* formal, Type* actual, Context why,
                int flags = None)
      : TypeChecker(pos, QualType(formal), QualType(actual), why, flags)
    {}
    TypeChecker(const token_pos_t& pos, QualType formal, QualType actual, Context why,
                int flags = None);

    bool Check();
    bool Coerce();

    static bool DoCoerce(Type* formal, Expr* actual);

  private:
    bool CheckImpl();
    bool CheckValueType(Type* formal, Type* actual);
    bool CheckArrays(ArrayType* formal, ArrayType* actual);
    bool CheckFunction();
    bool CheckFunctionSignature(FunctionType* formal, FunctionType* actual);
    bool DiagnoseFailure();
    bool DiagnoseFunctionFailure();

  private:
    const token_pos_t& pos_;
    QualType formal_;
    QualType actual_;
    Context why_;
    int flags_;
    AutoDeferReports defer_;
};

} // namespace cc
} // namespace sp
