// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
//  Copyright (c) AlliedModders LLC 2021
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

#include "scopes.h"

#include "parse-node.h"

namespace sp {
namespace cc {

void SymbolScope::Add(Decl* decl) {
    if (!symbols_) {
        auto& cc = CompileContext::get();
        symbols_ = cc.NewSymbolMap();
    }

    assert(symbols_->find(decl->name()) == symbols_->end());
    symbols_->emplace(decl->name(), decl);
}

void SymbolScope::AddChain(Decl* decl) {
    if (!symbols_) {
        auto& cc = CompileContext::get();
        symbols_ = cc.NewSymbolMap();
    }

    auto iter = symbols_->find(decl->name());
    if (iter == symbols_->end()) {
        symbols_->emplace(decl->name(), decl);
    } else {
        decl->next = iter->second;
        iter->second = decl;
    }
}

} // namespace cc
} // namespace sp
