// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2023-2026 AlliedModders LLC
//
#pragma once

#include "parse-node.h"
#include "scopes.h"

namespace sp {

inline void SymbolScope::ForEachSymbol(const std::function<void(Decl*)>& callback) {
    if (!symbols_)
        return;
    for (const auto& pair : *symbols_) {
        for (auto iter = pair.second; iter; iter = iter->next)
            callback(iter);
    }
}

}
