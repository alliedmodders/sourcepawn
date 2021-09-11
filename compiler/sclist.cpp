//  vim: set ts=8 sts=4 sw=4 tw=99 et:
//
//  Copyright (c) ITB CompuPhase, 2001-2006
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
#include <assert.h>
#include <inttypes.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>

#include <utility>

#include "sclist.h"
#include <amtl/am-hashmap.h>
#include <amtl/am-string.h>
#include "errors.h"
#include "sc.h"
#include "scvars.h"
#include "symbols.h"

struct MacroTablePolicy {
    static bool matches(const std::string& a, const std::string& b) {
        return a == b;
    }
    static bool matches(const sp::CharsAndLength& a, const std::string& b) {
        if (a.length() != b.length())
            return false;
        return strncmp(a.str(), b.c_str(), a.length()) == 0;
    }
    static uint32_t hash(const std::string& key) {
        return ke::HashCharSequence(key.c_str(), key.length());
    }
    static uint32_t hash(const sp::CharsAndLength& key) {
        return ke::HashCharSequence(key.str(), key.length());
    }
};

struct MacroEntry {
    std::string first;
    std::string second;
    std::string documentation;
    bool deprecated;
};
static bool sMacroTableInitialized;
static ke::HashMap<std::string, MacroEntry, MacroTablePolicy> sMacros;

void
insert_subst(const char* pattern, size_t pattern_length, const char* substitution)
{
    if (!sMacroTableInitialized) {
        sMacros.init(1024);
        sMacroTableInitialized = true;
    }

    MacroEntry macro;
    macro.first = pattern;
    macro.second = substitution;
    macro.deprecated = false;
    if (pc_deprecate.length() > 0) {
        macro.deprecated = true;
        macro.documentation = std::move(pc_deprecate);
    }

    std::string key(pattern, pattern_length);
    auto p = sMacros.findForAdd(key);
    if (p.found())
        p->value = macro;
    else
        sMacros.add(p, std::move(key), macro);
}

bool
find_subst(const char* name, size_t length, macro_t* macro)
{
    sp::CharsAndLength key(name, length);
    auto p = sMacros.find(key);
    if (!p.found())
        return false;

    MacroEntry& entry = p->value;
    if (entry.deprecated)
        error(234, p->key.c_str(), entry.documentation.c_str());

    if (macro) {
        macro->first = entry.first.c_str();
        macro->second = entry.second.c_str();
    }
    return true;
}

bool
delete_subst(const char* name, size_t length)
{
    sp::CharsAndLength key(name, length);
    auto p = sMacros.find(key);
    if (!p.found())
        return false;

    sMacros.remove(p);
    return true;
}

void
delete_substtable(void)
{
    sMacros.clear();
}
