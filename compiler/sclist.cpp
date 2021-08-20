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

static bool sAliasTableInitialized;
static ke::HashMap<sp::CharsAndLength, sp::Atom*, KeywordTablePolicy> sAliases;

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

/* ----- string list functions ----------------------------------- */
static stringlist*
insert_string(stringlist* root, const char* string)
{
    stringlist* cur;

    assert(string != NULL);
    if ((cur = (stringlist*)malloc(sizeof(stringlist))) == NULL)
        error(103); /* insufficient memory (fatal error) */
    if ((cur->line = strdup(string)) == NULL)
        error(103); /* insufficient memory (fatal error) */
    cur->next = NULL;
    if (root->tail)
        root->tail->next = cur;
    else
        root->next = cur;
    root->tail = cur;
    return cur;
}

static char*
get_string(stringlist* root, int index)
{
    stringlist* cur;

    assert(root != NULL);
    cur = root->next;
    while (cur != NULL && index-- > 0)
        cur = cur->next;
    if (cur != NULL) {
        assert(cur->line != NULL);
        return cur->line;
    }
    return NULL;
}

void
delete_stringtable(stringlist* root)
{
    stringlist *cur, *next;

    assert(root != NULL);
    cur = root->next;
    while (cur != NULL) {
        next = cur->next;
        assert(cur->line != NULL);
        free(cur->line);
        free(cur);
        cur = next;
    }
    memset(root, 0, sizeof(stringlist));
}

void
insert_alias(const char* name, sp::Atom* alias)
{
    if (!sAliasTableInitialized) {
        sAliases.init(128);
        sAliasTableInitialized = true;
    }

    sp::CharsAndLength key(name, strlen(name));
    auto p = sAliases.findForAdd(key);
    if (p.found())
        p->value = alias;
    else
        sAliases.add(p, key, alias);
}

sp::Atom*
lookup_alias(const char* name)
{
    if (!sAliasTableInitialized)
        return nullptr;

    sp::CharsAndLength key(name, strlen(name));
    auto p = sAliases.find(key);
    if (!p.found())
        return nullptr;
    return p->value;
}

void
delete_aliastable(void)
{
    if (sAliasTableInitialized)
        sAliases.clear();
}

/* ----- include paths list -------------------------------------- */
static stringlist includepaths; /* directory list for include files */

stringlist*
insert_path(const char* path)
{
    return insert_string(&includepaths, path);
}

char*
get_path(int index)
{
    return get_string(&includepaths, index);
}

void
delete_pathtable(void)
{
    delete_stringtable(&includepaths);
    assert(includepaths.next == NULL);
}

/* ----- substitutions (macros) -------------------------------------- */

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
        if (sc_status == statWRITE)
            macro.documentation = std::move(pc_deprecate);
        else
            pc_deprecate = "";
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

/* ----- input file list (explicit files) ------------------------ */
static stringlist sourcefiles;

stringlist*
insert_sourcefile(char* string)
{
    return insert_string(&sourcefiles, string);
}

char*
get_sourcefile(int index)
{
    return get_string(&sourcefiles, index);
}

void
delete_sourcefiletable(void)
{
    delete_stringtable(&sourcefiles);
    assert(sourcefiles.next == NULL);
}

/* ----- parsed file list (explicit + included files) ------------ */
static stringlist inputfiles;

stringlist*
insert_inputfile(const char* string)
{
    return insert_string(&inputfiles, string);
}

char*
get_inputfile(int index)
{
    return get_string(&inputfiles, index);
}

void
delete_inputfiletable(void)
{
    delete_stringtable(&inputfiles);
    assert(inputfiles.next == NULL);
}

/* ----- debug information --------------------------------------- */

#define PRIdC "d"
#define PRIxC "x"

static stringlist dbgstrings;

stringlist*
insert_dbgfile(const char* filename)
{
    if (sc_status == statWRITE && (sc_debug & sSYMBOLIC) != 0) {
        char string[PATH_MAX + 40];
        assert(filename != NULL);
        assert(strlen(filename) + 40 < sizeof string);
        sprintf(string, "F:%" PRIxC " %s", code_idx, filename);
        return insert_string(&dbgstrings, string);
    }
    return NULL;
}

stringlist*
insert_dbgline(int linenr)
{
    if (sc_status == statWRITE && (sc_debug & sSYMBOLIC) != 0) {
        char string[40];
        if (linenr > 0)
            linenr--; /* line numbers are zero-based in the debug information */
        sprintf(string, "L:%" PRIxC " %x", code_idx, linenr);
        return insert_string(&dbgstrings, string);
    }
    return NULL;
}

void
insert_dbgsymbol(symbol* sym)
{
    if (sc_status == statWRITE && (sc_debug & sSYMBOLIC) != 0) {
	auto symname = funcdisplayname(sym->name());

        /* address tag:name codestart codeend ident vclass [tag:dim ...] */
        assert(sym->ident != iFUNCTN);
        auto string = ke::StringPrintf("S:%" PRIxC " %x:%s %" PRIxC " %" PRIxC " %x %x %x",
                                       sym->addr(), sym->tag, symname.c_str(), sym->codeaddr,
                                       code_idx, sym->ident, sym->vclass, (int)sym->is_const);
        if (sym->ident == iARRAY || sym->ident == iREFARRAY) {
#if !defined NDEBUG
            int count = sym->dim.array.level;
#endif
            symbol* sub;
            string += " [ ";
            for (sub = sym; sub != NULL; sub = sub->array_child()) {
                assert(sub->dim.array.level == count--);
                string += ke::StringPrintf("%x:%x ", sub->x.tags.index, sub->dim.array.length);
            }
            string += "]";
        }

        if (curfunc) {
            auto data = curfunc->function();
            data->dbgstrs.emplace_back(string.c_str(), string.size());
        } else {
            insert_string(&dbgstrings, string.c_str());
        }
    }
}

stringlist*
get_dbgstrings()
{
    return &dbgstrings;
}

char*
get_dbgstring(int index)
{
    return get_string(&dbgstrings, index);
}

void
delete_dbgstringtable(void)
{
    delete_stringtable(&dbgstrings);
    assert(dbgstrings.next == NULL);
}
