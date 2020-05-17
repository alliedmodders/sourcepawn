/*  Pawn compiler  - maintenance of various lists
 *
 *  o  Name list (aliases)
 *  o  Include path list
 *  o  Macro definitions (text substitutions)
 *  o  Documentation tags and automatic listings
 *  o  Debug strings
 *
 *  Copyright (c) ITB CompuPhase, 2001-2006
 *
 *  This software is provided "as-is", without any express or implied warranty.
 *  In no event will the authors be held liable for any damages arising from
 *  the use of this software.
 *
 *  Permission is granted to anyone to use this software for any purpose,
 *  including commercial applications, and to alter it and redistribute it
 *  freely, subject to the following restrictions:
 *
 *  1.  The origin of this software must not be misrepresented; you must not
 *      claim that you wrote the original software. If you use this software in
 *      a product, an acknowledgment in the product documentation would be
 *      appreciated but is not required.
 *  2.  Altered source versions must be plainly marked as such, and must not be
 *      misrepresented as being the original software.
 *  3.  This notice may not be removed or altered from any source distribution.
 *
 *  Version: $Id$
 */
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
#include "sp_symhash.h"

static bool sAliasTableInitialized;
static ke::HashMap<sp::CharsAndLength, ke::AString, KeywordTablePolicy> sAliases;

struct MacroTablePolicy {
    static bool matches(const ke::AString& a, const ke::AString& b) {
        return a == b;
    }
    static bool matches(const sp::CharsAndLength& a, const ke::AString& b) {
        if (a.length() != b.length())
            return false;
        return strncmp(a.str(), b.chars(), a.length()) == 0;
    }
    static uint32_t hash(const ke::AString& key) {
        return ke::HashCharSequence(key.chars(), key.length());
    }
    static uint32_t hash(const sp::CharsAndLength& key) {
        return ke::HashCharSequence(key.str(), key.length());
    }
};

struct MacroEntry {
    ke::AString first;
    ke::AString second;
    ke::AString documentation;
    bool deprecated;
};
static bool sMacroTableInitialized;
static ke::HashMap<ke::AString, MacroEntry, MacroTablePolicy> sMacros;

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
insert_alias(const char* name, const char* alias)
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

bool
lookup_alias(char* target, const char* name)
{
    if (!sAliasTableInitialized)
        return false;

    sp::CharsAndLength key(name, strlen(name));
    auto p = sAliases.find(key);
    if (!p.found())
        return false;
    ke::SafeStrcpy(target, sNAMEMAX + 1, p->value.chars());
    return true;
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
insert_path(char* path)
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

    ke::AString key(pattern, pattern_length);
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
        error(234, p->key.chars(), entry.documentation.chars());

    if (macro) {
        macro->first = entry.first.chars();
        macro->second = entry.second.chars();
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
insert_inputfile(char* string)
{
    if (sc_status != statFIRST)
        return insert_string(&inputfiles, string);
    return NULL;
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

/* ----- autolisting --------------------------------------------- */
static stringlist autolist;

stringlist*
insert_autolist(const char* string)
{
    return insert_string(&autolist, string);
}

char*
get_autolist(int index)
{
    return get_string(&autolist, index);
}

void
delete_autolisttable(void)
{
    delete_stringtable(&autolist);
    assert(autolist.next == NULL);
}

/* ----- debug information --------------------------------------- */

#define PRIdC "d"
#define PRIxC "x"

static stringlist dbgstrings;

stringlist*
insert_dbgfile(const char* filename)
{
    if (sc_status == statWRITE && (sc_debug & sSYMBOLIC) != 0) {
        char string[_MAX_PATH + 40];
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

stringlist*
insert_dbgsymbol(symbol* sym)
{
    if (sc_status == statWRITE && (sc_debug & sSYMBOLIC) != 0) {
        char string[2 * sNAMEMAX + 128];
        char symname[2 * sNAMEMAX + 16];

        funcdisplayname(symname, sym->name());
        /* address tag:name codestart codeend ident vclass [tag:dim ...] */
        assert(sym->ident != iFUNCTN);
        sprintf(string, "S:%" PRIxC " %x:%s %" PRIxC " %" PRIxC " %x %x %x", sym->addr(), sym->tag,
                symname, sym->codeaddr, code_idx, sym->ident, sym->vclass, (int)sym->is_const);
        if (sym->ident == iARRAY || sym->ident == iREFARRAY) {
#if !defined NDEBUG
            int count = sym->dim.array.level;
#endif
            symbol* sub;
            strcat(string, " [ ");
            for (sub = sym; sub != NULL; sub = sub->array_child()) {
                assert(sub->dim.array.level == count--);
                sprintf(string + strlen(string), "%x:%x ", sub->x.tags.index,
                        sub->dim.array.length);
            }
            strcat(string, "]");
        }

        if (curfunc) {
            if (!curfunc->function()->dbgstrs)
                curfunc->function()->dbgstrs = (stringlist*)calloc(1, sizeof(stringlist));
            return insert_string(curfunc->function()->dbgstrs, string);
        }
        return insert_string(&dbgstrings, string);
    }
    return NULL;
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
