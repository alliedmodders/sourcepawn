// vim: set ts=8 sts=2 sw=2 tw=99 et:
#include "sp_symhash.h"
#include <amtl/am-hashtable.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "sc.h"

struct NameAndScope {
    sp::Atom* name;
    int fnumber;

    NameAndScope(const char* name, int fnumber)
     : name(gAtoms.add(name)),
       fnumber(fnumber)
    {}
};

struct SymbolHashPolicy {
    typedef symbol* Payload;

    // Everything with the same name has the same hash, because the compiler
    // wants to know two names that have the same tag for some reason. Even
    // so, we can't be that accurate, since we might match the right symbol
    // very early.
    static uint32_t hash(const NameAndScope& key) {
        return ke::HashPointer(key.name);
    }
    static uint32_t hash(const symbol* s) {
        return ke::HashPointer(s->nameAtom());
    }

    static bool matches(const NameAndScope& key, symbol* sym) {
        if (sym->parent() && sym->ident != iCONSTEXPR)
            return false;
        if (sym->is_static && sym->fnumber != key.fnumber)
            return false;
        if (key.name != sym->nameAtom())
            return false;
        return true;
    }
    static bool matches(const symbol* key, symbol* sym) {
        return key == sym;
    }
};

struct HashTable : public ke::HashTable<SymbolHashPolicy> {};

uint32_t
NameHash(const char* str)
{
    return ke::HashCharSequence(str, strlen(str));
}

HashTable*
NewHashTable()
{
    HashTable* ht = new HashTable();
    if (!ht->init()) {
        delete ht;
        return nullptr;
    }
    return ht;
}

void
DestroyHashTable(HashTable* ht)
{
    delete ht;
}

symbol*
FindInHashTable(HashTable* ht, const char* name, int fnumber)
{
    NameAndScope nas(name, fnumber);
    HashTable::Result r = ht->find(nas);
    if (!r.found())
        return nullptr;
    return *r;
}

void
AddToHashTable(HashTable* ht, symbol* sym)
{
    HashTable::Insert i = ht->findForAdd(sym);
    assert(!i.found());
    ht->add(i, sym);
}

void
RemoveFromHashTable(HashTable* ht, symbol* sym)
{
    HashTable::Result r = ht->find(sym);
    assert(r.found());
    ht->remove(r);
}
