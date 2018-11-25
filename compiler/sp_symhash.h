/* vim: set ts=4 sw=4 tw=99 et: */
#ifndef _INCLUDE_SPCOMP_SYMHASH_H_
#define _INCLUDE_SPCOMP_SYMHASH_H_

#include <stddef.h>
#include <string.h>
#include <amtl/am-hashtable.h>
#include "shared/string-pool.h"

struct KeywordTablePolicy {
  static bool matches(const sp::CharsAndLength& a, const sp::CharsAndLength& b) {
    if (a.length() != b.length())
      return false;
    return strncmp(a.str(), b.str(), a.length()) == 0;
  }
  static uint32_t hash(const sp::CharsAndLength& key) {
    return ke::HashCharSequence(key.str(), key.length());
  }
};

uint32_t NameHash(const char *str);

struct HashTable;

HashTable *NewHashTable();
void DestroyHashTable(HashTable *ht);
void AddToHashTable(HashTable *ht, symbol *sym);
void RemoveFromHashTable(HashTable *ht, symbol *sym);
symbol *FindInHashTable(HashTable *ht, const char *name, int fnumber);
symbol *FindTaggedInHashTable(HashTable *ht, const char *name, int fnumber,
                                      int *cmptag);

#endif /* _INCLUDE_SPCOMP_SYMHASH_H_ */

