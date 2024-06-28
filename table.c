#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"

#define TABLE_MAX_LOAD 0.75

void initTable(Table* table) {
    table->count = 0;
    table->capacity = 0;
    table->entries = NULL;
}

void freeTable(Table* table) {
    FREE_ARRAY(Entry, table->entries, table->capacity);
    initTable(table);
}

static Entry* findEntry(Entry* entries, int capacity, ObjString* key) {
    uint32_t index = key->hash%capacity;
    Entry* tombstone = NULL;
    // linear probing & collision handling
    for (;;) {
        Entry* entry = &entries[index];

        // In this function, we are either adding a new key/value or replacing
        // an exiting one, so:
        // if key is NULL:
        //   if it's a tombstone, we keep a reference to it
        //   if it's not a tombstone -> we are done with linear probing
        //     so we can add the new key/value inside either:
        //       - the first tombstone we found
        //       - the current bucket (= no tombstone were found before)
        if (entry->key == NULL) {
            if (IS_NIL(entry->value)) {
                // empty entry
                return tombstone != NULL ? tombstone : entry;
            } else {
                // we found a tombstone
                if (tombstone == NULL) tombstone = entry;
            }
        } else if (entry->key == key) {
            // found the key
            return entry;
        }
        // else the bucket is occupied with an entry with a != key

        index = (index + 1) % capacity;
    }
}

/**
* Compared to a simple dynamic array, we can't just allocate a new array and
* copy the values, because to choose a bucket for an entry, we take its hash
* module the array size.
* The simplest solution is to recalculate the buckers for each existing
* entries in the hash table.
*
* We make sure to not insert tombstones.
* (and update the count)
*/
static void adjustCapacity(Table* table, int capacity) {
    Entry* entries = ALLOCATE(Entry, capacity);
    for (int i = 0; i < capacity; i++) {
        entries[i].key = NULL;
        entries[i].value = NIL_VAL;
    }

    table->count = 0;
    for (int i = 0; i < table->capacity; i++) {
        Entry* entry = &table->entries[i];
        if (entry->key == NULL) continue;

        Entry* dest = findEntry(entries, capacity, entry->key);
        dest->key = entry->key;
        dest->value = entry->value;
        table->count++;
    }

    FREE_ARRAY(Entry, table->entries, table->capacity);
    table->entries = entries;
    table->capacity = capacity;

}

bool tableSet(Table* table, ObjString* key, Value value) {

    // we grow table when array becomes TABLE_MAX_LOAD% full
    // that's how we handle the load factor.
    if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
        int capacity = GROW_CAPACITY(table->capacity);
        adjustCapacity(table, capacity);
    }

    Entry* entry = findEntry(table->entries, table->capacity, key);
    bool isNewKey = entry->key == NULL;
    if (isNewKey && IS_NIL(entry->value)) table->count++;

    entry->key = key;
    entry->value = value;
    return isNewKey;
}

/**
* won't need this until we support method inheritance
* To copy entries from one table to another
*/
void tableAddAll(Table* from, Table* to) {
    for (int i = 0; i < from->capacity; i++) {
        Entry* entry = &from->entries[i];
        if (entry->key != NULL) {
            tableSet(to, entry->key, entry->value);
        }
    }
}

bool tableGet(Table* table, ObjString* key, Value* value) {
    if (table->count == 0) return false;

    Entry* entry = findEntry(table->entries, table->capacity, key);
    if (entry->key == NULL) return false;

    *value = entry->value;
    return true;
}

/**
* to no break the linear probing, instead of simply deleting the entry, we had
* a "tombstone", a value to indicate that the entry was deleting, and when
* looking for a key, we can skip this bucket and not break linear probing.
*
* here a tombstone is (key: NULL, value: Value {.type: VAL_BOOL, .boolean = true})
*
* A tombstone doesn't decrement table->count.
* Because, if we do, and because of linear probing, we could end up running out
* of buckets, and be stuck in infinite loop in findEntry.
* (because a tombstone cannot be used as an empty bucket (otherwise it breaks
* linear probing)).
* If we consider, a tombstone to be an regular entry, then we will end up with
* a bigger array.
* The trick is to not increase count when inserting an entry in a tombstone.
*/
bool tableDelete(Table* table, ObjString* key) {
    if (table->count == 0) return false;

    // Find the entry.
    Entry* entry = findEntry(table->entries, table->capacity, key);
    if (entry->key == NULL) return false;

    // Place a tombstone in the entry.
    entry->key = NULL;
    entry->value = BOOL_VAL(true);
    return true;
}


/**
* This is the one place in the VM where we actually test strings for textual
* equality. We do it here to deduplicate strings and then the rest of the VM
* can take for granted that any 2 strings at different addresses in memory
* must have different contents.
*/
ObjString* tableFindString(Table* table, const char* chars,
                           int length, uint32_t hash) {

    if (table->count == 0) return NULL;
    uint32_t index = hash % table->capacity;

    for (;;) {
        Entry* entry = &table->entries[index];
        if (entry->key == NULL) {
            // Stop if we find an empty non-tombstone entry.
            if (IS_NIL(entry->value)) return NULL;
        } else if (entry->key->length == length &&
            entry->key->hash == hash && // first compare hash
            memcmp(entry->key->chars, chars, length) == 0) { // then compare char by char
            // We found it.
            return entry->key;
        }

        index = (index + 1) % table->capacity;
    }
}
