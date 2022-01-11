#ifndef DHASHTABLE_H_
#define DHASHTABLE_H_

#include <stdint.h>
#include <stddef.h>

/* Repurposed generic hashtable I implemented as an example back in "21 Spring */

struct dhash_table_pair {
    uint64_t key;
    uint64_t value;
};

struct dhash_table_entry;

typedef struct dhash_table {
    struct dhash_table_entry *entries;
    size_t num_entries;
    unsigned bits;
} dhashtable_t;

void dhashtable_init(struct dhash_table *table);
void dhashtable_destroy(struct dhash_table *table);

struct dhash_table_pair *dhashtable_insert(struct dhash_table *table, 
                                           uint64_t key, uint64_t value);

struct dhash_table_pair *dhashtable_lookup(const struct dhash_table *table, uint64_t key);
struct dhash_table_pair dhashtable_remove(struct dhash_table *table, uint64_t key);
void dhashtable_update(struct dhash_table *table, uint64_t key, uint64_t (*update)(uint64_t));

// Iterators for the hash table
struct dhash_table_pair *dhashtable_first(const struct dhash_table *table);
struct dhash_table_pair *dhashtable_next(const struct dhash_table *table,
                                         const struct dhash_table_pair *last);


void dhashtable_print(const struct dhash_table *table);
void dhashtable_full_print(const struct dhash_table *table);

#endif
