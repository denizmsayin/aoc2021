#ifndef DHASHTABLE_H_
#define DHASHTABLE_H_

#include <stdint.h>
#include <stddef.h>

/* Repurposed generic hashtable I implemented as an example back in "21 Spring */

struct dhashtable_pair {
    const void *key;
    uint64_t value;
};

struct dhashtable_entry;

struct dhashtable_ops {
    uint64_t (*hash)(const void *);
    int (*eq)(const void *, const void *);
};

typedef struct dhashtable {
    struct dhashtable_entry *entries;
    size_t num_entries;
    unsigned bits;
    const struct dhashtable_ops *ops;
} dhashtable_t;

void dhashtable_init(struct dhashtable *table, const struct dhashtable_ops *ops);
void dhashtable_destroy(struct dhashtable *table);

struct dhashtable_pair *dhashtable_insert(struct dhashtable *table, 
                                           const void *key, uint64_t value);

struct dhashtable_pair *dhashtable_lookup(const struct dhashtable *table, const void *key);
struct dhashtable_pair dhashtable_remove(struct dhashtable *table, const void *key);
void dhashtable_update(struct dhashtable *table, const void *key, uint64_t (*update)(uint64_t));

// Iterators for the hash table
struct dhashtable_pair *dhashtable_first(const struct dhashtable *table);
struct dhashtable_pair *dhashtable_next(const struct dhashtable *table,
                                         const struct dhashtable_pair *last);


void dhashtable_print(const struct dhashtable *table);
void dhashtable_full_print(const struct dhashtable *table);

// A nice alternative for dijkstra!
struct dhashtable_pair *dhashtable_lookup_or_insert(struct dhashtable *table, 
                                                     const void *key, uint64_t value);

#endif
