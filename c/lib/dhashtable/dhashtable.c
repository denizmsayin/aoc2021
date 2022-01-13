#include "dhashtable.h"

#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdarg.h>

#define HASH_TABLE_MIN_BITS 5 // 32 size

#define BITS2SIZE(x)    (1UL << (x))
#define BITMASK(x)      ((1UL << (x)) - 1)

#define HASH_TABLE_MIN_SIZE BITS2SIZE(HASH_TABLE_MIN_BITS)

#define MAX_LOAD_FACTOR_INV 2 // 1/2 -> 0.5 is the max load factor

static void xerror(const char *fmt, ...)
{
    va_list args;
    fprintf(stderr, "Error: ");
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    putc('\n', stderr);
    exit(EXIT_FAILURE);
}

// This little enum is going to cost us
// many bytes, thanks alignment!
enum dhashtable_entry_type {
    HT_ENTRY_EMPTY = 0, // <- Zero allocations will make empty entries
    HT_ENTRY_MARKED = 1, // Existed before, but removed
    HT_ENTRY_FULL = 2, // Contains value right now
};

struct dhashtable_entry {
    struct dhashtable_pair pair;
    enum dhashtable_entry_type type;
    uint64_t hash_value;
};

void dhashtable_init(struct dhashtable *table, const struct dhashtable_ops *ops)
{
    table->bits = HASH_TABLE_MIN_BITS;
    table->num_entries = 0;
    table->entries = calloc(HASH_TABLE_MIN_SIZE, sizeof(*table->entries));
    table->ops = ops;
}

void dhashtable_destroy(struct dhashtable *table)
{
    free(table->entries);
}

static inline bool load_exceeded(const struct dhashtable *table)
{
    return MAX_LOAD_FACTOR_INV * table->num_entries > BITS2SIZE(table->bits);
}

static inline void insert_into_entry(struct dhashtable_entry *entry, 
                                     const void *key, uint64_t value, uint64_t hash)
{
    entry->type = HT_ENTRY_FULL;
    entry->hash_value = hash;
    entry->pair.key = key;
    entry->pair.value = value;
}

static struct dhashtable_pair *insert_pair_hashed(
        struct dhashtable_entry *entries,
        unsigned bits,
        const void *key, uint64_t value, uint64_t hash_value,
        int *is_new_key, const struct dhashtable_ops *ops)
{
    uint64_t i, offset;
    struct dhashtable_entry *last = NULL;
    uint64_t mask = BITMASK(bits); // Used to do modulo
    // Triangular numbers are famous with 2^ hashes apparently, so
    // I'm going to use them. Finding an empty spot is guaranteed!
    // http://www.chilton-computing.org.uk/acl/literature/reports/p012.htm
    for (i = hash_value & mask, offset = 1;
         offset <= BITS2SIZE(bits); 
         i = (i + offset) & mask, offset++)
    {
        // Something I initially neglected: the chain must absolutely
        // be scanned until its end, to ensure no duplicate key insertions
        // happen. However, once the end is reached, insertion can be made
        // into a previously found sentinel value.
        struct dhashtable_entry *entry = &entries[i];
        switch (entry->type) {
        case HT_ENTRY_FULL: 
            if (hash_value == entry->hash_value && ops->eq(entry->pair.key, key)) {
                entry->pair.value = value;
                if (is_new_key)
                    *is_new_key = 0;
                return &entry->pair;
            }
            break;
        case HT_ENTRY_MARKED:
            // Save the first sentinel entry for writing the last key
            last = last ? last : entry;
            break;
        case HT_ENTRY_EMPTY:
            // Either select the first sentinel, or the cur entry if it does not exist
            last = last ? last : entry;
            insert_into_entry(last, key, value, hash_value);
            if (is_new_key)
                *is_new_key = 1;
            return &last->pair;
        }
    }
    xerror("Went through the whole hash table without finding a spot, must be a bug...");
    return NULL;
}

static struct dhashtable_pair *insert_pair(
        struct dhashtable_entry *entries,
        unsigned bits,
        const void *key, uint64_t value,
        int *is_new_key,
        const struct dhashtable_ops *ops)
{
    return insert_pair_hashed(entries, bits, key, value, ops->hash(key), is_new_key, ops);
}

static void dhashtable_grow_and_rehash(struct dhashtable *table)
{
    // Create a new entry array and rehash into it
    struct dhashtable_entry *new_entries;
    unsigned new_bits = table->bits + 1;
    size_t i, size;
    new_entries = calloc(BITS2SIZE(new_bits), sizeof(*new_entries));
    for (i = 0, size = BITS2SIZE(table->bits); i < size; i++) {
        const struct dhashtable_entry *entry = &table->entries[i];
        if (entry->type == HT_ENTRY_FULL)
            insert_pair_hashed(new_entries, new_bits, entry->pair.key, entry->pair.value, 
                               entry->hash_value, NULL, table->ops);
    }
    // Assign the new array to the table
    free(table->entries);
    table->entries = new_entries;
    table->bits = new_bits;
}

struct dhashtable_pair *dhashtable_insert(
        struct dhashtable *table, 
        const void *key, uint64_t value)
{
    int is_new_key;
    if (load_exceeded(table))
        dhashtable_grow_and_rehash(table);
    struct dhashtable_pair *r = insert_pair(table->entries, table->bits, 
                                             key, value, &is_new_key, table->ops);
    if (is_new_key)
        table->num_entries++;
    return r;
}

static struct dhashtable_entry *dhashtable_lookup_e(const struct dhashtable *table, 
                                                    const void *key)
{
    uint64_t i, offset, mask = BITMASK(table->bits), hash_value = table->ops->hash(key);
    // Triangular numbers are famous with 2^ hashes apparently, so
    // I'm going to use them. Finding an empty spot is guaranteed!
    // http://www.chilton-computing.org.uk/acl/literature/reports/p012.htm
    for (i = hash_value & mask, offset = 1;
         offset <= BITS2SIZE(table->bits); 
         i = (i + offset) & mask, offset++)
    {
        struct dhashtable_entry *entry = &table->entries[i];
        if (entry->type == HT_ENTRY_EMPTY)
            return NULL;
        if (entry->type == HT_ENTRY_FULL && hash_value == entry->hash_value && 
            table->ops->eq(entry->pair.key, key))
            return entry;
    }
    return NULL;
}

struct dhashtable_pair *dhashtable_lookup(const struct dhashtable *table, const void *key)
{
    struct dhashtable_entry *m = dhashtable_lookup_e(table, key);
    return m ? &m->pair : NULL; // return the entry without the extra table metadata
}

struct dhashtable_pair *dhashtable_lookup_or_insert(struct dhashtable *table, 
                                                    const void *key, uint64_t value)
{
    struct dhashtable_pair *p = dhashtable_lookup(table, key);
    if (p)
        return p;
    return dhashtable_insert(table, key, value);
}

static const struct dhashtable_pair NULL_PAIR = {0, 0};

struct dhashtable_pair dhashtable_remove(struct dhashtable *table, const void *key)
{
    struct dhashtable_entry *found = dhashtable_lookup_e(table, key);
    if (found) {
        found->type = HT_ENTRY_MARKED;
        return found->pair;
    } else {
        xerror("hash_table_remove: Key %lu not present in table at %p", key, table);
        return NULL_PAIR;
    }
}

void dhashtable_update(struct dhashtable *table, const void *key, uint64_t (*update)(uint64_t))
{
    struct dhashtable_entry *found = dhashtable_lookup_e(table, key);
    if (found) 
        found->pair.value = update(found->pair.value);
    else 
        xerror("hash_table_update: Key %lu not present in table at %p", key, table);
}

struct dhashtable_pair *dhashtable_first(const struct dhashtable *table)
{
    uint64_t i, size;
    for (i = 0, size = BITS2SIZE(table->bits); i < size; i++) {
        struct dhashtable_entry *entry = &table->entries[i];
        if (entry->type == HT_ENTRY_FULL)
            return &entry->pair;
    }
    return NULL;
}

static const struct dhashtable_entry *dhashtable_end(const struct dhashtable *table)
{
    return &table->entries[BITS2SIZE(table->bits)];
}


#define containerof(ptr, type, member)\
    ((type *) ((char *) ptr - offsetof(type, member)))

#define marked_entry_of(entry_ptr)\
    containerof(entry_ptr, struct dhashtable_entry, data)

// Could make next not need the table by adding a special end marker, but that
// seems to be too much effort compared to just passing the table as well!
struct dhashtable_pair *dhashtable_next(
        const struct dhashtable *table,
        const struct dhashtable_pair *last) 
{
    // Get the containing entry and iterate forward!
    // Need containerof to go from the pair to the entry.
    struct dhashtable_entry *base, *itr;
    base = containerof(last, struct dhashtable_entry, pair);
    for (itr = base + 1; itr != dhashtable_end(table); itr++)
        if (itr->type == HT_ENTRY_FULL)
            return &itr->pair;
    return NULL;
}

static char type2char(enum dhashtable_entry_type type)
{
    switch (type) {
        case HT_ENTRY_FULL:     return 'F';
        case HT_ENTRY_MARKED:   return 'M';
        case HT_ENTRY_EMPTY:    return 'E';
        default:                return '?';
    }
}

void dhashtable_print(const struct dhashtable *table)
{
    const struct dhashtable_pair *pair;
    puts("--------------TABLE---------------");
    for (pair = dhashtable_first(table); pair != NULL; pair = dhashtable_next(table, pair)) {
        printf("%p %lu\n", pair->key, pair->value);
        putchar('\n');
    }
    fputs("----------------------------------", stdout); // No newline
}

void dhashtable_full_print(const struct dhashtable *table)
{
    size_t i, size, count = 0;
    puts("--------------------------------------------");
    for (i = 0, size = BITS2SIZE(table->bits); i < size; i++) {
        const struct dhashtable_entry *entry = &table->entries[i];
        if (entry->type == HT_ENTRY_FULL) {
            printf("Entry #%02lu, type: %c, content: %p=%lu", i, type2char(entry->type),
                   entry->pair.key, entry->pair.value);
            count++;
        } else {
            printf("Entry #%02lu, type: %c", i, type2char(entry->type));
        }
        putchar('\n');
//         printf("|%c|%-20s|%-20s|\n", type2char(entry->type),
//                table->ops->k_repr(entry->pair.key, kbuf),
//                table->ops->v_repr(entry->pair.value, vbuf));
        puts("--------------------------------------------");
    }
    if (count != table->num_entries)
        xerror("Count=%lu did not match num_entries=%lu.\n", count, table->num_entries);
}

