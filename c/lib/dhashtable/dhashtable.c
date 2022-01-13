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

// Linux kernel style hashing
#define GOLDEN_RATIO_64 0x61C8864680B583EBull

static inline uint64_t hashf64(uint64_t x) { return x * GOLDEN_RATIO_64; } 

// This little enum is going to cost us
// many bytes, thanks alignment!
enum dhash_table_entry_type {
    HT_ENTRY_EMPTY = 0, // <- Zero allocations will make empty entries
    HT_ENTRY_MARKED = 1, // Existed before, but removed
    HT_ENTRY_FULL = 2, // Contains value right now
};

struct dhash_table_entry {
    struct dhash_table_pair pair;
    enum dhash_table_entry_type type;
    uint64_t hash_value;
};

void dhashtable_init(struct dhash_table *table)
{
    table->bits = HASH_TABLE_MIN_BITS;
    table->num_entries = 0;
    table->entries = calloc(HASH_TABLE_MIN_SIZE, sizeof(*table->entries));
    table->num_collisions = 0;
}

void dhashtable_destroy(struct dhash_table *table)
{
    free(table->entries);
}

static inline bool load_exceeded(const struct dhash_table *table)
{
    return MAX_LOAD_FACTOR_INV * table->num_entries > BITS2SIZE(table->bits);
}

static inline void insert_into_entry(struct dhash_table_entry *entry, 
                                     uint64_t key, uint64_t value, uint64_t hash)
{
    entry->type = HT_ENTRY_FULL;
    entry->hash_value = hash;
    entry->pair.key = key;
    entry->pair.value = value;
}

static struct dhash_table_pair *insert_pair_hashed(
        struct dhash_table_entry *entries,
        unsigned bits,
        uint64_t key, uint64_t value, uint64_t hash_value,
        int *is_new_key, unsigned *cc)
{
    uint64_t i, offset;
    struct dhash_table_entry *last = NULL;
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
        struct dhash_table_entry *entry = &entries[i];
        switch (entry->type) {
        case HT_ENTRY_FULL: 
            if (hash_value == entry->hash_value && entry->pair.key == key) {
                entry->pair.value = value;
                if (is_new_key)
                    *is_new_key = 0;
                return &entry->pair;
            } else {
                *cc += 1;
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

static struct dhash_table_pair *insert_pair(
        struct dhash_table_entry *entries,
        unsigned bits,
        uint64_t key, uint64_t value,
        int *is_new_key,
        unsigned *cc)
{
    return insert_pair_hashed(entries, bits, key, value, hashf64(key), is_new_key, cc);
}

static void dhashtable_grow_and_rehash(struct dhash_table *table)
{
    // Create a new entry array and rehash into it
    struct dhash_table_entry *new_entries;
    unsigned new_bits = table->bits + 1;
    size_t i, size;
    new_entries = calloc(BITS2SIZE(new_bits), sizeof(*new_entries));
    for (i = 0, size = BITS2SIZE(table->bits); i < size; i++) {
        const struct dhash_table_entry *entry = &table->entries[i];
        if (entry->type == HT_ENTRY_FULL)
            insert_pair_hashed(new_entries, new_bits, entry->pair.key,
                               entry->pair.value, entry->hash_value, NULL, &table->num_collisions);
    }
    // Assign the new array to the table
    free(table->entries);
    table->entries = new_entries;
    table->bits = new_bits;
}

struct dhash_table_pair *dhashtable_insert(
        struct dhash_table *table, 
        uint64_t key, uint64_t value)
{
    int is_new_key;
    if (load_exceeded(table))
        dhashtable_grow_and_rehash(table);
    struct dhash_table_pair *r = insert_pair(table->entries, table->bits, 
                                             key, value, &is_new_key, &table->num_collisions);
    if (is_new_key)
        table->num_entries++;
    return r;
}

static struct dhash_table_entry *dhashtable_lookup_e(const struct dhash_table *table, 
                                                     uint64_t key)
{
    uint64_t i, offset, mask = BITMASK(table->bits), hash_value = hashf64(key);
    // Triangular numbers are famous with 2^ hashes apparently, so
    // I'm going to use them. Finding an empty spot is guaranteed!
    // http://www.chilton-computing.org.uk/acl/literature/reports/p012.htm
    for (i = hash_value & mask, offset = 1;
         offset <= BITS2SIZE(table->bits); 
         i = (i + offset) & mask, offset++)
    {
        struct dhash_table_entry *entry = &table->entries[i];
        if (entry->type == HT_ENTRY_EMPTY)
            return NULL;
        if (entry->type == HT_ENTRY_FULL && hash_value == entry->hash_value && 
            entry->pair.key == key)
            return entry;
    }
    return NULL;
}

struct dhash_table_pair *dhashtable_lookup(const struct dhash_table *table, uint64_t key)
{
    struct dhash_table_entry *m = dhashtable_lookup_e(table, key);
    return m ? &m->pair : NULL; // return the entry without the extra table metadata
}

struct dhash_table_pair *dhashtable_lookup_or_insert(struct dhash_table *table, 
                                                     uint64_t key, uint64_t value)
{
    struct dhash_table_pair *p = dhashtable_lookup(table, key);
    if (p)
        return p;
    return dhashtable_insert(table, key, value);
}

static const struct dhash_table_pair NULL_PAIR = {0, 0};

struct dhash_table_pair dhashtable_remove(struct dhash_table *table, uint64_t key)
{
    struct dhash_table_entry *found = dhashtable_lookup_e(table, key);
    if (found) {
        found->type = HT_ENTRY_MARKED;
        return found->pair;
    } else {
        xerror("hash_table_remove: Key %lu not present in table at %p", key, table);
        return NULL_PAIR;
    }
}

void dhashtable_update(struct dhash_table *table, uint64_t key, uint64_t (*update)(uint64_t))
{
    struct dhash_table_entry *found = dhashtable_lookup_e(table, key);
    if (found) 
        found->pair.value = update(found->pair.value);
    else 
        xerror("hash_table_update: Key %lu not present in table at %p", key, table);
}

struct dhash_table_pair *dhashtable_first(const struct dhash_table *table)
{
    uint64_t i, size;
    for (i = 0, size = BITS2SIZE(table->bits); i < size; i++) {
        struct dhash_table_entry *entry = &table->entries[i];
        if (entry->type == HT_ENTRY_FULL)
            return &entry->pair;
    }
    return NULL;
}

static const struct dhash_table_entry *dhashtable_end(const struct dhash_table *table)
{
    return &table->entries[BITS2SIZE(table->bits)];
}


#define containerof(ptr, type, member)\
    ((type *) ((char *) ptr - offsetof(type, member)))

#define marked_entry_of(entry_ptr)\
    containerof(entry_ptr, struct dhash_table_entry, data)

// Could make next not need the table by adding a special end marker, but that
// seems to be too much effort compared to just passing the table as well!
struct dhash_table_pair *dhashtable_next(
        const struct dhash_table *table,
        const struct dhash_table_pair *last) 
{
    // Get the containing entry and iterate forward!
    // Need containerof to go from the pair to the entry.
    struct dhash_table_entry *base, *itr;
    base = containerof(last, struct dhash_table_entry, pair);
    for (itr = base + 1; itr != dhashtable_end(table); itr++)
        if (itr->type == HT_ENTRY_FULL)
            return &itr->pair;
    return NULL;
}

static char type2char(enum dhash_table_entry_type type)
{
    switch (type) {
        case HT_ENTRY_FULL:     return 'F';
        case HT_ENTRY_MARKED:   return 'M';
        case HT_ENTRY_EMPTY:    return 'E';
        default:                return '?';
    }
}

void dhashtable_print(const struct dhash_table *table)
{
    const struct dhash_table_pair *pair;
    puts("--------------TABLE---------------");
    for (pair = dhashtable_first(table); pair != NULL; pair = dhashtable_next(table, pair)) {
        printf("%lu %lu\n", pair->key, pair->value);
        putchar('\n');
    }
    fputs("----------------------------------", stdout); // No newline
}

void dhashtable_full_print(const struct dhash_table *table)
{
    size_t i, size, count = 0;
    puts("--------------------------------------------");
    for (i = 0, size = BITS2SIZE(table->bits); i < size; i++) {
        const struct dhash_table_entry *entry = &table->entries[i];
        if (entry->type == HT_ENTRY_FULL) {
            printf("Entry #%02lu, type: %c, content: %lu=%lu", i, type2char(entry->type),
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

