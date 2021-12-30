#ifndef DHEAP_H_
#define DHEAP_H_

#include <stdint.h>
#include <stddef.h>

// Around 1mil elements. Better define as static
// if you don't want the stack to blow up!
#define DHEAP_CAP 1048576 

typedef struct {
    struct dheapnode {
        uint64_t value;
        uint64_t key;
    } array[DHEAP_CAP];
    size_t size;
} dheap_t;

#define DHEAP_INIT { .size = 0 }

int dheap_empty(const dheap_t *);
void dheap_add(dheap_t *, uint64_t k, uint64_t v);
uint64_t dheap_min(const dheap_t *);
uint64_t dheap_min_key(const dheap_t *);
void dheap_pop_min(dheap_t *);

// For testing
int dheap_heap_property_holds(const dheap_t *);

#endif

