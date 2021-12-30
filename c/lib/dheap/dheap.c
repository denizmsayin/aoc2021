#include "dheap.h"

#include <assert.h>

int dheap_empty(const dheap_t *h) { return h->size == 0; }

static inline size_t pidx(size_t i) { assert (i > 0); return (i - 1) >> 1; }
static inline size_t lidx(size_t i) { return (i << 1) + 1; }

/* Return the child with the least value. */
static inline size_t select_child(const dheap_t *h, size_t i)
{
    size_t li = lidx(i);
    size_t ri = li + 1;
    if (ri >= h->size || h->array[li].value < h->array[ri].value)
        return li;
    return ri;
}

static void perc_down(dheap_t *h, size_t i)
{
    struct dheapnode *arr = h->array;
    struct dheapnode v = arr[i];
    uint64_t vval = v.value;
    size_t size = h->size, ci;
    // Move down while value is < than children.
    while (lidx(i) < size && arr[(ci = select_child(h, i))].value < vval) {
        arr[i] = arr[ci];
        i = ci;
    }
    arr[i] = v; // Put in proper position.
}

static void perc_up(dheap_t *h, size_t i)
{
    struct dheapnode *arr = h->array;
    struct dheapnode v = arr[i];
    uint64_t vval = v.value;
    size_t p;
    while (i > 0 && arr[(p = pidx(i))].value > vval) {
        arr[i] = arr[p];
        i = p;
    }
    arr[i] = v;
}

void dheap_add(dheap_t *h, uint64_t k, uint64_t v)
{
    size_t sz = h->size;
    assert (sz < DHEAP_CAP);
    h->array[sz] = (struct dheapnode) { .value = v, .key = k };
    perc_up(h, sz);
    h->size = sz + 1;
}

uint64_t dheap_min(const dheap_t *h)
{
    assert (!dheap_empty(h));
    return h->array[0].value;
}

uint64_t dheap_min_key(const dheap_t *h)
{
    assert (!dheap_empty(h));
    return h->array[0].key;
}

void dheap_pop_min(dheap_t *h)
{
    struct dheapnode *arr = h->array;
    assert (!dheap_empty(h));
    arr[0] = arr[--h->size];
    if (h->size > 0)
        perc_down(h, 0);
}

static int heap_check(const struct dheapnode *arr, size_t n, size_t i)
{
    if (0 < i && i < n) {
        size_t p = pidx(i);
        int heap_holds = arr[p].value <= arr[i].value;
        heap_holds = heap_holds && heap_check(arr, n, lidx(i));
        heap_holds = heap_holds && heap_check(arr, n, lidx(i) + 1);
        return heap_holds;
    } else {
        return 1;
    }
}

int dheap_heap_property_holds(const dheap_t *h)
{
    return heap_check(h->array, h->size, 0);
}
