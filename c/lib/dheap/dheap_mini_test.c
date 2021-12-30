#include "dheap.h"

#include <stdio.h>
#include <assert.h>

int main(void)
{
    static uint64_t values[] = { 10, 17, 3, 54, 12, 15 };
    static dheap_t heap = DHEAP_INIT;

    puts("Adding values:");
    for (size_t i = 0; i < sizeof(values) / sizeof(values[0]); i++) {
        dheap_add(&heap, 0, values[i]);
        printf("Added %lu\n", values[i]);
        printf("Min so far: %lu\n", dheap_min(&heap));
        assert (dheap_heap_property_holds(&heap));
    }

    puts("Popping mins:");
    while (!dheap_empty(&heap)) {
        printf("%lu\n", dheap_min(&heap));
        assert (dheap_heap_property_holds(&heap));
        dheap_pop_min(&heap);
    }

    return 0;
}
