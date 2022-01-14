#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <assert.h>

#ifndef N
#error N must be defined.
#endif

#ifndef COST_MAX
#error COST_MAX must be defined.
#endif

#ifndef STACK_CAP
#error STACK_CAP must be defined.
#endif

typedef uint64_t ugrid_t[N][N];
typedef uint8_t bytegrid_t[N][N];

typedef uint16_t u16;
typedef uint32_t u32;
typedef int16_t i16;


struct pos {
    i16 i, j;
};

struct stack {
    u16 size;
    struct pos stack[STACK_CAP];
};

struct cheap {
    u16 min_cost;
    size_t size;
    struct stack stacks[COST_MAX + 1];
};

static inline struct pos mkpos(i16 a, i16 b)
{
    return (struct pos) { a, b };
}

#define CHEAP_INIT { .min_cost = COST_MAX + 1, .size = 0, .stacks = {{0}} }

static int cheap_empty(const struct cheap *h)
{
    return h->size == 0;
}

static void cheap_push(struct cheap *h, struct pos pos, u16 cost)
{
    assert (cost <= COST_MAX);
    struct stack *s = &h->stacks[cost];
    assert (s->size < STACK_CAP);
    s->stack[s->size++] = pos;
    if (cost < h->min_cost)
        h->min_cost = cost;
    h->size++;
}

static u16 cheap_pop_min(struct cheap *h, struct pos *out_pos)
{
    assert (h->size > 0);
    struct stack *s = &h->stacks[h->min_cost];
    u16 ret = h->min_cost;
    *out_pos = s->stack[--s->size];
    h->size--;
    if (h->size == 0) {
        h->min_cost = COST_MAX + 1;
    } else if (s->size == 0) { // Gonna do a scan here
        for (int cost = h->min_cost + 1; cost <= COST_MAX; cost++) {
            if (h->stacks[cost].size > 0) {
                h->min_cost = cost;
                break;
            }
        }
    }
    return ret;
}

void cheap_print(const struct cheap *h)
{
    printf("Size: %lu, MinCost: %hu\n", h->size, h->min_cost);
    for (u16 cost = 0; cost <= COST_MAX; cost++) {
        const struct stack *s = &h->stacks[cost];
        if (s->size > 0) {
            printf("Cost %hu: ", cost);
            for (int i = (int) s->size - 1; i >= 0; i--)
                printf("(%d, %d) ", s->stack[i].i, s->stack[i].j);
            putchar('\n');
        }
    }
}

void read_bytegrid(bytegrid_t grid, long *nrow, long *ncol)
{
    char line_buf[N];
    size_t i = 0, j = 0;
   
    while (fgets(line_buf, N, stdin)) {
        for (j = 0; line_buf[j] && line_buf[j] != '\n'; j++) {
            assert ('0' <= line_buf[j] && line_buf[j] <= '9');
            grid[i][j] = line_buf[j] - '0';
        }
        i++;
    }

    *nrow = i;
    *ncol = j;
}

void tile(bytegrid_t grid, long *nrows, long *ncols, long a, long b)
{
    long ca, cb;
    for (ca = 0; ca < a; ca++) {
        for (cb = 0; cb < b; cb++) {
            char inc = ca + cb;
            long ibase = ca * *nrows;
            long jbase = cb * *ncols;
            long i, j;
            for (i = 0; i < *nrows; i++)
                for (j = 0; j < *ncols; j++)
                    grid[ibase + i][jbase + j] = (grid[i][j] + inc - 1) % 9 + 1;
        }
    }
    *nrows *= a; 
    *ncols *= b;
}

/* Need to encode i,j into 64-bit keys to use with dheap. */
inline uint64_t enc(int32_t i, int32_t j)
{
    uint64_t e = j;
    e |= ((uint64_t) i) << 32;
    return e;
}

inline void dec(uint64_t e, int32_t *i, int32_t *j)
{
    *i = (int32_t) (e >> 32);
    *j = (int32_t) e;
}

static int32_t I_OFFS[] = {-1, 1, 0, 0};
static int32_t J_OFFS[] = {0, 0, -1, 1};

#define N_OFFS (sizeof(I_OFFS) / sizeof(I_OFFS[0]))

void day15_main(int part2)
{
    static bytegrid_t grid;
    static ugrid_t dists;
    // static dheap_t heap = DHEAP_INIT;
    static struct cheap heap = CHEAP_INIT;
    long nrow, ncol;

    read_bytegrid(grid, &nrow, &ncol);
    
    if (part2)
        tile(grid, &nrow, &ncol, 5, 5);

    memset(dists, 255, sizeof(dists)); // Setting all bits to 1 will make ULONG_MAX
    dists[0][0] = 0;
    cheap_push(&heap, mkpos(0, 0), 0);
//     cheap_print(&heap);
    while (!cheap_empty(&heap)) {
        struct pos nearest_pos; 
        size_t k;
        u16 nearest_dist = cheap_pop_min(&heap, &nearest_pos);
        i16 i = nearest_pos.i, j = nearest_pos.j;
//         printf("Popped (%d, %d), cost=%d\nv", i, j, nearest_dist);
//         cheap_print(&heap);

        // Reached the last node
        if (i == nrow - 1 && j == ncol - 1)
            break;

        // There is no decrease-key for this variant, so just skip
        // if the dist > the one stored in dists. Deals with duplicate nodes.
        if (nearest_dist > dists[i][j])
            continue;

        for (k = 0; k < N_OFFS; k++) {
            i16 ni = i + I_OFFS[k], nj = j + J_OFFS[k];
            if (0 <= ni && ni < nrow && 0 <= nj && nj < ncol) {
                u16 tentative_dist = nearest_dist + grid[ni][nj];
                if (tentative_dist < dists[ni][nj]) {
//                     printf("Pushed (%d, %d), cost=%d\n", ni, nj, tentative_dist);
                    cheap_push(&heap, mkpos(ni, nj), tentative_dist);
//                     cheap_print(&heap);
                    dists[ni][nj] = tentative_dist;                    
                }
            }
        }
    }

    printf("%lu\n", dists[nrow-1][ncol-1]);
}
