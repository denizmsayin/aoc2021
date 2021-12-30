#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <assert.h>

#include "dheap/dheap.h"

#define N 10000

typedef uint64_t ugrid_t[N][N];
typedef uint8_t bytegrid_t[N][N];

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

int main(void)
{
    static bytegrid_t grid;
    static ugrid_t dists;
    static dheap_t heap = DHEAP_INIT;
    long nrow, ncol;

    read_bytegrid(grid, &nrow, &ncol);
    tile(grid, &nrow, &ncol, 5, 5);
    memset(dists, 255, sizeof(dists)); // Setting all bits to 1 will make ULONG_MAX
    dists[0][0] = 0;
    dheap_add(&heap, enc(0, 0), 0);
    while (!dheap_empty(&heap)) {
        int32_t i, j;
        size_t k;
        uint64_t nearest_dist = dheap_min(&heap);
        uint64_t nearest_node = dheap_min_key(&heap);
        dheap_pop_min(&heap);
        dec(nearest_node, &i, &j);

        // Reached the last node
        if (i == nrow - 1 && j == ncol - 1)
            break;

        // There is no decrease-key for this variant, so just skip
        // if the dist > the one stored in dists. Deals with duplicate nodes.
        if (nearest_dist > dists[i][j])
            continue;

        for (k = 0; k < N_OFFS; k++) {
            int32_t ni = i + I_OFFS[k], nj = j + J_OFFS[k];
            if (0 <= ni && ni < nrow && 0 <= nj && nj < ncol) {
                uint64_t tentative_dist = nearest_dist + grid[ni][nj];
                if (tentative_dist < dists[ni][nj]) {
                    dheap_add(&heap, enc(ni, nj), tentative_dist);
                    dists[ni][nj] = tentative_dist;                    
                }
            }
        }
    }

    printf("%lu\n", dists[nrow-1][ncol-1]);
    return 0;
}
