#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>

#define N 1024
#define N_STEPS 100

typedef long grid_t[N][N];
typedef char bytegrid_t[N][N];

static long I_OFFS[] = {1, -1, 0, 0, 1, -1, 1, -1};
static long J_OFFS[] = {0, 0, 1, -1, -1, 1, 1, -1};
#define N_OFFS (sizeof(I_OFFS) / sizeof(I_OFFS[0]))

void read_sentinel_grid(grid_t grid, long *nrow, long *ncol)
{
    char line_buf[N];
    size_t i, j;
   
    // Sentinel values negative inf, so that they always remain
    // negative even after many steps of flashes
    for (i = 0; i < N; i++)
        for (j = 0; j < N; j++)
            grid[i][j] = LONG_MIN;

    i = 0;
    while (fgets(line_buf, N, stdin)) {
        for (j = 0; line_buf[j] && line_buf[j] != '\n'; j++) {
            assert ('0' <= line_buf[j] && line_buf[j] <= '9');
            grid[i + 1][j + 1] = line_buf[j] - '0';
        }
        i++;
    }

    *nrow = i;
    *ncol = j;
}

struct pair {
    int i, j;
};

int main(void)
{
    static grid_t grid;
    long rows, cols;
    long i, j, total_flashes;
    size_t k, step;
    
    read_sentinel_grid(grid, &rows, &cols);

    total_flashes = 0;
    for (step = 0; step < N_STEPS; step++) {
        static struct pair flashes[N*N] = {};
        size_t n_flashes = 0;

        // Increment step
        for (i = 1; i <= rows; i++)
            for (j = 1; j <= cols; j++)
                if (++grid[i][j] > 9)
                    flashes[n_flashes++] = (struct pair) {i, j};

        // Looping over the whole grid and not tracking would be
        // too lazy even for me :) Negative values act like 'visited'
        while (n_flashes) {
            struct pair pop = flashes[--n_flashes];
            if (grid[pop.i][pop.j] > 0) {
                grid[pop.i][pop.j] = LONG_MIN;
                total_flashes++;
                for (k = 0; k < N_OFFS; k++) {
                    long ni = pop.i + I_OFFS[k];
                    long nj = pop.j + J_OFFS[k];
                    if (++grid[ni][nj] > 9)
                        flashes[n_flashes++] = (struct pair) {ni, nj};
                }
            }
        }

        // Zero the flashed positions previously set to LONG_MIN
        for (i = 1; i <= rows; i++)
            for (j = 1; j <= cols; j++)
                if (grid[i][j] < 0)
                    grid[i][j] = 0;
    }
    printf("%ld\n", total_flashes);

    return 0;
}

