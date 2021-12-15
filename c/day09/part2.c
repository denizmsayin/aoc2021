#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#define N 1024

typedef unsigned char uchar;
typedef uchar grid_t[N][N];

static long I_OFFS[] = {1, -1, 0, 0};
static long J_OFFS[] = {0, 0, 1, -1};
#define N_OFFS (sizeof(I_OFFS) / sizeof(I_OFFS[0]))

void read_sentinel_grid(grid_t grid, long *nrow, long *ncol)
{
    char line_buf[N];
    size_t i, j;
    
    memset(grid, 9, N * N * sizeof(grid[0][0]));
    
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

int revcmp(const void *xp, const void *yp)
{
    long x = *(long *) xp;
    long y = *(long *) yp;
    if (x < y)
        return 1;
    else if (x == y)
        return 0;
    return -1;
}

int main(void)
{
    static grid_t grid; 
    long rows, cols;
    long i, j;
    long basin_sizes[N], n_basins = 0;
    size_t k;

    read_sentinel_grid(grid, &rows, &cols);
    for (i = 1; i <= rows; i++) {
        for (j = 1; j <= cols; j++) {
            // Detect if low
            int is_low = 1;
            for (k = 0; k < N_OFFS; k++) {
               long ni = i + I_OFFS[k];
               long nj = j + J_OFFS[k];
               if (grid[ni][nj] <= grid[i][j]) {
                   is_low = 0;
                   break;
               }
            }

            // If so, time for a basin DFS
            if (is_low) {
                struct pair stack[N] = {{i, j}};
                grid_t vis = {};
                size_t stack_size = 1;
                size_t basin_size = 0;
                vis[i][j] = 1;
                while (stack_size) {
                    struct pair pop = stack[--stack_size];
                    basin_size++;
                    for (k = 0; k < N_OFFS; k++) {
                        long ni = pop.i + I_OFFS[k];
                        long nj = pop.j + J_OFFS[k];
                        if (!vis[ni][nj] && grid[ni][nj] < 9 && grid[ni][nj] > grid[pop.i][pop.j]) {
                            stack[stack_size++] = (struct pair) {ni, nj};
                            vis[ni][nj] = 1;
                        }
                    }
                }
                basin_sizes[n_basins++] = basin_size;
            }
        }
    }
    qsort(basin_sizes, n_basins, sizeof(*basin_sizes), revcmp);
    printf("%ld\n", basin_sizes[0] * basin_sizes[1] * basin_sizes[2]);

    return 0;
}

