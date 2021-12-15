#include <stdio.h>
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

int main(void)
{
    static grid_t grid;
    long rows, cols;
    long i, j, total_risk;
    size_t k;

    read_sentinel_grid(grid, &rows, &cols);

    total_risk = 0;
    for (i = 1; i <= rows; i++) {
        for (j = 1; j <= cols; j++) {
            int is_low = 1;
            for (k = 0; k < N_OFFS; k++) {
               long ni = i + I_OFFS[k];
               long nj = j + J_OFFS[k];
               if (grid[ni][nj] <= grid[i][j]) {
                   is_low = 0;
                   break;
               }
            }
            if (is_low)
                total_risk += grid[i][j] + 1;
        }
    }
    printf("%ld\n", total_risk);

    return 0;
}

