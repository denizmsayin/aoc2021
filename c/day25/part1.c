#include <stdio.h>
#include <string.h>

#define N 4096

typedef char grid_t[N][N];

static inline int incrmod(int i, int m) 
{ 
    int incr = i + 1;
    return incr == m ? 0 : incr;
}

void read_grid(grid_t grid, int *n, int *m)
{
    int i = 0;
    while (fgets(grid[i], N, stdin)) {
        grid[i][strcspn(grid[i], "\n")] = 0;
        i++;
    }
    *n = i;
    *m = strlen(grid[0]);
}

int step(grid_t grid, int n, int m)
{
    int changed = 0;

    // Can actually move while checking, but need a special
    // case for the first character in a row/column. It might
    // have moved by the time we check it for the last character.
    // So, check that one first.
    for (int i = 0; i < n; i++) {
        int j, first_available = grid[i][0] == '.';
        for (j = 0; j < m - 1; j++) {
            int nj = j + 1;
            if (grid[i][j] == '>' && grid[i][nj] == '.') {
                grid[i][j] = '.';
                grid[i][nj] = '>';
                j++;
                changed = 1;
            }
        }
        if (j != m && first_available && grid[i][m-1] == '>') {
            grid[i][m-1] = '.';
            grid[i][0] = '>';
        }
    }
    for (int j = 0; j < m; j++) {
        int i, first_available = grid[0][j] == '.';
        for (i = 0; i < n - 1; i++) {
            int ni = i + 1;
            if (grid[i][j] == 'v' && grid[ni][j] == '.') {
                grid[i][j] = '.';
                grid[ni][j] = 'v';
                i++;
                changed = 1;
            }
        }
        if (i != n && first_available && grid[n-1][j] == 'v') {
            grid[n-1][j] = '.';
            grid[0][j] = 'v';
        }
    }
    return changed;
}

void print_grid(grid_t grid, int n)
{
    for (int i = 0; i < n; i++)
        puts(grid[i]);
}

int main(void)
{
    static grid_t grid;
    int n, m;
    int c = 1;

    read_grid(grid, &n, &m);
    while (step(grid, n, m))
        c++;

    printf("%d\n", c);

    return 0;
}
