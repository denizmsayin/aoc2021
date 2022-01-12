#include <stdio.h>
#include <string.h>
#include <assert.h>

#define N 256

struct pos {
    short i, j;
};

typedef char grid_t[N][N];

static inline int incrmod(int i, int m) 
{ 
    int incr = i + 1;
    return incr == m ? 0 : incr;
}

static inline int max(int a, int b) { return a > b ? a : b; }

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

// If we store positions of > and v in an auxiliary data
// structure, it will be quite a bit faster than scanning
// through all the cells. That should give me the edge I need!
// EDIT: Brought down from ~50ms to ~15ms. Win!
void scan_positions(const grid_t grid, int n, int m, 
                    struct pos rpos[], int *rc,
                    struct pos dpos[], int *dc)
{
    int k0 = 0, k1 = 0;
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < m; j++) {
            if (grid[i][j] == '>')
                rpos[k0++] = (struct pos) { .i = i, .j = j };
            else if (grid[i][j] == 'v')
                dpos[k1++] = (struct pos) { .i = i, .j = j };
        }
    }
    *rc = k0;
    *dc = k1;
}

int step(grid_t grid, int n, int m, struct pos rpos[], int rc, struct pos dpos[], int dc,
         int av[])
{
    size_t av_size = max(dc, rc) * sizeof(av[0]);
    int chg = 0, av_i;


    memset(av, 0, av_size);
    av_i = 0;
    for (int k = 0; k < rc; k++) {
        int i = rpos[k].i;
        int j = rpos[k].j;
        int nj = incrmod(j, m);
        if (grid[i][nj] == '.')
            av[av_i++] = k;
    }

    if (av_i > 0)
        chg = 1;

    for (int k = 0; k < av_i; k++) {
        int t = av[k];
        int i = rpos[t].i;
        int j = rpos[t].j;
        int nj = incrmod(j, m);
        grid[i][j] = '.';
        grid[i][nj] = '>';
        rpos[t].j = nj;
    }

    memset(av, 0, av_size);
    av_i = 0;
    for (int k = 0; k < dc; k++) {
        int i = dpos[k].i;
        int j = dpos[k].j;
        int ni = incrmod(i, n);
        if (grid[ni][j] == '.')
            av[av_i++] = k;
    }

    if (av_i > 0)
        chg = 1;

    for (int k = 0; k < av_i; k++) {
        int t = av[k];
        int i = dpos[t].i;
        int j = dpos[t].j;
        int ni = incrmod(i, n);
        grid[i][j] = '.';
        grid[ni][j] = 'v';
        dpos[t].i = ni;
    }

    return chg;
}

void print_grid(grid_t grid, int n)
{
    for (int i = 0; i < n; i++)
        puts(grid[i]);
}

int main(void)
{
    static grid_t grid;
    static struct pos rpos[N*N], dpos[N*N];
    static int av[N*N];
    int n, m, rc, dc;
    int c = 1;

    read_grid(grid, &n, &m);
    scan_positions(grid, n, m, rpos, &rc, dpos, &dc);
    while (step(grid, n, m, rpos, rc, dpos, dc, av))
        c++;

    printf("%d\n", c);

    return 0;
}
