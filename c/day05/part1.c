#include <stdio.h>
#include <stdint.h>
#include <assert.h>

#define MAX_SEGS 1024
#define MAX_COORD 1024

struct point {
    int x, y;
}; 

struct seg {
    struct point s, e;
};

int read_seg(struct seg *s)
{
    int ret = scanf("%d,%d -> %d,%d", &s->s.x, &s->s.y, &s->e.x, &s->e.y);
    assert (s->s.x < MAX_COORD && s->s.y < MAX_COORD && s->e.x < MAX_COORD && s->e.y < MAX_COORD);
    return ret != EOF;
}

size_t read_segs(struct seg segs[])
{
    size_t i = 0;
    while (read_seg(&segs[i])) {
        i++;
        assert (i < MAX_SEGS);
    }
    return i;
}

typedef char grid_t[MAX_COORD][MAX_COORD];

void mark_segment(const struct seg *seg, grid_t grid)
{
    int sx = seg->s.x, sy = seg->s.y, ex = seg->e.x, ey = seg->e.y;
    long c;
    if (sx == ex) {
        long incr = sy < ey ? 1 : -1;
        for (c = sy; c != ey; c += incr)
            grid[c][sx]++;
        grid[c][sx]++;
    } else if (sy == ey) {
        long incr = sx < ex ? 1 : -1;
        for (c = sx; c != ex; c += incr)
            grid[sy][c]++;
        grid[sy][c]++;
    } 
}

int main(void)
{
    struct seg segs[MAX_SEGS];
    size_t n_segs = read_segs(segs);
    static grid_t grid = {};
    size_t i, j, cnt;

    for (i = 0; i < n_segs; i++)
        mark_segment(&segs[i], grid);

    cnt = 0;
    for (i = 0; i < MAX_COORD; i++)
        for (j = 0; j < MAX_COORD; j++)
            if (grid[i][j] > 1)
                cnt++;
    printf("%lu\n", cnt);

    return 0;
}
