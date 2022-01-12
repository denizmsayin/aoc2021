#include <stdio.h>

typedef unsigned char ubyte;

struct range {
    int s, e;
};

static inline struct range clamp_range(struct range r, int min, int max)
{
    return (struct range) {
        .s = r.s <= min ? min : r.s,
        .e = r.e >= max ? max : r.e
    };
}

int main(void)
{
    char buf[4];
    ubyte grid[101][101][101] = {}; 
    struct range xr, yr, zr;
    int count = 0;

    while (scanf(" %3s x=%d..%d,y=%d..%d,z=%d..%d", buf, 
                 &xr.s, &xr.e, &yr.s, &yr.e, &zr.s, &zr.e) == 7) 
    {
        ubyte on = buf[0] == 'o' && buf[1] == 'n';
        xr = clamp_range(xr, -50, 50);
        yr = clamp_range(yr, -50, 50);
        zr = clamp_range(zr, -50, 50);
        for (int x = xr.s; x <= xr.e; x++)
            for (int y = yr.s; y <= yr.e; y++)
                for (int z = zr.s; z <= zr.e; z++)
                    grid[x+50][y+50][z+50] = on;
    }

    for (int i = 0; i < 101; i++)
        for (int j = 0; j < 101; j++)
            for (int k = 0; k < 101; k++)
                count += grid[i][j][k];

    printf("%d\n", count);

    return 0;
}
