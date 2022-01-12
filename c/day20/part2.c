#include <stdio.h>
#include <string.h>
#include <assert.h>

#define N_STEPS 50
#define HW_MAX  256
#define ALGO_SZ 512
#define BUF_SZ  1024

typedef unsigned char ubyte;
typedef ubyte algo_t[ALGO_SZ];
typedef long long lli;

struct img {
    int outside;
    lli nrows, ncols;
    ubyte grid[HW_MAX][HW_MAX];
};

void read_algo(algo_t algo)
{
    size_t i;
    char linebuf[BUF_SZ];
    assert (fgets(linebuf, BUF_SZ, stdin));
    for (i = 0; linebuf[i] != '\n' && linebuf[i] != 0; i++)
        algo[i] = linebuf[i] == '#' ? 1 : 0;
    assert (i == ALGO_SZ);
}

void read_img(struct img *img)
{
    lli i = 0, j = 0;
    char linebuf[BUF_SZ];
    while (fgets(linebuf, BUF_SZ, stdin)) {
        for (j = 0; linebuf[j] != '\n' && linebuf[j] != 0; j++)
            img->grid[i][j] = linebuf[j] == '#' ? 1 : 0;
        i++;
    }
    img->nrows = i;
    img->ncols = j;
    img->outside = 0;
}

void print_img(const struct img *img)
{
    for (lli i = 0; i < img->nrows; i++) {
        for (lli j = 0; j < img->ncols; j++)
            putchar(img->grid[i][j] ? '#' : '.');
        putchar('\n');
    }
}

ubyte pixat(const struct img *img, lli i, lli j)
{
    if (0 <= i && i < img->nrows && 0 <= j && j < img->ncols)
        return img->grid[i][j];
    return img->outside;
}

unsigned decode_around(const struct img *img, lli i, lli j)
{
    unsigned value = 0;
    for (lli ioff = -1; ioff <= 1; ioff++)
        for (lli joff = -1; joff <= 1; joff++)
            value = (value << 1) + pixat(img, i + ioff, j + joff);
    assert (value < 512);
    return value;
}

void step_img(struct img *in, struct img *out, const algo_t algo)
{
    memset(out, 0, sizeof(*out));
    for (lli i = -1; i <= in->nrows; i++)
        for (lli j = -1; j <= in->ncols; j++)
            out->grid[i+1][j+1] = algo[decode_around(in, i, j)];
    out->outside = in->outside ? algo[511] : algo[0];
    out->nrows = in->nrows + 2;
    out->ncols = in->ncols + 2;
    assert (out->nrows < HW_MAX);
    assert (out->ncols < HW_MAX);
}

unsigned count_lit(const struct img *img)
{
    unsigned count = 0;
    for (lli i = 0; i < img->nrows; i++)
        for (lli j = 0; j < img->ncols; j++)
            if (img->grid[i][j])
                count++;
    return count;
}

int main(void)
{
    algo_t algo;
    struct img img1, img2, *cur, *next;

    read_algo(algo);
    assert (scanf("%*[^\n]") >= 0);
    read_img(&img1);

    cur = &img1;
    next = &img2;
    for (size_t c = 0; c < N_STEPS; c++) {
        struct img *tmp = cur;
        step_img(cur, next, algo);
        cur = next;
        next = tmp;
    }

    printf("%u\n", count_lit(cur));

    return 0;
}
