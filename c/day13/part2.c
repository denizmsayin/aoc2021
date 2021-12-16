#include <stdio.h>
#include <stdlib.h>

#define MAX_POINTS 8192
#define MAX_FOLDS 128

struct point {
    int x, y;
};

struct axis {
    int along_x, pos;
};

void fold_along(struct point pts[], size_t n, struct axis axis)
{
    size_t i;
    for (i = 0; i < n; i++) {
        int *p = axis.along_x ? &pts[i].x : &pts[i].y;
        int diff = *p - axis.pos;
        if (diff > 0)
            *p = *p - 2 * diff;
    }
}

int cmp(int x, int y)
{
    if (x > y)
        return 1;
    else if (x == y)
        return 0;
    return -1;
}

int ptcmp(const void *pp1, const void *pp2)
{
    const struct point *p1 = pp1, *p2 = pp2;
    int l = cmp(p1->y, p2->y);
    if (l != 0)
        return l;
    return cmp(p1->x, p2->x);
}

size_t dedup(struct point pts[], size_t n)
{
    size_t i = 0, j = 1;
    while (j < n) {
        if (ptcmp(&pts[i], &pts[j])) {
            i++;
            pts[i] = pts[j];
        } else {
            j++;
        }
    }
    i++;
    return i;
}

void visualize(const struct point pts[], size_t n)
{
    int max_y = pts[n-1].y;
    int max_x = 0;
    int x, y;
    size_t i;
    for (i = 0; i < n; i++)
        if (pts[i].x > max_x)
            max_x = pts[i].x;

    i = 0;
    for (y = 0; y <= max_y; y++) {
        for (x = 0; x <= max_x; x++) {
            if (i < n && y == pts[i].y && x == pts[i].x) {
                putchar('#');
                i++;
            } else {
                putchar('.');
            }
        }
        putchar('\n');
    }
}

int main(void)
{
    struct point pts[MAX_POINTS];
    struct axis fold;
    size_t n_points = 0;
    char c;

    while (scanf(" %d,%d", &pts[n_points].x, &pts[n_points].y) > 0)
        n_points++;

    while (scanf(" fold along %c=%d", &c, &fold.pos) != EOF) {
        fold.along_x = c == 'x';
        fold_along(pts, n_points, fold);
    }

    qsort(pts, n_points, sizeof(*pts), ptcmp);
    n_points = dedup(pts, n_points);
    
    visualize(pts, n_points);

    return 0;
}
