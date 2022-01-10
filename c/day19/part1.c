#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#define MAX_SCANNERS 128
#define MAX_POINTS_PER_SCANNER 128
#define MAX_PAIRS_PER_SCANNER ((MAX_POINTS_PER_SCANNER * (MAX_POINTS_PER_SCANNER + 1))/2)

typedef long long int lli;

struct rot {
    struct {
        unsigned char i, neg;
    } comp[3];
};

static inline lli pairsof(lli n) { return (n * (n - 1)) / 2; }

#define NROTATIONS 24

static struct rot ROTATIONS[NROTATIONS];

struct point {
    union {
        short x, y, z;
        short coords[3];
    };
};

int rotposdet(struct rot r)
{
    int sub_ori_neg = r.comp[1].i > r.comp[2].i; // Sub-det sign
    int first_row_neg = r.comp[0].i == 1; // negated only if first row [0 1 0]
    int mul_neg = r.comp[0].neg ^ r.comp[1].neg ^ r.comp[2].neg;
    return !(sub_ori_neg ^ first_row_neg ^ mul_neg);
}

void generate_rotations(void)
{
    // Enough recursion with Haskell!
    static int PERMS[][3] = {{0, 1, 2}, {0, 2, 1}, {1, 0, 2}, {1, 2, 0}, {2, 0, 1}, {2, 1, 0}};
    int k = 0;
    for (unsigned i = 0; i < sizeof(PERMS) / sizeof(PERMS[0]); i++) {
        for (int fneg = 0; fneg <= 1; fneg++) {
            for (int sneg = 0; sneg <= 1; sneg++) {
                for (int tneg = 0; tneg <= 1; tneg++) {
                    struct rot r = {
                        {{ PERMS[i][0], fneg },
                         { PERMS[i][1], sneg },
                         { PERMS[i][2], tneg }}
                    };
                    if (rotposdet(r))
                        ROTATIONS[k++] = r;
                }
            }
        }
    }
    assert (k == NROTATIONS);
}

void print_rotation(struct rot r)
{
    // Matrix-style:
    // [[1 0 0]
    //  [0 1 0]
    //  [0 0 1]]
    for (int i = 0; i < 3; i++) {
        if (i == 0)
            putchar('[');
        else
            putchar(' ');
        putchar('[');
        for (int j = 0; j < 3; j++) {
            if (r.comp[i].i == j)
                printf("%2s", r.comp[i].neg ? "-1" : "1");
            else
                printf(" 0");
        }
        putchar(']');
        if (i == 2)
            putchar(']');
        putchar('\n');
    }
}

struct point rotate(struct point p, struct rot r)
{
    struct point q;
    for (int i = 0; i < 3; i++) {
        q.coords[i] = p.coords[r.comp[i].i];
        if (r.comp[i].neg)
            q.coords[i] = -q.coords[i];
    }
    return q;
}

struct pointpair {
    int dist;
    char from, to;
};

int cmp_short(short x, short y)
{
    if (x < y)
        return -1;
    else if (x == y)
        return 0;
    else
        return 1;
}

int cmp_pair(const void *pp1, const void *pp2)
{
    const struct pointpair *p1 = pp1, *p2 = pp2;
    return cmp_short(p1->dist, p2->dist);
}

struct scanner {
    size_t npoints;
    struct point points[MAX_POINTS_PER_SCANNER];
    struct pointpair pairs[MAX_PAIRS_PER_SCANNER];
};

int manhattan(const struct point p1, const struct point p2)
{
    return abs(p1.x - p2.x) + abs(p1.y - p2.y) + abs(p1.z - p2.z);
}

int read_scanner(struct scanner *s)
{
    int ret = scanf("%*[^\n]");
    size_t i, j, k;
    struct point *pts = s->points;

    if (ret == EOF)
        return 0;

    i = 0;
    while (scanf(" %hd,%hd,%hd", &pts[i].x, &pts[i].y, &pts[i].z) == 3)
        i++;
    s->npoints = i;

    // generate pairs between points
    k = 0;
    for (i = 0; i < s->npoints; i++) {
        for (j = i + 1; j < s->npoints; j++) {
            struct pointpair *p = &s->pairs[k++];
            p->dist = manhattan(pts[i], pts[j]);
            p->from = i;
            p->to = j;
        }
    }

    assert ((lli) k == pairsof(s->npoints));
    
    qsort(s->pairs, k, sizeof(s->pairs[0]), cmp_pair);

    return 1;
}

void print_scanner(const struct scanner *s)
{
    size_t i, n = s->npoints;
    printf("N=%lu\n", n);
    for (i = 0; i < n; i++) {
        struct point p = s->points[i];
        printf("%hd,%hd,%hd\n", p.x, p.y, p.z);
    }
    puts("Pairs:");
    n = pairsof(n);
    for (i = 0; i < n; i++) {
        struct pointpair p = s->pairs[i];
        printf("%d = %hhd->%hhd\n", p.dist, p.from, p.to);
    }
}

struct spair {
    size_t i, j;
};

int find_n_equals(const struct pointpair *ps1, size_t n1, 
                  const struct pointpair *ps2, size_t n2, 
                  size_t n_eql_target, struct spair *eqls)
{
    size_t i = 0, j = 0, k = 0;

    while (i < n1 && j < n2) {
        if (ps1[i].dist == ps2[j].dist) {
           eqls[k++] = (struct spair) { i++, j++ };
           if (k == n_eql_target)
               return 1;
        } else if (ps1[i].dist < ps2[j].dist) {
            i++;
        } else {
            j++;
        }
    }

    return 0;
}

int main(void)
{
    generate_rotations();
    for (int i = 0; i < NROTATIONS; i++) {
        print_rotation(ROTATIONS[i]);
    }
    return 0;
    static struct scanner scanners[MAX_SCANNERS];
    size_t i, j, n = 0;
    while (read_scanner(&scanners[n])) n++;

    for (i = 0; i < n; i++) {
        for (j = i + 1; j < n; j++) {
            struct spair eqls[66];
            if (find_n_equals(scanners[i].pairs, pairsof(scanners[i].npoints),
                              scanners[j].pairs, pairsof(scanners[j].npoints),
                              66, eqls))
            {
                printf("%lu - %lu match!\n", i, j);
                size_t k;
                for (k = 0; k < 66; k++) {
                    const struct pointpair ipair = scanners[i].pairs[eqls[k].i],
                                           jpair = scanners[j].pairs[eqls[k].j];
                    printf("%hhd<->%hhd == %hhd<->%hhd\n", 
                           ipair.from, ipair.to, jpair.from, jpair.to);
                }
            }
        }
    }

    return 0;
}

