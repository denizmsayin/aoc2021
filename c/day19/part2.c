#include <stdio.h>
#include <stdlib.h>
#include <string.h>
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

#define IDENT_ROT ((struct rot) {{{ .i = 0, .neg = 0}, { .i = 1, .neg = 0}, { .i = 2, .neg = 0}}})

#define N_ROTATIONS 24

static struct rot ROTATIONS[N_ROTATIONS];

struct point {
    union {
        struct { 
            short x, y, z;
        };
        short coords[3];
    };
};

#define IDENT_TRANSL ((struct point) {{{ .x = 0, .y = 0, .z = 0 }}})

static struct point psub(struct point p1, struct point p2)
{
    return (struct point) {{{ .x = p1.x - p2.x, .y = p1.y - p2.y, .z = p1.z - p2.z }}};
}

static struct point padd(struct point p1, struct point p2)
{
    return (struct point) {{{ .x = p1.x + p2.x, .y = p1.y + p2.y, .z = p1.z + p2.z }}};
}

static int rotposdet(struct rot r)
{
    int sub_ori_neg = r.comp[1].i > r.comp[2].i; // Sub-det sign
    int first_row_neg = r.comp[0].i == 1; // negated only if first row [0 1 0]
    int mul_neg = r.comp[0].neg ^ r.comp[1].neg ^ r.comp[2].neg;
    return !(sub_ori_neg ^ first_row_neg ^ mul_neg);
}

static void generate_rotations(void)
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
    assert (k == N_ROTATIONS);
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

static struct point rotate(struct point p, struct rot r)
{
    struct point q;
    for (int i = 0; i < 3; i++) {
        q.coords[i] = p.coords[r.comp[i].i];
        if (r.comp[i].neg)
            q.coords[i] = -q.coords[i];
    }
    return q;
}

static struct rot rot_compose(struct rot r1, struct rot r2)
{
    struct rot c;
    for (size_t i = 0; i < 3; i++) {
        size_t j = r1.comp[i].i;
        c.comp[i].i = r2.comp[j].i;
        c.comp[i].neg = r1.comp[i].neg ^ r2.comp[j].neg;
    }
    return c;
}

struct transform {
    struct rot rot;
    struct point transl;
};

#define IDENT_TF ((struct transform) { .rot = IDENT_ROT, .transl = IDENT_TRANSL })

static struct transform compose(struct transform t1, struct transform t2)
{
    return (struct transform) {
        .rot = rot_compose(t1.rot, t2.rot),
        .transl = padd(rotate(t2.transl, t1.rot), t1.transl)
    };
}

struct pointpair {
    int dist;
    unsigned char from, to;
};

typedef long long unsigned llu;

static int hcmp(short x, short y)
{
    return (int) x - (int) y;
}

static int paircmp(const void *pp1, const void *pp2)
{
    const struct pointpair *p1 = pp1, *p2 = pp2;
    return hcmp(p1->dist, p2->dist);
}

static int pointcmp_v(struct point p1, struct point p2)
{
    int r = hcmp(p1.x, p2.x);
    if (r)
        return r;
    r = hcmp(p1.y, p2.y);
    if (r)
        return r;
    return hcmp(p1.z, p2.z);
}

static int pointcmp(const void *pp1, const void *pp2)
{
    return pointcmp_v(*(struct point *) pp1, *(struct point *) pp2);
}

struct scanner {
    size_t npoints;
    struct point points[MAX_POINTS_PER_SCANNER];
    struct pointpair pairs[MAX_PAIRS_PER_SCANNER];
};

static int manhattan(const struct point p1, const struct point p2)
{
    return abs(p1.x - p2.x) + abs(p1.y - p2.y) + abs(p1.z - p2.z);
}

static int read_scanner(struct scanner *s)
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

    qsort(s->pairs, k, sizeof(s->pairs[0]), paircmp);

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

static int find_n_equals(const struct pointpair *ps1, size_t n1, 
                  const struct pointpair *ps2, size_t n2, 
                  size_t n_eql_target)
{
    size_t i = 0, j = 0, k = 0;

    while (i < n1 && j < n2) {
        if (ps1[i].dist == ps2[j].dist) {
            k++;
            if (k == n_eql_target)
                return 1;
            i++;
            j++;
        } else if (ps1[i].dist < ps2[j].dist) {
            i++;
        } else {
            j++;
        }
    }

    return 0;
}

#define N_MATCHES 12
#define N_MATCHING_PAIRS 66 // pairsof(12)

static int scanner_matches(const struct scanner *s1, const struct scanner *s2)
{
    return find_n_equals(s1->pairs, pairsof(s1->npoints), s2->pairs, pairsof(s2->npoints),
                          N_MATCHING_PAIRS);
}

static size_t sort_find_longest_eq_subseq(struct point *ps, size_t n, size_t *eq_start)
{
    size_t max_count = 0;
    size_t i = 0;

    qsort(ps, n, sizeof(ps[0]), pointcmp);

    while (i < n) {
        size_t count;
        size_t j = i + 1;
        while (j < n && pointcmp_v(ps[i], ps[j]) == 0)
            j++;
        count = j - i;
        if (count > max_count) {
            max_count = count;
            *eq_start = i;
        }
        i = j;
    } 

    return max_count;
}

static struct transform matching_transform(const struct scanner *s1, const struct scanner *s2)
{
    struct point s1_sorted_points[MAX_POINTS_PER_SCANNER];
    memcpy(s1_sorted_points, s1->points, s1->npoints * sizeof(s1->points[0]));
    qsort(s1_sorted_points, s1->npoints, sizeof(s1->points[0]), pointcmp);

    for (size_t rot_i = 0; rot_i < N_ROTATIONS; rot_i++) {
        // Try all 66 combinations to find the correct transformation
        struct rot rot = ROTATIONS[rot_i];
        struct point s2_rot_points[MAX_POINTS_PER_SCANNER];
        struct point transls[MAX_POINTS_PER_SCANNER * MAX_POINTS_PER_SCANNER];
        size_t k = 0;
        size_t eq_count, eq_start;

        for (size_t i = 0; i < s2->npoints; i++)
            s2_rot_points[i] = rotate(s2->points[i], rot);

        // Now, list the translation vector between all cross-pairs.
        // 12 of them should be equal if this is the correct rotation.
        // For relatively fast equality checking, qsort & count.
        for (size_t i = 0; i < s1->npoints; i++)
            for (size_t j = 0; j < s2->npoints; j++)
                transls[k++] = psub(s1_sorted_points[i], s2_rot_points[j]);

        eq_count = sort_find_longest_eq_subseq(transls, k, &eq_start);
        if (eq_count >= 12)
            return (struct transform) { .rot = rot, .transl = transls[eq_start] };
    }

    assert (0);
}

static size_t search(const struct scanner scanners[], size_t n_scanners,
                     char scanner_visited[], struct point scanners_found[],
                     size_t i, struct transform tf2first)
{
    size_t j, k = 0;

    // Mark visited and insert points
    scanner_visited[i] = 1;
    scanners_found[k++] = tf2first.transl;

    for (j = 0; j < n_scanners; j++) {
        if (!scanner_visited[j] && scanner_matches(&scanners[i], &scanners[j])) {
            struct transform tf = matching_transform(&scanners[i], &scanners[j]);
            struct transform total_tf = compose(tf2first, tf);
            // The transform's translation component is the position of the scanner
            // w.r.t. scanner 0. Drawing a diagram on paper shows this... Neat!
            k += search(scanners, n_scanners, scanner_visited, 
                        scanners_found + k, j, total_tf);
        }
    }

    return k;
}

static llu furthest_scanners_dist(const struct scanner *scanners, size_t n_scanners)
{
    char scanner_visited[MAX_SCANNERS] = {0};
    struct point scanners_found[MAX_SCANNERS];
    llu max_dist = 0;
    search(scanners, n_scanners, scanner_visited, scanners_found, 0, IDENT_TF);

    for (size_t i = 0; i < n_scanners; i++) {
        for (size_t j = i + 1; j < n_scanners; j++) {
            llu dist = manhattan(scanners_found[i], scanners_found[j]);
            if (dist > max_dist)
                max_dist = dist;
        }
    }

    return max_dist;
}

int main(void)
{
    static struct scanner scanners[MAX_SCANNERS];
    size_t n = 0;
    
    generate_rotations();
    
    while (read_scanner(&scanners[n])) n++;
    
    printf("%llu\n", furthest_scanners_dist(scanners, n));

    return 0;
}

