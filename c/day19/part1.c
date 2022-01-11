#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "dhashtable/dhashtable.h"

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

struct point psub(struct point p1, struct point p2)
{
    return (struct point) {{{ .x = p1.x - p2.x, .y = p1.y - p2.y, .z = p1.z - p2.z }}};
}

struct point padd(struct point p1, struct point p2)
{
    return (struct point) {{{ .x = p1.x + p2.x, .y = p1.y + p2.y, .z = p1.z + p2.z }}};
}

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

struct rot rot_compose(struct rot r1, struct rot r2)
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

struct point transform(struct point p, struct transform t)
{
    return padd(rotate(p, t.rot), t.transl);
}

struct transform compose(struct transform t1, struct transform t2)
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

int find_n_equals_ub(const struct pointpair *ps1, size_t n1, 
                  const struct pointpair *ps2, size_t n2, 
                  size_t n_eql_target, struct spair *eqls)
{
    size_t i = 0, j = 0, k = 0;

    while (i < n1 && j < n2) {
        if (ps1[i].dist == ps2[j].dist) {
           eqls[k++] = (struct spair) { i++, j++ };
        } else if (ps1[i].dist < ps2[j].dist) {
            i++;
        } else {
            j++;
        }
    }

    return k;
}

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

#define N_MATCHES 12
#define N_MATCHING_PAIRS 66 // pairsof(12)

int scanner_matches(const struct scanner *s1, const struct scanner *s2, struct spair *eqls)
{
    return find_n_equals(s1->pairs, pairsof(s1->npoints), s2->pairs, pairsof(s2->npoints),
                         N_MATCHING_PAIRS, eqls);
}

int scanner_points_match(const struct scanner *s1, size_t pi,
                         const struct scanner *s2, size_t pj,
                         struct transform tf)
{
    struct point s1point = s1->points[pi];
    struct point s2point = s2->points[pj];
    struct point s2t = transform(s2point, tf);
    return s2t.x == s1point.x && s2t.y == s1point.y && s2t.z == s2point.z;
}

// Generic deduplication, similar to qsort's interface
size_t dedup(void *arr, size_t n, size_t size, int (*cmp)(const void *, const void *))
{
    char *iptr = arr;
    char *jptr = iptr + size;
    char *endptr = iptr + n * size;
    while (jptr < endptr) {
        if (cmp(iptr, jptr)) {
            iptr += size;
            memcpy(iptr, jptr, size);
        } else {
            jptr += size;
        }
    }
    iptr += size;
    return (iptr - (char *) arr) / size;
}

size_t qsortdedup(void *arr, size_t n, size_t size, int (*cmp)(const void *, const void *))
{
    qsort(arr, n, size, cmp);
    return dedup(arr, n, size, cmp);
}

typedef long long unsigned llu;

int ucmp(const void *p1, const void *p2)
{
    llu a = *(llu *) p1;
    llu b = *(llu *) p2;
    return (a > b) - (a < b); /* a fun trick for sure */
}

// Fill indices given by the eqls array, sort & dedup them.
// Useful for getting a list of the matching points.
void fill_eqls(const struct scanner *s1, const struct scanner *s2,
               const struct spair *eqls, size_t n_eqls,
               size_t *inds1, size_t *n1, size_t *inds2, size_t *n2)
{
    for (size_t i = 0; i < n_eqls; i += 2) {
        inds1[i] = s1->pairs[eqls[i].i].from;
        inds1[i+1] = s1->pairs[eqls[i].i].to;
        inds2[i] = s2->pairs[eqls[i].j].from;
        inds2[i+1] = s2->pairs[eqls[i].j].to;
    }

    // Sort & dedup the
    *n1 = qsortdedup(inds1, 2 * n_eqls, sizeof(inds1[0]), ucmp);
    *n2 = qsortdedup(inds2, 2 * n_eqls, sizeof(inds2[0]), ucmp);
}

size_t count_equal_match_points(const struct point *ps1, const struct point *ps2)
{
    size_t c = 0;
    for (size_t i = 0; i < N_MATCHES; i++) {
        for (size_t j = i + 1; j < N_MATCHES; j++) {
            struct point p1 = ps1[i];
            struct point p2 = ps2[j];
            if (p1.x == p2.x && p1.y == p2.y && p1.z == p2.z)
                c++;
        }
    }
    return c;
}

struct transform matching_transform(const struct scanner *s1, const struct scanner *s2,
                                    const struct spair *eqls)
{
    size_t s1_inds[N_MATCHES], s2_inds[N_MATCHES];
    size_t s1_inds_n, s2_inds_n;
    struct point s1_match_points[N_MATCHES], s2_match_points[N_MATCHES];

    // We can make some assumptions, since we know that 12 points will be matching.
    fill_eqls(s1, s2, eqls, N_MATCHING_PAIRS, s1_inds, &s1_inds_n, s2_inds, &s2_inds_n);

    printf("%d\n", s1_inds_n);
    printf("%d\n", s2_inds_n);

    assert (s1_inds_n == N_MATCHES);
    assert (s2_inds_n == N_MATCHES);

    for (size_t i = 0; i < N_MATCHES; i++) {
        s1_match_points[i] = s1->points[s1_inds[i]];
        s2_match_points[i] = s2->points[s2_inds[i]];
    }

    for (size_t rot_i = 0; rot_i < N_ROTATIONS; rot_i++) {
        // Try all 66 combinations to find the correct transformation
        struct rot rot = ROTATIONS[rot_i];
        struct point s2_rot_points[N_MATCHES];
        
        for (size_t i = 0; i < N_MATCHES; i++)
            s2_rot_points[i] = rotate(s2_match_points[i], rot);

        for (size_t i = 0; i < N_MATCHES; i++) {
            for (size_t j = i + 1; j < N_MATCHES; j++) {
                struct point transl = psub(s1_match_points[i], s2_rot_points[j]);
                struct point s2_tf_points[N_MATCHES];
                size_t neql;
                for (size_t i = 0; i < N_MATCHES; i++)
                    s2_tf_points[i] = padd(s2_rot_points[i], transl);
                
                neql = count_equal_match_points(s1_match_points, s2_tf_points);
                if (neql == 12)
                    return (struct transform) { .rot = rot, .transl = transl };
            }
        }
    }
    assert (0);
}

uint64_t encode_point(struct point p)
{
    return p.x | ((uint64_t) p.y << 16) | ((uint64_t) p.z << 32);
}

struct point decode_point(uint64_t e)
{
    return (struct point) {{{ .x = e & 0xFFFF, .y = (e >> 16) & 0xFFFF, .z = e >> 24 }}};
}

void search(const struct scanner *scanners, size_t n_scanners,
            char scanner_visited[], dhashtable_t *points_found,
            size_t i, struct transform tf2first)
{
    size_t j;
    const struct scanner *cursc = &scanners[i];

    // Mark visited and insert points
    scanner_visited[i] = 1;
    for (j = 0; j < cursc->npoints; j++) {
        struct point tfp = transform(cursc->points[j], tf2first);
        dhashtable_insert(points_found, encode_point(tfp), 0);
    }

    for (j = 0; j < n_scanners; j++) {
        struct spair eqls[N_MATCHING_PAIRS];
        if (!scanner_visited[j] && scanner_matches(&scanners[i], &scanners[j], eqls)) {
            printf("%d %d\n", i, j);
            // Now, need to match the equals somehow...
            struct transform tf = matching_transform(&scanners[i], &scanners[j], eqls);
            struct transform total_tf = compose(tf2first, tf);
            search(scanners, n_scanners, scanner_visited, points_found, j, total_tf);
        }
    }
}

int unique_points(const struct scanner *scanners, size_t n_scanners)
{
    char *scanner_visited = calloc(n_scanners, sizeof(scanner_visited[0]));
    dhashtable_t points_found;
    size_t n;
    dhashtable_init(&points_found);
    search(scanners, n_scanners, scanner_visited, &points_found, 0, IDENT_TF);
    n = points_found.num_entries;
    free(scanner_visited);
    dhashtable_destroy(&points_found);
    return n;
}

int main(void)
{
    generate_rotations();
//     
//     for (int i = 0; i < N_ROTATIONS; i++) {
//         for (int j = 0; j < N_ROTATIONS; j++) {
//             print_rotation(ROTATIONS[i]);
//             print_rotation(ROTATIONS[j]);
//             print_rotation(rot_compose(ROTATIONS[i], ROTATIONS[j]));
//             putchar('\n');;
//         }
//     }
//     

    static struct scanner scanners[MAX_SCANNERS];
    size_t n = 0;
    while (read_scanner(&scanners[n])) n++;

    printf("%d\n", unique_points(scanners, n));

    return 0;
}

