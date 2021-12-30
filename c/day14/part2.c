#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

/* Polymer values: B, C, F, H, K, N, O, P, S, V */
/* Encode for sim: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 */

typedef unsigned char u8;
typedef unsigned long long u64;

enum molecule {
    B, C, F, H, K, N, O, P, S, V, NO_MOLECULE
};

#define NUM_MOLECULES ((u64) NO_MOLECULE)

typedef u64 pairctr_t[NUM_MOLECULES][NUM_MOLECULES];

u8 TEMPLATE[NUM_MOLECULES][NUM_MOLECULES];

enum molecule encode(char p)
{
    switch (p) {
        case 'B': return B;
        case 'C': return C;
        case 'F': return F;
        case 'H': return H;
        case 'K': return K;
        case 'N': return N;
        case 'O': return O;
        case 'P': return P;
        case 'S': return S;
        case 'V': return V;
        default:  assert (0);
    }
}

void read_template(void) 
{
    size_t i, j;
    char f, s, r;

    for (i = 0; i < NUM_MOLECULES; i++)
        for (j = 0; j < NUM_MOLECULES; j++)
            TEMPLATE[i][j] = NO_MOLECULE;

    while (scanf(" %c%c -> %c", &f, &s, &r) > 0)
        TEMPLATE[encode(f)][encode(s)] = encode(r);
}

void read_poly(pairctr_t ctr)
{
    int c, prev_c;
    prev_c = encode(getchar());
    while ((c = getchar()) != '\n') {
        c = encode(c);
        ctr[prev_c][c]++;
        prev_c = c;
    }
}

void step(const pairctr_t *cur, pairctr_t *next)
{
    size_t i, j;
    memset(*next, 0, sizeof(*next));
    for (i = 0; i < NUM_MOLECULES; i++) {
        for (j = 0; j < NUM_MOLECULES; j++) {
            u64 count = (*cur)[i][j];
            if (count > 0) {
                u8 rule = TEMPLATE[i][j];
                if (rule == NO_MOLECULE) {
                    (*next)[i][j] += count;
                } else {
                    (*next)[i][rule] += count;
                    (*next)[rule][j] += count;
                }
            }
        }
    }
}

u64 maxmindiff(const pairctr_t *poly)
{
    u64 f_counts[NUM_MOLECULES] = {0}, 
        s_counts[NUM_MOLECULES] = {0}, 
        counts[NUM_MOLECULES] = {0};
    u64 max, min;
    size_t i, j;

    for (i = 0; i < NUM_MOLECULES; i++) {
        for (j = 0; j < NUM_MOLECULES; j++) {
            f_counts[i] += (*poly)[i][j];
            s_counts[j] += (*poly)[i][j];
        }
    }

    for (i = 0; i < NUM_MOLECULES; i++)
        counts[i] = f_counts[i] > s_counts[i] ? f_counts[i] : s_counts[i];

    max = 0ULL;
    min = ~0ULL;
    for (i = 0; i < NUM_MOLECULES; i++) {
        if (counts[i] > max)
            max = counts[i];
        else if (counts[i] > 0 && counts[i] < min)
            min = counts[i];
    }

    return max - min;
}

int main(int argc, char *argv[])
{
    int steps = 40;

    if (argc >= 2)
        steps = atoi(argv[1]);

    static pairctr_t c0 = {0}, c1 = {0};
    pairctr_t *cur = &c0, *next = &c1;

    read_poly(c0);
    read_template();

    while (steps--) {
        pairctr_t *tmp = cur;
        step(cur, next);
        cur = next;
        next = tmp;
    }

    printf("%llu\n", maxmindiff(cur));    

    return 0;
}
