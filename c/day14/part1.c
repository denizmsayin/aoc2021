#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#define MAX_POLY_SIZE 65536

/* Polymer values: B, C, F, H, K, N, O, P, S, V */
/* Encode for sim: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 */

typedef unsigned char u8;
typedef unsigned long long u64;

enum molecule {
    B, C, F, H, K, N, O, P, S, V, NO_MOLECULE
};

#define NUM_MOLECULES ((u64) NO_MOLECULE)

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

void read_poly(u8 *poly)
{
    int c;
    while ((c = getchar()) != '\n')
        *poly++ = encode(c);
    *poly = NO_MOLECULE;
}

void step(u8 *cur, u8 *next)
{
    size_t i;
    for (i = 0; cur[i+1] != NO_MOLECULE; i++) {
        u8 f = cur[i], s = cur[i+1];
        u8 rule = TEMPLATE[f][s];
        *next++ = f;
        if (rule != NO_MOLECULE)
            *next++ = rule;
    } 
    *next++ = cur[i];
    *next = NO_MOLECULE;
}

u64 maxmindiff(u8 *poly)
{
    u64 counts[NUM_MOLECULES] = {0};
    u64 max, min;
    size_t i;

    while (*poly != NO_MOLECULE)
        counts[*poly++]++;

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

int main(void)
{
    int steps = 10;
    static u8 poly[MAX_POLY_SIZE], next_poly[MAX_POLY_SIZE];
    u8 *cur = poly, *next = next_poly;

    read_poly(poly);
    read_template();

    while (steps--) {
        u8 *tmp = cur;
        step(cur, next);
        cur = next;
        next = tmp;
    }

    printf("%llu\n", maxmindiff(cur));    

    return 0;
}
