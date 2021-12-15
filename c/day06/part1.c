#include <stdio.h>
#include <stdlib.h>

#define MAX_FISH (1UL << 24) // ~16M
#define NSTEPS 80

size_t read_sep_vals(char buf[])
{
    char *itr = buf;
    scanf("%hhd", itr++);
    while (scanf(",%hhd", itr) > 0) 
        itr++;
    return itr - buf;
}

int main(void)
{
    static char fishies[MAX_FISH];
    size_t n_fish = read_sep_vals(fishies);    
    size_t step;

    for (step = 0; step < NSTEPS; step++) {
        size_t i, end = n_fish;
        for (i = 0; i < n_fish; i++) {
            if (fishies[i] > 0) {
                fishies[i]--;
            } else {
                fishies[i] = 6;
                fishies[end++] = 8;
            }
        }
        n_fish = end;
    }
    printf("%lu\n", n_fish);

    return 0;
}

