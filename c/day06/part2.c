#include <stdio.h>
#include <stdlib.h>

#define MAX_FISH 1024
#define NSTEPS 256

size_t read_sep_vals(unsigned char buf[])
{
    unsigned char *itr = buf;
    scanf("%hhd", itr++);
    while (scanf(",%hhd", itr) > 0) 
        itr++;
    return itr - buf;
}

int main(void)
{
    static unsigned char fishies[MAX_FISH];
    size_t n_fish = read_sep_vals(fishies);    
    size_t i, step;
    long sum, fish_counters[9] = {};

    for (i = 0; i < n_fish; i++)
        fish_counters[fishies[i]]++;

    for (step = 0; step < NSTEPS; step++) {
        long tmp = fish_counters[0];
        for (i = 0; i < 8; i++)
            fish_counters[i] = fish_counters[i+1];
        fish_counters[8] = tmp;
        fish_counters[6] += tmp;
    }

    sum = 0;
    for (i = 0; i < 9; i++)
        sum += fish_counters[i];
    printf("%ld\n", sum);

    return 0;
}

