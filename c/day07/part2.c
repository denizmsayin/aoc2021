#include <stdio.h>
#include <stdlib.h>

#define MAX_VALUES 1024

size_t read_sep_vals(long buf[])
{
    long *itr = buf;
    scanf("%ld", itr++);
    while (scanf(",%ld", itr) > 0) 
        itr++;
    return itr - buf;
}

int main(void)
{
    long values[MAX_VALUES];
    size_t n_values = read_sep_vals(values);
    long min, max, target, best_cost;
    size_t i, j;

    min = max = values[0];
    for (i = 1; i < n_values; i++) {
        if (values[i] < min)
            min = values[i];
        else if (values[i] > max)
            max = values[i];
    }
       
    best_cost = 1L << 62; // Close to max
    for (target = min; target <= max; target++) {
        long cost = 0;
        for (j = 0; j < n_values; j++) {
            long diff = labs(target - values[j]);
            cost += (diff * (diff + 1)) >> 1;
        }
        if (cost < best_cost)
            best_cost = cost;
    }
    printf("%ld\n", best_cost);
    
    return 0;
}

