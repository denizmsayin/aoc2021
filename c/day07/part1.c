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

int cmp(const void *xp, const void *yp)
{
    long x = *(long *) xp;
    long y = *(long *) yp;
    if (x < y)
        return -1;
    else if (x == y)
        return 0;
    return 1;
}

int main(void)
{
    long values[MAX_VALUES];
    size_t n_values = read_sep_vals(values);
    long median, cost;
    size_t i;

    qsort(values, n_values, sizeof(*values), cmp);
    median = values[n_values >> 1];

    cost = 0;
    for (i = 0; i < n_values; i++)
        cost += labs(values[i] - median);
    printf("%ld\n", cost);
    
    return 0;
}

