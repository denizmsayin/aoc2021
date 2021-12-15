#include <stdio.h>

int main(void)
{
    unsigned i;
    unsigned long one_counts[64] = {}, zero_counts[64] = {}, gamma, eps;
    char bitbuf[65];

    while (fgets(bitbuf, 65, stdin)) {
        for (i = 0; bitbuf[i] != 0; i++)
            if (bitbuf[i] == '1')
                one_counts[i]++;
            else
                zero_counts[i]++;
    }

    gamma = eps = 0;
    for (i = 0; i < 64; i++) {
        if (one_counts[i] != 0 && zero_counts[i] != 0) {
            if (one_counts[i] > zero_counts[i]) {
                gamma = (gamma << 1) + 1;
                eps <<= 1;
            } else {
                gamma <<= 1;
                eps = (eps << 1) + 1;
            }
        }
    }

    printf("%lu\n", gamma * eps);

    return 0;
}
