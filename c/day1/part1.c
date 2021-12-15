#include <stdio.h>

int main(void)
{
    int cur, prev, incr = 0;
    
    scanf("%d", &prev);

    while ((scanf("%d", &cur)) != EOF) {
         if (cur > prev)
             incr++;
         prev = cur;
    }

    printf("%d\n", incr);

    return 0;
}
