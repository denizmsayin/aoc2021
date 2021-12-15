#include <stdio.h>

int main(void)
{
    int x, y, z, t, incr = 0;
    
    scanf("%d %d %d", &x, &y, &z);

    while ((scanf("%d", &t)) != EOF) {
         if (t > x)
             incr++;
         x = y;
         y = z;
         z = t;
    }

    printf("%d\n", incr);

    return 0;
}

