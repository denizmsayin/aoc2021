#include <stdio.h>

static inline long max(long a, long b) { return a < b ? b : a; }

int main(void)
{
    char cmdbuf[32];
    long x, depth = 0, position = 0;

    while ((scanf("%s %ld", cmdbuf, &x)) != EOF) {
        switch (cmdbuf[0]) {
        case 'f':   position += x; break;
        case 'd':   depth += x; break;
        case 'u':   depth = max(depth - x, 0); break;
        default:    return 1;
        }
    }

    printf("%ld\n", position * depth);
    return 0;
}
