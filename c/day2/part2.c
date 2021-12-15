#include <stdio.h>

static inline long max(long a, long b) { return a < b ? b : a; }

int main(void)
{
    char cmdbuf[32];
    long x, aim = 0, depth = 0, position = 0;

    while ((scanf("%s %ld", cmdbuf, &x)) != EOF) {
        switch (cmdbuf[0]) {
        case 'f':   
            position += x; 
            depth = max(depth + aim * x, 0);
            break;
        case 'd':   
            aim += x; 
            break;
        case 'u':   
            aim -= x; 
            break;
        default:    
            return 1;
        }
    }

    printf("%ld\n", position * depth);
    return 0;
}
