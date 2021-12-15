#include <stdio.h>
#include <string.h>

#define MAX_CODELEN 7
#define NUM_DIGITS 4

typedef char code_t[MAX_CODELEN + 1];

int read_line(code_t codes[], code_t encoded_num[])
{
    size_t i;
    
    int ret = scanf(" %s", codes[0]);
    if (ret == EOF)
        return 0;
    for (i = 1; i < 10; i++) {
        ret = scanf(" %s", codes[i]);
    }
    
    scanf(" | %s", encoded_num[0]);
    for (i = 1; i < 4; i++)
        scanf(" %s", encoded_num[i]);

    return 1;
}

int main(void)
{
    code_t codes[10], encoded_num[NUM_DIGITS];
    size_t i;

    long count = 0;
    while (read_line(codes, encoded_num)) {
        for (i = 0; i < 4; i++) {
            size_t n = strlen(encoded_num[i]);
            if ((2 <= n && n <= 4) || n == 7)
                count++;
        }
    }
    printf("%ld\n", count);

    return 0;
}
