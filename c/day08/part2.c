#include <stdio.h>
#include <string.h>
#include <assert.h>

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

size_t count_common(const char *s1, const char *s2)
{
    size_t c = 0;
    while (*s1) {
        if (strchr(s2, *s1))
            c++;
        s1++;
    }
    return c;
}

int main(void)
{
    code_t random_codes[10], encoded_num[NUM_DIGITS];

    long total = 0;
    while (read_line(random_codes, encoded_num)) {
        code_t codebook[10];
        size_t i, j;
        long value;

        // Decoding phase
        // First pass to determine 1,4,7,8
        for (i = 0; i < 10; i++) {
            size_t n = strlen(random_codes[i]);
            long target = -1;
            switch (n) {
                case 2: target = 1; break;
                case 3: target = 7; break;
                case 4: target = 4; break;
                case 7: target = 8; break;
                default: break;
            }
            if (target >= 0)
                strcpy(codebook[target], random_codes[i]);
        }

        // Second pass to determine the others
        for (i = 0; i < 10; i++) {
            size_t n = strlen(random_codes[i]);
            if (n == 5 || n == 6) {
                size_t com1 = count_common(random_codes[i], codebook[1]);
                size_t com4 = count_common(random_codes[i], codebook[4]);
                long target;
                if (n == 5) {
                    if (com1 == 1 && com4 == 2)
                        target = 2;
                    else if (com1 == 2 && com4 == 3)
                        target = 3;
                    else if (com1 == 1 && com4 == 3)
                        target = 5;
                    else
                        assert (0);
                } else {
                    if (com1 == 2 && com4 == 3)
                        target = 0;
                    else if (com1 == 1 && com4 == 3)
                        target = 6;
                    else if (com1 == 2 && com4 == 4)
                        target = 9;
                    else
                        assert (0);
                }
                strcpy(codebook[target], random_codes[i]);
            }
        }
    
        // Calculation phase
        value = 0;
        for (i = 0; i < NUM_DIGITS; i++) {
            size_t n = strlen(encoded_num[i]);
            for (j = 0; j < 10; j++) {
                if (n == strlen(codebook[j]) && n == count_common(encoded_num[i], codebook[j])) {
                    value = 10 * value + j;
                    break;
                }
            }
        }

        total += value;
    }

    printf("%ld\n", total);
    
    return 0;
}
