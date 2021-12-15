#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#define MAX_LINE 1024
#define MAX_BITS 64
#define NOT_FOUND ~((size_t) 0)

void trim_line(char *line)
{
    line[strcspn(line, "\n")] = 0;
}

unsigned long bitbuf2ul(char bitbuf[])
{
    unsigned long r = 0;
    for (size_t i = 0; i < MAX_BITS && bitbuf[i]; i++)
        r = (r << 1) + bitbuf[i] - '0';
    return r;
}

size_t select_bitbuf(char bitbufs[][MAX_BITS+1], size_t n, int invert)
{
    char out[MAX_LINE] = {};
    size_t left_count = n;

    for (size_t j = 0; j < MAX_BITS && left_count > 1; j++) {
        size_t zc = 0, oc = 0;
        char target;

        for (size_t i = 0; i < n; i++) {
            if (!out[i]) { 
                if (bitbufs[i][j] == '1')
                    oc++;
                else
                    zc++;
            }
        }

        target = (oc >= zc) ? 1 : 0;
        target = invert ? !target : target;
        target += '0';

        for (size_t i = 0; i < n; i++) {
            if (!out[i] && bitbufs[i][j] != target) {
                out[i] = 1;
                left_count--;
            }
        }
    }

    for (size_t i = 0; i < n; i++)
        if (!out[i])
            return i;

    return NOT_FOUND;
}

int main(void)
{
    static char bitbufs[MAX_LINE][MAX_BITS+1] = {};
    size_t n = 0;

    while (fgets(bitbufs[n], MAX_BITS+1, stdin)) {
        trim_line(bitbufs[n]);
        n++;
        if (n > MAX_LINE) {
            fputs("Oops, number of lines overflowed!\n", stderr);
            return 1;
        }
    }

    size_t o2_i = select_bitbuf(bitbufs, n, 0);
    size_t co2_i = select_bitbuf(bitbufs, n, 1);
    assert (o2_i != NOT_FOUND && co2_i != NOT_FOUND);

    unsigned long o2 = bitbuf2ul(bitbufs[o2_i]), co2 = bitbuf2ul(bitbufs[co2_i]);
    printf("%lu\n", o2 * co2);

    return 0;
}
