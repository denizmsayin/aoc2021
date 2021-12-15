#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LINE 1024
#define MAX_BITS 64
#define NOT_FOUND ~((size_t) 0)

static inline char d2n(char d) 
{ 
    if ('0' <= d && d <= '9')
        return d - '0';
    else
        return d;
}

size_t trim_line(char *line, size_t n)
{
    if (line[n-1] == '\n') {
        line[n-1] = 0;
        return n - 1;
    }
    return n;
}

void linebuf2bitbuf(char *bitbuf, const char *linebuf)
{
    // Determine if a newline gets in the way
    size_t len = strlen(linebuf);
    if (linebuf[len - 1] == '\n')
        len--;
    
    // Copy to end of bitbuf
    memcpy(bitbuf + MAX_BITS - len, linebuf, len);

    // Convert bitbuf values
    for (size_t i = MAX_BITS - len; i < MAX_BITS; i++)
        bitbuf[i] = d2n(bitbuf[i]);

    // Reverse the bitbuf
    for (size_t i = 0; i < (MAX_BITS >> 1); i++) {
        char tmp = bitbuf[i];
        bitbuf[i] = bitbuf[MAX_BITS - 1 - i];
        bitbuf[MAX_BITS - 1 - i] = tmp;
    }
}

void bitbuf_d2n(char bitbuf[])
{
    for (size_t i = 0; i < MAX_BITS; i++)
        bitbuf[i] = d2n(bitbuf[i]);
}

unsigned long bitbuf2ul(char bitbuf[])
{
    unsigned long r = 0;
    for (size_t i = 0; i < MAX_BITS; i++)
        r = (r << 1) + bitbuf[i];
    return r;
}

size_t select_bitbuf(char bitbufs[][MAX_BITS], size_t n, int invert)
{
    char out[MAX_LINE] = {};
    size_t out_count = 0;
    
    for (size_t j = 0; j < MAX_BITS && out_count < n - 1; j++) {
        size_t zc = 0, oc = 0;
        char target;

        for (size_t i = 0; i < n; i++)
            if (!out[i] && bitbufs[i][j])
                oc++;
            else
                zc++;

        target = (oc >= zc) ? 1 : 0;
        target = invert ? !target : target;

        printf("Target! %d\n", target);

        for (size_t i = 0; i < n; i++) {
            if (!out[i] && bitbufs[i][j] != target) {
                out[i] = 1;
                out_count++;
            }
        }
    }

    printf("Out count: %lu\n", out_count);

    for (size_t i = 0; i < n; i++)
        if (!out[i])
            return i;

    return NOT_FOUND;
}

int main(void)
{
    static char bitbufs[MAX_LINE][MAX_BITS] = {};
    char linebuf[MAX_BITS+1];
    size_t n = 0;

    return 0;

    while (fgets(linebuf, MAX_BITS+1, stdin)) {
        
        n++;
        if (n > MAX_LINE) {
            fputs("Oops, number of lines overflowed!\n", stderr);
            return 1;
        }
    }

    size_t o2_i = select_bitbuf(bitbufs, n, 0);
    size_t co2_i = select_bitbuf(bitbufs, n, 1);
    if (o2_i == NOT_FOUND || co2_i == NOT_FOUND) {
        fputs("Failed to select bit buffers, problem in the input or bug.\n", stderr);
        return 1;
    }

    unsigned long o2 = bitbuf2ul(bitbufs[o2_i]), co2 = bitbuf2ul(bitbufs[co2_i]);
    printf("%lu\n", o2 * co2);

    return 0;
}
