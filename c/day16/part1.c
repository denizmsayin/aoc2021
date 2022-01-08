#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>

#define MAX_BITS 65536

typedef unsigned char byte;

int consume(byte **bits_p, size_t n)
{
    int x = 0;
    size_t i;
    byte *bits = *bits_p;
    for (i = 0; i < n; i++) {
        assert (bits[i] == 0 || bits[i] == 1);
        x = (x << 1) + bits[i];
    }
    *bits_p = bits + n;
    return x;
}

void read_decode_hex(byte *bits)
{
    int c;
    while ((c = getchar()) != EOF) {
        byte value;

        if ('0' <= c && c <= '9')
            value = c - '0';
        else if ('A' <= c && c <= 'F')
            value = c - 'A' + 10;
        else if ('a' <= c && c <= 'f')
            value = c - 'a' + 10;
        else
            continue;

        // Careful with the order
        assert (0 <= value && value < 16);
        *bits++ = value >> 3;
        *bits++ = (value >> 2) & 1;
        *bits++ = (value >> 1) & 1;
        *bits++ = value & 1;
    }
}

int parse_packet(byte **bits)
{
    int ver_num = consume(bits, 3);
    int type_id = consume(bits, 3);
    if (type_id == 4) {
        while (**bits) // first bit of remainder is 1
            consume(bits, 5);
        consume(bits, 5); // last piece starting with 0
    } else {
        int len_id = consume(bits, 1);
        if (len_id) {
            int num_subpackets = consume(bits, 11);
            while (num_subpackets--)
                ver_num += parse_packet(bits);
        } else {
            int num_bits = consume(bits, 15);
            const byte *end = *bits + num_bits;
            while (*bits != end)
                ver_num += parse_packet(bits);
        }
    }
    return ver_num;
}

int main(void)
{
    byte bits[MAX_BITS];
    byte *p = bits;

    memset(bits, 255, MAX_BITS); // Defensively set to not 0-1 for checking
    read_decode_hex(p);
    printf("%d\n", parse_packet(&p));

    return 0;
}
