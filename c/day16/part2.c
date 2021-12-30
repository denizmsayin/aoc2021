#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>

#define MAX_BITS 65536

typedef unsigned char byte;

uint64_t consume(byte **bits_p, size_t n)
{
    uint64_t x = 0;
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

/* Not the fastest code due to lack of inlining, but practical! */
typedef uint64_t (*op_t)(uint64_t, uint64_t);

uint64_t add(uint64_t x, uint64_t y) { return x + y; }
uint64_t mul(uint64_t x, uint64_t y) { return x * y; }
uint64_t min(uint64_t x, uint64_t y) { return x < y ? x : y; }
uint64_t max(uint64_t x, uint64_t y) { return x > y ? x : y; }
uint64_t gt(uint64_t x, uint64_t y) { return x > y; }
uint64_t lt(uint64_t x, uint64_t y) { return x < y; }
uint64_t eq(uint64_t x, uint64_t y) { return x == y; }

op_t determine_op(uint64_t type_id)
{
    switch (type_id) {
        case 0: return add;
        case 1: return mul;
        case 2: return min;
        case 3: return max;
        case 5: return gt;
        case 6: return lt;
        case 7: return eq;
        default: assert (0);
    }
}

uint64_t parse_packet(byte **bits)
{
    consume(bits, 3);
    uint64_t value = 0;
    uint64_t type_id = consume(bits, 3);
    if (type_id == 4) {
        while (consume(bits, 1)) // first bit of remainder is 1
            value = (value << 4) + consume(bits, 4);
        value = (value << 4) + consume(bits, 4);
    } else {
        uint64_t len_id = consume(bits, 1);
        op_t op = determine_op(type_id);
        if (len_id) {
            uint64_t num_subpackets = consume(bits, 11);
            value = parse_packet(bits);
            while (--num_subpackets)
                value = op(value, parse_packet(bits));
        } else {
            uint64_t num_bits = consume(bits, 15);
            const byte *cur = *bits;
            const byte *end = cur + num_bits;
            value = parse_packet(bits);
            while (*bits != end)
                value = op(value, parse_packet(bits));
        }
    }
    return value;
}

int main(void)
{
    byte bits[MAX_BITS];
    byte *p = bits;

    memset(bits, 255, MAX_BITS); // Defensively set to not 0-1 for checking
    read_decode_hex(p);
    printf("%lu\n", parse_packet(&p));

    return 0;
}
