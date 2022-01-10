#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>

#define N 1048576 // 1 Mebi

// Since linked lists suck, I'll take the lazy
// approach and simply shift around a huge array.
// Long live caches!

struct elem {
    char is_num;
    union {
        uint16_t num;
        char ch;
    };
};

struct elem ch_elem(char ch)
{
    return (struct elem) { .is_num = 0, { .ch = ch } };
}

struct elem num_elem(uint16_t num)
{
    return (struct elem) { .is_num = 1, { .num = num } };
}

struct snailnum {
    struct elem elems[N];
    size_t len;
};

int read_snailnum(struct snailnum *sn)
{
    static char linebuf[N];
    if (fgets(linebuf, N, stdin)) {
        size_t i;
        for (i = 0; linebuf[i] != '\n' && linebuf[i] != 0; i++) {
            struct elem *e = &sn->elems[i];
            assert (i < N);
            if ('0' <= linebuf[i] && linebuf[i] <= '9')
                *e = num_elem(linebuf[i] - '0');
            else
                *e = ch_elem(linebuf[i]);
        }
        sn->len = i;
        return 1;
    }
    return 0;
}

void print_snailnum(const struct snailnum *sn)
{
    for (size_t i = 0; i < sn->len; i++) {
        const struct elem *e = &sn->elems[i];
        if (e->is_num) {
            printf("%d", e->num);
        } else {
            putchar(e->ch);
        }
    }
    putchar('\n');
}

void shl(struct snailnum *sn, size_t start, size_t shift)
{
    for (size_t i = start; i + shift < sn->len; i++)
        sn->elems[i] = sn->elems[i + shift];
    sn->len -= shift;
}

void shr(struct snailnum *sn, size_t start, size_t shift)
{
    sn->len += shift;
    assert (sn->len < N);
    for (size_t i = sn->len - 1; start + shift <= i; i--)
        sn->elems[i] = sn->elems[i - shift];
}

void add_snailnum(struct snailnum *sn1, const struct snailnum *sn2)
{
    assert (sn1->len + sn2->len + 3 < N);
    shr(sn1, 0, 1);
    sn1->elems[0] = ch_elem('[');
    sn1->elems[sn1->len++] = ch_elem(',');
    memcpy(sn1->elems + sn1->len, sn2->elems, sn2->len * sizeof(sn2->elems[0]));
    sn1->len += sn2->len;
    sn1->elems[sn1->len++] = ch_elem(']');
}

inline int elem_char_is(const struct elem *elem, char ch)
{
    return !elem->is_num && elem->ch == ch;
}

int completes_num_pair(const struct elem *elems, size_t i)
{
    return elems[i+1].is_num && elem_char_is(&elems[i+2], ',')
        && elems[i+3].is_num && elem_char_is(&elems[i+4], ']');
}

// The snailnum never starts with splits.
// There can be multiple explodes which can be process in one pass,
// since explosions have priorities over splits.
// Then, each split can trigger an explosion on the number it created.
// Could use this info for optimization, but I won't!

int explode(struct snailnum *sn)
{
    int depth = 0;
    struct elem *elems = sn->elems;
    for (size_t i = 0; i < sn->len; i++) {
        struct elem *e = &elems[i];
        if (!e->is_num) {
            char ch = e->ch;
            if (ch == '[') { 
                if (depth == 4 && completes_num_pair(elems, i)) {
                    uint16_t left = elems[i+1].num;
                    uint16_t right = elems[i+3].num;
                    
                    // Eliminate pair and replace with 0
                    shl(sn, i + 1, 4);
                    elems[i] = num_elem(0);
                    
                    // Try to add to sides
                    for (ssize_t j = i - 1; j >= 0; j--) {
                        if (elems[j].is_num) {
                            elems[j].num += left;
                            break;
                        }
                    }
                    
                    for (size_t j = i + 1; j < sn->len; j++) {
                        if (elems[j].is_num) {
                            elems[j].num += right;
                            break;
                        }
                    }
                    
                    return 1;
                } else {
                    depth++;
                }
            } else if (ch == ']') {
                depth--;
            }
        }
    }
    return 0;
}

int split(struct snailnum *sn)
{
    struct elem *elems = sn->elems;
    for (size_t i = 0; i < sn->len; i++) {
        if (elems[i].is_num) {
            uint16_t x = elems[i].num;
            if (x >= 10) {
                uint16_t left = x / 2;
                uint16_t right = x - left;

                // Add some space and insert the new pair
                shr(sn, i + 1, 4);
                elems[i] = ch_elem('[');
                elems[i+1] = num_elem(left);
                elems[i+2] = ch_elem(',');
                elems[i+3] = num_elem(right);
                elems[i+4] = ch_elem(']');

                return 1;
            }
        }
    }
    return 0;
}

int step(struct snailnum *sn)
{
    int boom = explode(sn);
    if (boom)
        return 1;
    return split(sn);
}

void reduce(struct snailnum *sn)
{
    while (step(sn));
}

// Instead of parsing into a tree, just calculate mag on the fly as if parsing
uint64_t magnitude_(const struct snailnum *sn, size_t *pos)
{
    const struct elem *e = &sn->elems[*pos];
    if (e->is_num) {
        *pos += 1;
        return e->num;
    } else {
        uint64_t left, right;
        *pos += 1; // skip [
        left = magnitude_(sn, pos);
        *pos += 1; // skip ,
        right = magnitude_(sn, pos);
        *pos += 1; // skip ]
        return 3 * left + 2 * right;
    }
}

uint64_t magnitude(const struct snailnum *sn)
{
    size_t p = 0;
    return magnitude_(sn, &p);
}

int main(void)
{
    static struct snailnum num;
    static struct snailnum read_num;
    assert (read_snailnum(&num));
    while (read_snailnum(&read_num)) {
        add_snailnum(&num, &read_num);
        reduce(&num);
    }
    printf("%lu\n", magnitude(&num));
    return 0;
}

