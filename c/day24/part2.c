#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#define BUF_SZ 32
#define N_BLOCKS 14

// Only switch this around for part2
static const int VALUES[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9 };
#define N_VALUES ((int) (sizeof(VALUES) / sizeof(VALUES[0])))

struct block {
    int push;
    int value;
};

int read_line_trim_end(char buf[], size_t n)
{
    int i;

    if (!fgets(buf, n, stdin))
        return 0;

    i = strlen(buf) - 1;
    while (i >= 0 && isspace(buf[i]))
       buf[i--] = 0; 

    return 1;
}

void read_block_init(void)
{
    char buf[BUF_SZ];
    assert (read_line_trim_end(buf, BUF_SZ) && !strcmp(buf, "inp w"));
}

int is_div_z_26(const char *instr)
{
    return instr[0] == 'd' && instr[4] == 'z' && instr[6] == '2' && instr[7] == '6';
}

int is_add_const_to_ch(const char *instr, char target_ch)
{
    return instr[0] == 'a' && instr[4] == target_ch && isdigit(instr[6]);
}

enum reader_state {
    EXPECT_DIV_OR_ADD,
    EXPECT_DIV_NEXT_LINE,
    SKIP_TO_END
};

// Read until the next inp w or end of file.
int read_block(struct block *blk)
{
    char buf[BUF_SZ];
    int state = EXPECT_DIV_OR_ADD;
    int is_push = 1;
    int last_add_value;
    int ret = read_line_trim_end(buf, BUF_SZ);
    if (!ret)
        return 0;

    while (read_line_trim_end(buf, BUF_SZ) && strcmp(buf, "inp w")) {
        switch (state) {
        case EXPECT_DIV_OR_ADD:
            if (is_div_z_26(buf)) {
                state = EXPECT_DIV_NEXT_LINE;
                is_push = 0;
            } else if (is_add_const_to_ch(buf, 'y')) {
                sscanf(buf, "add y %d", &last_add_value);
            }
            break;
        case EXPECT_DIV_NEXT_LINE:
            sscanf(buf, "add x %d", &last_add_value);
            state = SKIP_TO_END;
            break;
        case SKIP_TO_END:
            break;
        }
    }

    blk->push = is_push;
    blk->value = last_add_value;
    return 1;
}

int try(const struct block blocks[], int values[], int stack[], int i, int stk_i)
{
    if (i >= N_BLOCKS) {
        return stk_i == 0;
    } else {
        if (blocks[i].push) {
            for (int j = 0; j < N_VALUES; j++) {
                values[i] = VALUES[j];
                stack[stk_i] = values[i] + blocks[i].value;
                if (try(blocks, values, stack, i + 1, stk_i + 1))
                    return 1;
            }
            return 0;
        } else {
            assert (stk_i > 0);
            int top = stack[stk_i-1];
            int target = top + blocks[i].value;
            if (1 <= target && target <= 9) {
                values[i] = target;
                return try(blocks, values, stack, i + 1, stk_i - 1);
            }
            else
                return 0;
        }
    }
}

int first_valid_values(const struct block blocks[], int values[])
{
    int stack[N_BLOCKS];
    return try(blocks, values, stack, 0, 0);
}

void print_block(const struct block *blk)
{
    printf("%s %d\n", blk->push ? "push" : "pop", blk->value);
}

int main(void)
{
    struct block blocks[N_BLOCKS];
    int values[N_BLOCKS] = {0};
    int i = 0;

    while (read_block(&blocks[i]))
        assert (i++ < N_BLOCKS);
    assert (i == N_BLOCKS);

    assert (first_valid_values(blocks, values));

    for (i = 0; i < N_BLOCKS; i++)
        printf("%d", values[i]);
    putchar('\n');

    return 0;
}
