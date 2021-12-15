#include <stdio.h>
#include <string.h>
#include <assert.h>

#define MAX_LEN 8192

static inline int is_opening_par(int c) { return c == '(' || c == '[' || c == '{' || c == '<'; }

static inline int get_closing_par(int c) { return c == '(' ? c + 1 : c + 2; }

void trim_line(char *line)
{
    line[strcspn(line, "\n")] = 0;
}

long get_error_score(const char *line)
{
    char stack[MAX_LEN];
    size_t stack_size = 0;
    const char *itr;
    for (itr = line; *itr; itr++) {
        char c = *itr;
        if (is_opening_par(c))
            stack[stack_size++] = c;
        else if (stack_size == 0 || c != get_closing_par(stack[--stack_size])) {
            switch (c) {
                case ')': return 3;
                case ']': return 57;
                case '}': return 1197;
                case '>': return 25137;
                default: assert (0);
            }
        }
    }

    return 0;
}

int main(void)
{
    char buf[MAX_LEN];
    long total = 0;
    while (fgets(buf, MAX_LEN, stdin)) {
        trim_line(buf);
        total += get_error_score(buf);
    }
    printf("%ld\n", total);
    return 0;
}

