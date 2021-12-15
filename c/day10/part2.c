#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#define MAX_LEN 8192
#define MAX_LINES 8192

static inline int is_opening_par(int c) { return c == '(' || c == '[' || c == '{' || c == '<'; }

static inline int get_closing_par(int c) { return c == '(' ? c + 1 : c + 2; }

void trim_line(char *line)
{
    line[strcspn(line, "\n")] = 0;
}

long completion_score(const char *line)
{
    char stack[MAX_LEN];
    size_t stack_size = 0;
    long score;
    const char *itr;
    for (itr = line; *itr; itr++) {
        char c = *itr;
        if (is_opening_par(c))
            stack[stack_size++] = c;
        else if (stack_size == 0 || c != get_closing_par(stack[--stack_size]))
           return 0; 
    }

    // Complete with what remains on the stack
    score = 0;
    while (stack_size) {
        long x;
        switch (stack[--stack_size]) {
            case '(': x = 1; break;
            case '[': x = 2; break;
            case '{': x = 3; break;
            case '<': x = 4; break;
            default: assert(0);
        }
        score = 5 * score + x;
    }

    return score;
}

int cmp(const void *xp, const void *yp)
{
    long x = *(long *) xp;
    long y = *(long *) yp;
    if (x < y)
        return -1;
    else if (x == y)
        return 0;
    return 1;
}

int main(void)
{
    char buf[MAX_LEN];
    long scores[MAX_LINES];
    size_t n_scores = 0;
    while (fgets(buf, MAX_LEN, stdin)) {
        long score;
        trim_line(buf);
        score = completion_score(buf);
        if (score)
            scores[n_scores++] = score;
    }

    qsort(scores, n_scores, sizeof(*scores), cmp);
    printf("%ld\n", scores[n_scores >> 1]);
    return 0;
}

