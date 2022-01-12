#include <stdio.h>
#include <assert.h>

static inline int mod1(int a, int b) { return (a - 1) % b + 1; }
static inline int min(int a, int b) { return a < b ? a : b; }

int main(void)
{
    int pos[2], score[2] = {0, 0};
    int d = 1, r = 0, i = 0;

    assert (scanf("Player 1 starting position: %d\n", &pos[0]) == 1);
    assert (scanf("Player 2 starting position: %d\n", &pos[1]) == 1);

    while (score[0] < 1000 && score[1] < 1000) {
        pos[i] = mod1(pos[i] + 3 * (d + 1), 10);
        score[i] += pos[i];
        d = mod1(d + 3, 100);
        r += 3;
        i = (i + 1) & 1;
    }

    printf("%d\n", r * min(score[0], score[1]));

    return 0;
}
