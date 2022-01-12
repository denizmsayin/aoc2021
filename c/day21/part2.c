#include <stdio.h>
#include <assert.h>

typedef long long lli;

struct pair {
    lli v[2];
};

static inline struct pair tupscmul(lli a, struct pair t)
{
    return (struct pair) {{ a * t.v[0], a * t.v[1] }};
}

static inline struct pair tupadd(struct pair t1, struct pair t2)
{
    return (struct pair) {{ t1.v[0] + t2.v[0], t1.v[1] + t2.v[1] }};
}

static inline lli max(lli a, lli b) { return a > b ? a : b; }

int main(void)
{
    static struct pair table[21][21][10][10][2];
    int pos[2];
    struct pair *initial;

    assert (scanf("Player 1 starting position: %d\n", &pos[0]) == 1);
    assert (scanf("Player 2 starting position: %d\n", &pos[1]) == 1);

    // Try to fill the DP table bottom-up this time,
    // instead of doing memoization like in Rust & Haskell
    // Kind of wasteful since I compute every state, but
    // I wanted to do something slightly different this time.
    static const lli MULTS[] = { 1, 3, 6, 7, 6, 3, 1 };
    static const int ROLLS[] = { 3, 4, 5, 6, 7, 8, 9 };
    static const size_t NROLLS = sizeof(ROLLS) / sizeof(ROLLS[0]);
    int s[2];
    for (int score_total = 50; score_total >= 0; score_total--) {
        for (s[0] = score_total; s[0] >= 0; s[0]--) {
            int p[2];
            s[1] = score_total - s[0];
            if (s[0] > 30 || s[1] > 30 || (s[0] >= 21 && s[1] >= 21))
                continue;
            for (p[0] = 0; p[0] <= 9; p[0]++) {
                for (p[1] = 0; p[1] <= 9; p[1]++) {
                    for (int i = 0; i <= 1; i++) {
                        struct pair value;
                        int j = (i + 1) & 1; // Last turn's player

                        if (s[0] >= 21) {
                            value = (struct pair) {{ 1, 0 }};
                        } else if (s[1] >= 21) {
                            value = (struct pair) {{ 0, 1 }};
                        } else {
                            value = table[s[0]][s[1]][p[0]][p[1]][i];
                        }

                        // Combinations of three 1-2-3 dice rolls are fun:
                        // 1: 3, 3: 4, 6: 5, 7: 6, 6: 7, 3: 8, 1: 9
                        for (size_t k = 0; k < NROLLS; k++) {
                            int prev_score = s[j];
                            int prev_pos = p[j];
                            int roll = ROLLS[k];
                            s[j] -= p[j] + 1;
                            p[j] = (p[j] + (10 - roll)) % 10; // 10 - roll to avoid neg.
                            if (s[j] >= 0 && s[0] < 21 && s[1] < 21) {
                                struct pair *pair = &table[s[0]][s[1]][p[0]][p[1]][j];
                                *pair = tupadd(*pair, tupscmul(MULTS[k], value));
                            }
                            s[j] = prev_score;
                            p[j] = prev_pos;
                        }
                    }
                }
            }
        }
    }

    initial = &table[0][0][pos[0]-1][pos[1]-1][0];
    printf("%lld\n", max(initial->v[0], initial->v[1]));

    return 0;
}
