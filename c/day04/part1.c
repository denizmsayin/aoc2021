#include <stdio.h>
#include <stdlib.h>

#define MAX_CALLED 4096
#define MAX_BINGOS 1024
#define MAX_NUMBER 1000
#define N 5

size_t read_sep_longs(long buf[])
{
    long *itr = buf;
    scanf("%ld", itr++);
    while (scanf(",%ld", itr) > 0) 
        itr++;
    return itr - buf;
}

typedef long bingo_t[N][N];

int read_bingo_board(bingo_t bingo)
{
    size_t i, j;
    for (i = 0; i < N; i++) {
        for (j = 0; j < N; j++) {
            int ret = scanf(" %ld", &bingo[i][j]);
            if (ret == EOF)
                return 0;
        }
    }
    return 1;
}

size_t read_bingo_boards(bingo_t bingos[])
{
    size_t i = 0;
    while (read_bingo_board(bingos[i]))
        i++;
    return i;
}

int board_won(bingo_t board, char called[])
{
    size_t i, j;
    int row_win, col_win;

    for (i = 0; i < N; i++) {
        row_win = col_win = 1;
        for (j = 0; j < N; j++) {
            if (!called[board[i][j]]) // row check
               row_win = 0; 
            if (!called[board[j][i]]) // col check
                col_win = 0;
        }
        if (row_win || col_win)
            return 1;
    }

    return 0;
}

long board_score(bingo_t board, char called[], long last_call)
{
    size_t i, j;
    long s = 0;

    for (i = 0; i < N; i++)
        for (j = 0; j < N; j++)
            if (!called[board[i][j]])
                s += board[i][j];

    return s * last_call;
}

int main(void)
{
    long called_numbers[MAX_CALLED];
    size_t n_called = read_sep_longs(called_numbers);
    static bingo_t boards[MAX_BINGOS];
    size_t n_boards = read_bingo_boards(boards);
    char called[MAX_NUMBER + 1] = {}; // Will use as set
    size_t i, j;

    for (i = 0; i < n_called; i++) {
        called[called_numbers[i]] = 1;
        for (j = 0; j < n_boards; j++) {
            if (board_won(boards[j], called)) {
                printf("%ld\n", board_score(boards[j], called, called_numbers[i]));
                return 0;
            }
        }
    }

    puts("No boards won, weird!");

    return 0;
}

