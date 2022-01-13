#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <assert.h>

#include "dheap/dheap.h"
#include "dhashtable/dhashtable.h"

#define ROOM_HEIGHT     2
#define HALLWAY_LEN     11
#define AMPHS_PER_TYPE  ROOM_HEIGHT
#define BURROW_HEIGHT   (ROOM_HEIGHT + 1)
#define BURROW_WIDTH    HALLWAY_LEN
#define BURROW_LEN      ((ROOM_HEIGHT * 4) + 7)
#define HW_LEFT_I       (ROOM_HEIGHT * 4)

// Try to do no space waste. Final state is:
// AABBCCDD.......
// Left is down, right is up.
typedef char burrow_t[BURROW_LEN];

typedef uint64_t u64;
#define U64_MAX (~((u64) 0))

// TODO: Become even more efficient!
// Encode '.' as 5 and A, B, C, D as 0, 1, 2, 3
// Pack whole rooms into numbers!
// Cache stuff!
// Anything goes...

static void read_burrow(burrow_t burrow)
{
    size_t i, j;
    int c, k;

    assert (scanf("%*[^\n]\n") == 0); // Skip first line

    j = BURROW_LEN - 1;
    // Read the first line
    i = 0;
    getchar();
    while ((c = getchar()) != '\n') {   
        if (i % 2 == 0 || i == 1 || i == HALLWAY_LEN)
            burrow[j--] = c;
        i++;
    } 

    // Read the hallway lines
    for (k = 1; k >= 0; k--) {
        j = k;
        while ((c = getchar()) != '\n') {
           if ('A' <= c && c <= 'D') {
               burrow[j] = c;
               j += ROOM_HEIGHT;
           }
        }
    }

    assert (scanf("%*[^\n]\n") == 0); // Skip last line
}

void print_burrow_raw(const burrow_t burrow)
{
    for (size_t i = 0; i < BURROW_LEN; i++)
        putchar(burrow[i]);
    putchar('\n');
}

void putchar2(int a)
{
    putchar(a);
    putchar(a);
}

void print_burrow(const burrow_t burrow)
{
    char fillc;

    for (size_t i = 0; i < HALLWAY_LEN + 2; i++)
        putchar('#');
    putchar('\n');

    putchar('#');
    for (size_t i = 4 * ROOM_HEIGHT; i < BURROW_LEN; i++) {
        putchar(burrow[i]);
        if (4 * ROOM_HEIGHT + 1 <= i && i < 4 * ROOM_HEIGHT + 5)
            putchar('.');
    }

    putchar('#');
    putchar('\n');

    fillc = '#';
    for (int k = ROOM_HEIGHT - 1; k >= 0; k--) {
        int j = k;
        putchar2(fillc);
        for (int i = 1; i <= HALLWAY_LEN - 2; i++) {
            if (i & 1) {
                putchar('#');
            } else {
                putchar(burrow[j]);
                j += ROOM_HEIGHT;
            }
        }
        putchar2(fillc);
        putchar('\n');
        fillc = ' ';
    }

    putchar2(' ');
    for (int i = 1; i < HALLWAY_LEN - 1; i++)
        putchar('#');
    putchar2(' ');
    putchar('\n');
}

static inline u64 encode_ch(char c)
{
    return c == '.' ? 4 : c - 'A';
}

static u64 encode_burrow(const burrow_t burrow)
{
    u64 e = 0;
    for (int i = 0; i < BURROW_LEN; i++)
        e = 5 * e + encode_ch(burrow[i]);
    return e;
}

static inline char decode_ch(u64 e)
{
    return e == 4 ? '.' : e + 'A';
}

static void decode_burrow(u64 e, burrow_t burrow)
{
    for (int i = BURROW_LEN - 1; i >= 0; i--) {
        burrow[i] = decode_ch(e % 5);
        e /= 5;
    }
    assert (e == 0);
}

static inline int is_amph(char c) { return 'A' <= c && c <= 'D'; }

static inline u64 amph_move_cost(char c)
{
    switch (c) {
        case 'A': return 1;
        case 'B': return 10;
        case 'C': return 100;
        case 'D': return 1000;
        default: assert (0);
    }
}

int room_available_spot(const burrow_t burrow, int room)
{
    char a = room + 'A';
    int i = room * ROOM_HEIGHT;
    int end = i + ROOM_HEIGHT;
    int candidate;

    while (i < end && burrow[i] == a)
        i++;

    candidate = i;

    while (i < end && burrow[i] == '.')
        i++;

    return i == end ? candidate : -1;
}

static inline void move_nocost(burrow_t burrow, int i, int j)
{
    burrow[j] = burrow[i];
    burrow[i] = '.';
}

static inline void get_ij(int k, int *i, int *j)
{
    if (k < 4 * ROOM_HEIGHT) {
        *j = (k / ROOM_HEIGHT) * 2 + 2;
        *i = ROOM_HEIGHT - k % ROOM_HEIGHT;
    } else {
        *i = 0;
        if (k <= 4 * ROOM_HEIGHT + 1)
            *j = k - 4 * ROOM_HEIGHT;
        else if (k <= 4 * ROOM_HEIGHT + 5)
            *j = 2 * (k - 4 * ROOM_HEIGHT) - 1;
        else
            *j = BURROW_WIDTH - 1;
    }
}

static inline u64 manhattan(int i, int j)
{
    int i1, i2, j1, j2;
    get_ij(i, &i1, &j1);
    get_ij(j, &i2, &j2);
    return abs(i1 - i2) + abs(j1 - j2);
}

static inline u64 move(burrow_t burrow, int i, int j)
{
    u64 mult = amph_move_cost(burrow[i]);
    move_nocost(burrow, i, j);
    return mult * manhattan(i, j);
}

static int amphipods_organized(const burrow_t burrow)
{
    char a = 'A';
    for (int b = 0; b < 4 * ROOM_HEIGHT; b += ROOM_HEIGHT) {
        for (int i = b; i < b + ROOM_HEIGHT; i++)
            if (burrow[i] != a)
                return 0;
        a++;
    }
    return 1;
}

static int amph_in_final_pos(const burrow_t burrow, int i)
{
    char a = burrow[i];
    int ai = a - 'A';
    int start = ai * ROOM_HEIGHT;
    if (start <= i && i < start + ROOM_HEIGHT) {
        while (--i >= start)
            if (burrow[i] != a)
                return 0;
        return 1;
    }
    return 0;
}

// static int amph_in_final_room(const burrow_t burrow, int i)
// {
//     char a = burrow[i];
//     int ai = a - 'A';
//     int start = ai * ROOM_HEIGHT;
//     while (--i >= start)
//         if (burrow[i] != 
// 
//     if (start <= i && i < start + ROOM_HEIGHT) {
//         while (--i >= start)
//             if (burrow[i] != a)
//                 return 0;
//         return 1;
//     }
//     return 0;
// }

static int room_to_hallway_clear(const burrow_t burrow, int i)
{
    while (++i % ROOM_HEIGHT != 0)
        if (burrow[i] != '.')
            return 0;
    return 1;
}

// Simply j distances to the target room
/*
static u64 horiz_cost_to_goal(const burrow_t burrow)
{
    u64 cost = 0;
    for (int k = 0; k < BURROW_LEN; k++) {
        if (is_amph(burrow[k])) {
            int room = burrow[k] - 'A';
            int room_j = 2 * room + 2;
            int i, j;
            get_ij(k, &i, &j);
            cost += amph_move_cost(burrow[k]) * abs(room_j - j);
        }
    }
    return cost;
}
*/

// Actual cost of getting to the end, if only amphs could move
// through eachother with no collision.
static u64 no_collision_cost_to_goal(const burrow_t burrow)
{
    // cost of moving everyone 'down' into rooms from corridor
    u64 cost = 1111 * ((ROOM_HEIGHT * (ROOM_HEIGHT + 1)) / 2);
    for (int k = 0; k < BURROW_LEN; k++) {
        if (is_amph(burrow[k])) {
            int room = burrow[k] - 'A';
            int room_j = 2 * room + 2;
            int i, j;
            get_ij(k, &i, &j);
            if (j == room_j) { // Reduce cost since already in room
                cost -= amph_move_cost(burrow[k]) * abs(i);
            } else { // Cost to move to top of the room
                cost += amph_move_cost(burrow[k]) * (abs(room_j - j) + abs(i));
            }
        }
    }
    return cost;
}


static u64 heuristic(const burrow_t burrow)
{
    return no_collision_cost_to_goal(burrow);
}

static void add_neighbor_to_search(dhashtable_t *costs, dheap_t *heap, burrow_t burrow, 
                                   int si, int ti, int current_cost)
{
    u64 amph_move_cost = move(burrow, si, ti);
    u64 neigh_enc = encode_burrow(burrow); // Zobrist for speed?
    u64 tentative_cost = current_cost + amph_move_cost;
    struct dhash_table_pair *p = dhashtable_lookup_or_insert(costs, neigh_enc, U64_MAX);
    if (tentative_cost < p->value) {
        // Less than old distance
        p->value = tentative_cost;
        dheap_add(heap, neigh_enc, tentative_cost + heuristic(burrow));
    }
    move_nocost(burrow, ti, si); // Undo the move
}

// 
// static void hallway_traverse(dhashtable_t *costs, dheap_t *heap, burrow_t burrow, 
//                              int si, int sj, int je, int jincr, int current_cost)
// {
//     for (int jj = sj + jincr; jj != je; jj += jincr) {
//         if (!is_room_j(jj)) { // Skip room columns
//             if (burrow[0][jj] == '.') { // Add if clear
//                 add_neighbor_to_search(costs, heap, burrow, si, sj, 0, jj,
//                                        current_cost);
//             } else {
//                 break; // Stop otherwise, hallway is blocked
//             }
//         }
//     }
// }

int main(void)
{
    static dheap_t heap;
    burrow_t burrow;
    dhashtable_t costs;
    u64 enc;

    dhashtable_init(&costs);

    read_burrow(burrow);
   
    enc = encode_burrow(burrow);
    dhashtable_insert(&costs, enc, 0);
    dheap_add(&heap, enc, heuristic(burrow));

    while (!dheap_empty(&heap)) {
        u64 current_f_cost = dheap_min(&heap);
        u64 current_enc = dheap_min_key(&heap);
        u64 current_cost;
        struct dhash_table_pair *p;
        dheap_pop_min(&heap);
        decode_burrow(current_enc, burrow);
        current_cost = current_f_cost - heuristic(burrow);

        if (amphipods_organized(burrow)) {
            printf("%lu\n", current_cost);
            break;
        }

        if ((p = dhashtable_lookup(&costs, current_enc)) && current_cost > p->value)
            continue;
// 
//         print_burrow(burrow);
//         print_burrow_raw(burrow);

        // Room check
        for (int i = 0; i < 4 * ROOM_HEIGHT; i++) {
//             printf("%d: isa(%d) isfinal(%d) isclear(%d)\n", i, is_amph(burrow[i]), amph_in_final_pos(burrow, i), room_to_hallway_clear(burrow, i));
            if (is_amph(burrow[i]) && !amph_in_final_pos(burrow, i) 
                && room_to_hallway_clear(burrow, i)) 
            {
                int room = i / ROOM_HEIGHT;
                int r =  room + 4 * ROOM_HEIGHT + 2;
                int l = r - 1;
                for (int ti = l; ti >= 4 * ROOM_HEIGHT; ti--) {
                    if (burrow[ti] == '.')
                        add_neighbor_to_search(&costs, &heap, burrow, i, ti, current_cost);
                    else
                        break;
                }
                for (int ti = r; ti < BURROW_LEN; ti++) {
                    if (burrow[ti] == '.')
                        add_neighbor_to_search(&costs, &heap, burrow, i, ti, current_cost);
                    else
                        break;
                }
            }
        }

        // Hallway check
        for (int i = 4 * ROOM_HEIGHT; i < BURROW_LEN; i++) {
            if (is_amph(burrow[i])) {
                int room = burrow[i] - 'A';
                int ti = room_available_spot(burrow, room);
                if (ti != -1) {
                    int r = room + 4 * ROOM_HEIGHT + 2;
                    int ok = 1; // Make sure hallway is empty
                    if (i >= r) { // Right of target room
                        for (int j = i - 1; j >= r; j--) {
                            if (burrow[j] != '.') {
                                ok = 0;
                                break;
                            }
                        }
                    } else { // Left of target room
                        int l = r - 1;
                        for (int j = i + 1; j <= l; j++) {
                            if (burrow[j] != '.') {
                                ok = 0;
                                break;
                            }
                        }
                    }
                    if (ok)
                        add_neighbor_to_search(&costs, &heap, burrow, i, ti, current_cost);
                }
            }
        }
    }

    fprintf(stderr, "States discovered: %lu\n", costs.num_entries);
    dhashtable_destroy(&costs);

    return 0;
}

// static u64 encode_burrow(const burrow_t burrow)
// {
//     u64 enc = 0;
//     for (size_t i = 0; i < BURROW_HEIGHT; i++) {
//         for (size_t j = 0 ; j < BURROW_WIDTH; j++) {
//             char c = burrow[i][j];
//             if (c == '#') {
//                 continue;
//             } else if (c == '.') {
//                 enc *= 5;
//             } else {
//                 enc = 5 * enc + c - 'A' + 1;
//             }
//         }
//     }
//     return enc;
// }
// 
// static inline char decode_ch(u64 *enc)
// {
//     u64 enc_ch = *enc % 5;
//     *enc /= 5;
//     if (enc_ch == 0)
//         return '.';
//     return enc_ch - 1 + 'A';
// }
// 
// static void decode_burrow(u64 enc, burrow_t burrow)
// {
//     init_burrow(burrow);
// 
//     // Decode side rooms
//     for (size_t i = ROOM_HEIGHT; i > 0; i--)
//         for (size_t j = 8; j >= 2; j -= 2)
//             burrow[i][j] = decode_ch(&enc);
// 
//     // Decode hallway
//     for (size_t j = BURROW_WIDTH; j --> 0;) // Ah yes, the 'goes to' operator :3
//         burrow[0][j] = decode_ch(&enc);
// 
//     assert (enc == 0);
// }
// 
// typedef long long lli;
// 
// static inline int is_amph(char c) { return 'A' <= c && c <= 'D'; }
// 
// static inline u64 amph_move_cost(char c)
// {
//     switch (c) {
//         case 'A': return 1;
//         case 'B': return 10;
//         case 'C': return 100;
//         case 'D': return 1000;
//         default: assert (0);
//     }
// }
// 
// static inline int target_room_j(char c)
// {
//     return 2 * (c - 'A' + 1);
// }
// 
// static inline char room_target_amph(int j)
// {
//     return (j / 2) - 1 + 'A';
// }
// 
// // Start not inclusive, end inclusive
// int row_clear(const burrow_t burrow, int start, int end, int i)
// {
//     int inc = start > end ? -1 : 1;
//     for (int j = start + inc; j != end; j += inc)
//         if (burrow[i][j] != '.')
//             return 0;
//     return 1;
// }
// 
// // Returns an index for an available i in the given room,
// // otherwise returns -1, in case the room is not available
// int room_available_spot(const burrow_t burrow, int j)
// {
//     char a = room_target_amph(j);
//     int i = ROOM_HEIGHT, candidate;
// 
//     // Skip same amphs
//     while (i > 0 && burrow[i][j] == a)
//         i--;
// 
//     candidate = i;
// 
//     // Now, all remaining slots should be empty
//     while (i > 0 && burrow[i][j] == '.')
//         i--;
// 
//     if (i == 0)
//         return candidate;
//     return -1;
// }
// 
// static inline void move_nocost(burrow_t burrow, int si, int sj, int ti, int tj)
// {
//     burrow[ti][tj] = burrow[si][sj];
//     burrow[si][sj] = '.';
// }
// 
// static inline u64 move(burrow_t burrow, int si, int sj, int ti, int tj)
// {
//     u64 dist = abs(si - ti) + abs(sj - tj);
//     u64 cost = dist * amph_move_cost(burrow[si][sj]);
//     move_nocost(burrow, si, sj, ti, tj);
//     return cost;
// }
// 
// static int amph_in_final_pos(const burrow_t burrow, int i, int j)
// {
//     // To be in a final position, an amphipod must be
//     // in its target room, and all amphipods below it
//     // should be of the correct type as well.
//     char a = burrow[i][j];
//     if (j == target_room_j(a)) {
//         for (int ii = i + 1; ii <= ROOM_HEIGHT; ii++)
//             if (burrow[ii][j] != a)
//                 return 0;
//         return 1;
//     }
//     return 0;
// }
// 
// static int room_to_hallway_clear(const burrow_t burrow, int i, int j)
// {
//     for (int ii = i - 1; ii > 0; ii--)
//         if (burrow[ii][j] != '.')
//             return 0;
//     return 1;
// }
// 
// static int amphipods_organized(const burrow_t burrow)
// {
//     for (int j = 2; j <= 8; j += 2) {
//         char a = room_target_amph(j);
//         for (int i = ROOM_HEIGHT; i > 0; i--)
//             if (burrow[i][j] != a)
//                 return 0;
//     }
//     return 1;
// }
// 
// static inline int is_room_j(int j) { return j == 2 || j == 4 || j == 6 || j == 8; }
// 
// static void find_amph_positions(const burrow_t burrow, int positions[4][AMPHS_PER_TYPE][2])
// {
//     int inds[4] = {0};
//     for (int i = 0; i < BURROW_HEIGHT; i++) {
//         for (int j = 0; j < BURROW_WIDTH; j++) {
//             if (is_amph(burrow[i][j])) {
//                 int a = burrow[i][j] - 'A';
//                 positions[a][inds[a]][0] = i;
//                 positions[a][inds[a]][1] = j;
//                 inds[a]++;
//             }
//         }
//     }
// }
// 
// static u64 no_collision_cost(int positions[4][AMPHS_PER_TYPE][2])
// {
//     static const int pushdown_cost = (ROOM_HEIGHT * (ROOM_HEIGHT + 1)) / 2;
// 	// Try every permutation, assign minimum distance
//     u64 total_cost = 0;
//     for (int a = 0; a < 4; a++) {
//         int tj = target_room_j(a + 'A');
//         u64 cost = pushdown_cost;
//         for (int ai = 0; ai < AMPHS_PER_TYPE; ai++) {
//             if (positions[a][ai][1] == tj) { // In final room, reduce cost
//                 cost -= positions[a][ai][0];
//             } else { // Move to top of final room, no collision
//                 cost += abs(positions[a][ai][0]);
//                 cost += abs(positions[a][ai][1] - tj);
//             }
//         }
//         total_cost += amph_move_cost(a + 'A') * cost;
//     }
//     return total_cost;
// }
// 
// static u64 heuristic(const burrow_t burrow)
// {
//     int positions[4][AMPHS_PER_TYPE][2];
//     find_amph_positions(burrow, positions);
//     return no_collision_cost(positions);
// } 
// 
// static void add_neighbor_to_search(dhashtable_t *costs, dheap_t *heap, burrow_t burrow, 
//                                    int si, int sj, int ti, int tj, int current_cost)
// {
//     u64 amph_move_cost = move(burrow, si, sj, ti, tj);
//     u64 neigh_enc = encode_burrow(burrow); // Zobrist for speed?
//     u64 tentative_cost = current_cost + amph_move_cost;
//     struct dhash_table_pair *p = dhashtable_lookup_or_insert(costs, neigh_enc, U64_MAX);
//     if (tentative_cost < p->value) {
//         // Less than old distance
//         p->value = tentative_cost;
//         dheap_add(heap, neigh_enc, tentative_cost + heuristic(burrow));
//     }
//     move_nocost(burrow, ti, tj, si, sj); // Undo the move
// }
// 
// 
// static void hallway_traverse(dhashtable_t *costs, dheap_t *heap, burrow_t burrow, 
//                              int si, int sj, int je, int jincr, int current_cost)
// {
//     for (int jj = sj + jincr; jj != je; jj += jincr) {
//         if (!is_room_j(jj)) { // Skip room columns
//             if (burrow[0][jj] == '.') { // Add if clear
//                 add_neighbor_to_search(costs, heap, burrow, si, sj, 0, jj,
//                                        current_cost);
//             } else {
//                 break; // Stop otherwise, hallway is blocked
//             }
//         }
//     }
// }
// 
// int main(void)
// {
//     static dheap_t heap; // the heap is huge!
//     dhashtable_t costs;
//     burrow_t burrow;
//     u64 enc;
// 
//     dhashtable_init(&costs);
// 
//     read_burrow(burrow);
//     
//     enc = encode_burrow(burrow);
//     dhashtable_insert(&costs, enc, 0);
//     dheap_add(&heap, enc, heuristic(burrow));
// 
//     // Time to dijkstra up!
//     while (!dheap_empty(&heap)) {
//         u64 current_f_cost = dheap_min(&heap);
//         u64 current_enc = dheap_min_key(&heap);
//         u64 current_cost;
//         struct dhash_table_pair *p;
//         dheap_pop_min(&heap);
//         decode_burrow(current_enc, burrow);
//         current_cost = current_f_cost - heuristic(burrow);
// 
//         // TODO: Final state check
//         if (amphipods_organized(burrow)) {
//             printf("%lu\n", current_cost);
//             break;;
//         }
// 
//         // Skip if already found better before
//         if ((p = dhashtable_lookup(&costs, current_enc)) && current_cost > p->value)
//             continue;
// 
//         // Now, need to find neighbors. Hard to make it abstract since I do not
//         // want to do extra alloc's.
// 
//         // First, check the hallway
//         for (int j = 0; j < BURROW_WIDTH; j++) {
//             if (is_amph(burrow[0][j])) {
//                 int target_i, target_j = target_room_j(burrow[0][j]);
//                 // Check for slot in the room and clear hallway
//                 if ((target_i = room_available_spot(burrow, target_j)) != -1
//                     && row_clear(burrow, j, target_j, 0))
//                 {
//                     add_neighbor_to_search(&costs, &heap, burrow, 0, j,
//                                            target_i, target_j, current_cost);
//                 }
//             }
//         }
// 
//         // Now, check the rooms
//         for (int j = 2; j <= 8; j += 2) {
//             for (int i = ROOM_HEIGHT; i > 0; i--) {
//                 if (is_amph(burrow[i][j]) && !amph_in_final_pos(burrow, i, j)
//                     && room_to_hallway_clear(burrow, i, j)) 
//                 {
//                     // Find clear positions in the hallway, both directions
//                     hallway_traverse(&costs, &heap, burrow, i, j, -1, -1, current_cost);
//                     hallway_traverse(&costs, &heap, burrow, i, j, BURROW_WIDTH, 1, current_cost);
//                 }
//             }
//         }
//     }
//     
//     fprintf(stderr, "States discovered: %lu\n", costs.num_entries);
//     dhashtable_destroy(&costs);
//     return 0;
// }

// A hard to compute and kind of pointless heuristic, bad idea! Assumes
// the amphipods can go through walls, which is not realistic...
// Useful for part1 because it's actually close to the no-collision cost.
// It breaks down hard in part2, and only saves like ~250 states out of 85k,
// and triples total  time due to its computation cost. Oof!
// u64 noclip_cost(int positions[4][AMPHS_PER_TYPE][2])
// {
// #if (AMPHS_PER_TYPE == 4)
//     static const int permutations[][4] = {
// 		{0,1,2,3}, {1,0,2,3}, {2,1,0,3}, {1,2,0,3}, {2,0,1,3}, {0,2,1,3},
// 		{3,2,1,0}, {2,3,1,0}, {2,1,3,0}, {3,1,2,0}, {1,3,2,0}, {1,2,3,0},
// 		{3,0,1,2}, {0,3,1,2}, {0,1,3,2}, {3,1,0,2}, {1,3,0,2}, {1,0,3,2},
// 		{3,0,2,1}, {0,3,2,1}, {0,2,3,1}, {3,2,0,1}, {2,3,0,1}, {2,0,3,1}
// 	};
// #elif (AMPHS_PER_TYPE == 2)
//     static const int permutations[][2] = { {0,1}, {1,0} };
// #else
// #error Unsupported AMPHS_PER_TYPE count.
// #endif
// 	static const int n_perms = sizeof(permutations) / sizeof(permutations[0]);
// 	// Try every permutation, assign minimum distance
//     u64 total_cost = 0;
//     for (int a = 0; a < 4; a++) {
//         int tj = target_room_j(a + 'A');
//         u64 min_cost = U64_MAX;
// 		for (int pi = 0; pi < n_perms; pi++) {
//             u64 cost = 0;
//             for (int ai = 0; ai < AMPHS_PER_TYPE; ai++) {
//                 int ti = permutations[pi][ai] + 1;
//                 cost += abs(positions[a][ai][0] - ti);
//                 cost += abs(positions[a][ai][1] - tj);
//             }
//             if (cost < min_cost)
//                 min_cost = cost;
//         }
//         assert (min_cost != U64_MAX);
//         total_cost += amph_move_cost(a + 'A') * min_cost;
//     }
//     return total_cost;
// }
