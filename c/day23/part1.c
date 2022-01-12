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


typedef char burrow_t[BURROW_HEIGHT][BURROW_WIDTH];

typedef uint64_t u64;
#define U64_MAX (~((u64) 0))

static void init_burrow(burrow_t burrow)
{
    memset(burrow, '#', sizeof(burrow[0][0]) * BURROW_WIDTH * BURROW_HEIGHT);
}

static void read_burrow(burrow_t burrow)
{
    init_burrow(burrow);

    assert (scanf("%*[^\n]\n") == 0); // Skip first line

    // Replace ' ' with '#' for simplicity
    for (size_t i = 0; i < BURROW_HEIGHT; i++) {
        int c;
        size_t j = 0;
        getchar(); // Skip first '#' or ' '
        while ((c = getchar()) != '\n')
            if (j < BURROW_WIDTH)
                burrow[i][j++] = c == ' ' ? '#' : c;
    }

    assert (scanf("%*[^\n]\n") == 0); // Skip last line
}

void print_burrow(const burrow_t burrow)
{
    for (size_t i = 0; i < BURROW_HEIGHT; i++) {
       for (size_t j = 0; j < BURROW_WIDTH; j++)
          putchar(burrow[i][j]);
       putchar('\n');
   } 
}

static u64 encode_burrow(const burrow_t burrow)
{
    u64 enc = 0;
    for (size_t i = 0; i < BURROW_HEIGHT; i++) {
        for (size_t j = 0 ; j < BURROW_WIDTH; j++) {
            char c = burrow[i][j];
            if (c == '#') {
                continue;
            } else if (c == '.') {
                enc *= 5;
            } else {
                enc = 5 * enc + c - 'A' + 1;
            }
        }
    }
    return enc;
}

static inline char decode_ch(u64 *enc)
{
    u64 enc_ch = *enc % 5;
    *enc /= 5;
    if (enc_ch == 0)
        return '.';
    return enc_ch - 1 + 'A';
}

static void decode_burrow(u64 enc, burrow_t burrow)
{
    init_burrow(burrow);

    // Decode side rooms
    for (size_t i = ROOM_HEIGHT; i > 0; i--)
        for (size_t j = 8; j >= 2; j -= 2)
            burrow[i][j] = decode_ch(&enc);

    // Decode hallway
    for (size_t j = BURROW_WIDTH; j --> 0;) // Ah yes, the 'goes to' operator :3
        burrow[0][j] = decode_ch(&enc);

    assert (enc == 0);
}

typedef long long lli;

static inline int is_amph(char c) { return 'A' <= c && c <= 'D'; }

static inline u64 move_cost(char c)
{
    switch (c) {
        case 'A': return 1;
        case 'B': return 10;
        case 'C': return 100;
        case 'D': return 1000;
        default: assert (0);
    }
}

static inline int target_room_j(char c)
{
    return 2 * (c - 'A' + 1);
}

static inline char room_target_amph(int j)
{
    return (j / 2) - 1 + 'A';
}

// Start not inclusive, end inclusive
int row_clear(const burrow_t burrow, int start, int end, int i)
{
    int inc = start > end ? -1 : 1;
    for (int j = start + inc; j != end; j += inc)
        if (burrow[i][j] != '.')
            return 0;
    return 1;
}

// Returns an index for an available i in the given room,
// otherwise returns -1, in case the room is not available
int room_available_spot(const burrow_t burrow, int j)
{
    char a = room_target_amph(j);
    int i = ROOM_HEIGHT, candidate;

    // Skip same amphs
    while (i > 0 && burrow[i][j] == a)
        i--;

    candidate = i;

    // Now, all remaining slots should be empty
    while (i > 0 && burrow[i][j] == '.')
        i--;

    if (i == 0)
        return candidate;
    return -1;
}

static inline void move_nocost(burrow_t burrow, int si, int sj, int ti, int tj)
{
    burrow[ti][tj] = burrow[si][sj];
    burrow[si][sj] = '.';
}

static inline u64 move(burrow_t burrow, int si, int sj, int ti, int tj)
{
    u64 dist = abs(si - ti) + abs(sj - tj);
    u64 cost = dist * move_cost(burrow[si][sj]);
    move_nocost(burrow, si, sj, ti, tj);
    return cost;
}

static int amph_in_final_pos(const burrow_t burrow, int i, int j)
{
    // To be in a final position, an amphipod must be
    // in its target room, and all amphipods below it
    // should be of the correct type as well.
    char a = burrow[i][j];
    if (j == target_room_j(a)) {
        for (int ii = i + 1; ii <= ROOM_HEIGHT; ii++)
            if (burrow[ii][j] != a)
                return 0;
        return 1;
    }
    return 0;
}

static int room_to_hallway_clear(const burrow_t burrow, int i, int j)
{
    for (int ii = i - 1; ii > 0; ii--)
        if (burrow[ii][j] != '.')
            return 0;
    return 1;
}

static int amphipods_organized(const burrow_t burrow)
{
    for (int j = 2; j <= 8; j += 2) {
        char a = room_target_amph(j);
        for (int i = ROOM_HEIGHT; i > 0; i--)
            if (burrow[i][j] != a)
                return 0;
    }
    return 1;
}

static inline int is_room_j(int j) { return j == 2 || j == 4 || j == 6 || j == 8; }

static void find_amph_positions(const burrow_t burrow, int positions[4][AMPHS_PER_TYPE][2])
{
    int inds[4] = {0};
    for (int i = 0; i < BURROW_HEIGHT; i++) {
        for (int j = 0; j < BURROW_WIDTH; j++) {
            if (is_amph(burrow[i][j])) {
                int a = burrow[i][j] - 'A';
                positions[a][inds[a]][0] = i;
                positions[a][inds[a]][1] = j;
                inds[a]++;
            }
        }
    }
//     for (int a = 0; a < 4; a++)
//         for (int ai = 0; ai < AMPHS_PER_TYPE; ai++)
//             printf("%d %d\n", positions[a][ai][0], positions[a][ai][1]);
    for (int i = 0; i < 4; i++)
        assert (inds[i] == ROOM_HEIGHT);
}

static u64 noclip_cost(int positions[4][AMPHS_PER_TYPE][2])
{
#if (AMPHS_PER_TYPE == 4)
    static const int permutations[][4] = {
		{0,1,2,3}, {1,0,2,3}, {2,1,0,3}, {1,2,0,3}, {2,0,1,3}, {0,2,1,3},
		{3,2,1,0}, {2,3,1,0}, {2,1,3,0}, {3,1,2,0}, {1,3,2,0}, {1,2,3,0},
		{3,0,1,2}, {0,3,1,2}, {0,1,3,2}, {3,1,0,2}, {1,3,0,2}, {1,0,3,2},
		{3,0,2,1}, {0,3,2,1}, {0,2,3,1}, {3,2,0,1}, {2,3,0,1}, {2,0,3,1}
	};
#elif (AMPHS_PER_TYPE == 2)
    static const int permutations[][2] = { {0,1}, {1,0} };
#else
#error Unsupported AMPHS_PER_TYPE count.
#endif
	static const int n_perms = sizeof(permutations) / sizeof(permutations[0]);
	// Try every permutation, assign minimum distance
    u64 total_cost = 0;
    for (int a = 0; a < 4; a++) {
        int tj = target_room_j(a + 'A');
        u64 min_cost = U64_MAX;
		for (int pi = 0; pi < n_perms; pi++) {
            u64 cost = 0;
            for (int ai = 0; ai < AMPHS_PER_TYPE; ai++) {
                int ti = permutations[pi][ai] + 1;
                cost += abs(positions[a][ai][0] - ti);
                cost += abs(positions[a][ai][1] - tj);
            }
            if (cost < min_cost)
                min_cost = cost;
        }
        assert (min_cost != U64_MAX);
        total_cost += move_cost(a + 'A') * min_cost;
    }
    return total_cost;
}

static u64 heuristic(const burrow_t burrow)
{
    int positions[4][AMPHS_PER_TYPE][2];
    find_amph_positions(burrow, positions);
    return noclip_cost(positions);
} 

static void add_neighbor_to_search(dhashtable_t *costs, dheap_t *heap, burrow_t burrow, 
                                   int si, int sj, int ti, int tj, int current_cost)
{
    u64 move_cost = move(burrow, si, sj, ti, tj);
    u64 neigh_enc = encode_burrow(burrow); // Zobrist for speed?
    u64 tentative_cost = current_cost + move_cost;
    struct dhash_table_pair *p = dhashtable_lookup_or_insert(costs, neigh_enc, U64_MAX);
    if (tentative_cost < p->value) {
        // Less than old distance
        p->value = tentative_cost;
        dheap_add(heap, neigh_enc, tentative_cost + heuristic(burrow));
    }
    move_nocost(burrow, ti, tj, si, sj); // Undo the move
}


static void hallway_traverse(dhashtable_t *costs, dheap_t *heap, burrow_t burrow, 
                             int si, int sj, int je, int jincr, int current_cost)
{
    for (int jj = sj + jincr; jj != je; jj += jincr) {
        if (!is_room_j(jj)) { // Skip room columns
            if (burrow[0][jj] == '.') { // Add if clear
                add_neighbor_to_search(costs, heap, burrow, si, sj, 0, jj,
                                       current_cost);
            } else {
                break; // Stop otherwise, hallway is blocked
            }
        }
    }
}

int main(void)
{
    static dheap_t heap; // the heap is huge!
    dhashtable_t costs;
    burrow_t burrow;
    u64 enc;

    dhashtable_init(&costs);

    read_burrow(burrow);
    
    enc = encode_burrow(burrow);
    dhashtable_insert(&costs, enc, 0);
    dheap_add(&heap, enc, heuristic(burrow));

    // Time to dijkstra up!
    while (!dheap_empty(&heap)) {
        u64 current_f_cost = dheap_min(&heap);
        u64 current_enc = dheap_min_key(&heap);
        u64 current_cost;
        struct dhash_table_pair *p;
        dheap_pop_min(&heap);
        decode_burrow(current_enc, burrow);
        current_cost = current_f_cost - heuristic(burrow);

        // TODO: Final state check
        if (amphipods_organized(burrow)) {
            printf("%lu\n", current_cost);
            break;;
        }

        // Skip if already found better before
        if ((p = dhashtable_lookup(&costs, current_enc)) && current_cost > p->value)
            continue;

        // Now, need to find neighbors. Hard to make it abstract since I do not
        // want to do extra alloc's.

        // First, check the hallway
        for (int j = 0; j < BURROW_WIDTH; j++) {
            if (is_amph(burrow[0][j])) {
                int target_i, target_j = target_room_j(burrow[0][j]);
                // Check for slot in the room and clear hallway
                if ((target_i = room_available_spot(burrow, target_j)) != -1
                    && row_clear(burrow, j, target_j, 0))
                {
                    add_neighbor_to_search(&costs, &heap, burrow, 0, j,
                                           target_i, target_j, current_cost);
                }
            }
        }

        // Now, check the rooms
        for (int j = 2; j <= 8; j += 2) {
            for (int i = ROOM_HEIGHT; i > 0; i--) {
                if (is_amph(burrow[i][j]) && !amph_in_final_pos(burrow, i, j)
                    && room_to_hallway_clear(burrow, i, j)) 
                {
                    // Find clear positions in the hallway, both directions
                    hallway_traverse(&costs, &heap, burrow, i, j, -1, -1, current_cost);
                    hallway_traverse(&costs, &heap, burrow, i, j, BURROW_WIDTH, 1, current_cost);
                }
            }
        }
    }
    
    fprintf(stderr, "States discovered: %lu\n", costs.num_entries);
    dhashtable_destroy(&costs);
    return 0;
}
