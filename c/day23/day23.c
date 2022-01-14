#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <assert.h>

#include "dheap/dheap.h"
#include "dhashtable/dhashtable.h"

#ifndef ROOM_HEIGHT
#error ROOM_HEIGHT must be defined.
#endif

#define HALLWAY_LEN     11
#define HW_COMPACT_LEN  7
#define AMPHS_PER_TYPE  ROOM_HEIGHT
#define BURROW_HEIGHT   (ROOM_HEIGHT + 3)
#define BURROW_WIDTH    (HALLWAY_LEN + 2)
#define BURROW_LEN      ((ROOM_HEIGHT * 4) + 7)
#define HW_LEFT_I       (ROOM_HEIGHT * 4)

// typedef struct {
//     char rooms[4][ROOM_HEIGHT]; // Initial room containers, like stacks
//     char room_size[4]; // 
//     char final_rooms[4];
//     char hallway[7];
//     char pad[1];
// } burrow_t;

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

// Since enum underlying types are int,
// I'll make my own crude enum with defines
#define EMPTY ((u16) 0)
#define A ((u16) 1)
#define B ((u16) 2)
#define C ((u16) 3)
#define D ((u16) 4)

// Try to pack values 0-5 into 4 bits each
typedef struct {
    u32 hallway; // 4 bits x 7 spots
    u16 room_stacks[4]; // 4 bits x 2/4 elements in each room, act as stacks
    u16 pad; // Exact pad to 16 bytes
    u16 room_fills; // 4 bits x 4 rooms
    u64 h_cost;
} burrow_t;

// Where are the sizes for the room stacks???
// They need not exist! We can infer them directly
// from the stacks themselves. 
// Empty stack: 0x0000
// One elem:    0x0002
// ...
// Four elems:  0x1234

#ifndef RELEASE
#define dbgassert(x) (assert(x))
#else
#define dbgassert(x)
#endif

static inline u16 get4(u32 x, int i)
{
    return (x >> (i << 2)) & 0xF;
}

static inline u32 clear4(u32 x, int i)
{
    return x & ~((u32) 0xF << (i << 2));
}

static inline u32 set4(u32 x, int i, u32 v)
{
    dbgassert (v <= 5);
    return x | (v << (i << 2));
}

static inline u32 incr4(u32 x, int i)
{
    return x + (1 << (i << 2));
}

// Don't call when the stack is empty!
static inline int stk_size(u16 stk)
{
    dbgassert (stk);
    if (stk > 0xFF) {
        if (stk > 0xFFF)
            return 4;
        else
            return 3;
    } else {
        if (stk > 0xF)
            return 2;
        else
            return 1;
    }
}

// Many ops! Only used for printing though
static u16 rev_stk(u16 stk)
{
    u16 rev = 0;
    while (stk) {
        rev <<= 4;
        rev |= stk & 0xF;
        stk >>= 4;
    }
    return rev;
}

#define U64_MAX (~((u64) 0))

static inline u16 ch2code(char c)
{
    dbgassert (c == '.' || ('A' <= c && c <= 'D'));
    if (c == '.')
        return EMPTY;
    else
        return c - 'A' + 1;
}

static inline char code2ch(u16 code)
{
    if (code == 0)
        return '.';
    else
        return 'A' + code - 1;
}

static inline u16 c2r(u16 code)
{
    return code - 1; // Sucks... But oh well!
}

static inline u16 r2c(u16 r)
{
    return r + 1;
}

// TODO: Become even more efficient!
// Encode '.' as 5 and A, B, C, D as 0, 1, 2, 3
// Pack whole rooms into numbers!
// Cache stuff!
// Anything goes...
    
static const int ROOM_JS[] = { 3, 5, 7, 9 };

static void read_burrow(burrow_t *burrow, int part2)
{
    char room_buf[ROOM_HEIGHT][16] = {0};
    int i, j, c, k;
    u32 x, sh;

    memset(burrow, 0, sizeof(*burrow));

    assert (scanf("%*[^\n]\n") == 0); // Skip first line

    // Read the hallway 
    x = i = sh = 0;
    getchar(); // Skip first #
    while ((c = getchar()) != '\n') {   
        if (c != '#' && (i % 2 == 1 || i == 0 || i == HALLWAY_LEN-1)) {
            x |= ch2code(c) << (sh << 2);
            sh++;
        }
        i++;
    } 
    burrow->hallway = x;
    assert (sh == 7);

    // Read the room, in other words (!): 空気を読め！
    
    // Read them all up for later scanning
    if (part2) {
        for (k = 0; k < ROOM_HEIGHT; k++) {
            if (k == 1) {
                strcpy(room_buf[k], "  #D#C#B#A#");
            } else if (k == 2) {
                strcpy(room_buf[k], "  #D#B#A#C#");
            } else {
                assert (fgets(room_buf[k], 16, stdin));
            }
        }
    } else {
        for (k = 0; k < ROOM_HEIGHT; k++)
            assert (fgets(room_buf[k], 16, stdin));
    }

    // Now, scan each room
    for (k = 0; k < 4; k++) { // Iterate rooms from 1 to 4
        u16 room_stack = 0, final_cnt = 0, is_final = 1;
        j = ROOM_JS[k];
        for (i = ROOM_HEIGHT - 1; i >= 0; i--) {
            int ch = room_buf[i][j];
            u16 code = ch2code(ch);
            // To deal with initial amphs in the final spot
            if (is_final && code == k + 1) {
                final_cnt++;
            } else {
                is_final = 0;
                room_stack <<= 4;
                room_stack |= code;
            }
        }
        assert (final_cnt <= ROOM_HEIGHT);
        burrow->room_stacks[k] = room_stack;
        burrow->room_fills |= (final_cnt << (k << 2));
    }

    assert (scanf("%*[^\n]\n") == 0); // Skip last line
}

void print_burrow_raw(const burrow_t *burrow)
{
    puts("Room stacks:");
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++)
            putchar(get4(burrow->room_stacks[i], j) + '0');
        putchar('\n');
    }
    printf("Room fills: ");
    for (int i = 0; i < 4; i++)
        putchar(get4(burrow->room_fills, i) + '0');
    putchar('\n');

    printf("Hallway: ");
    for (int i = 0; i < HW_COMPACT_LEN; i++)
        putchar(get4(burrow->hallway, i) + '0');
    putchar('\n');
}

void print_burrow(const burrow_t *burrow)
{
    int k;
    char grid[BURROW_HEIGHT][BURROW_WIDTH + 1];

    memset(grid, '#', sizeof(grid));

    for (int i = 0; i < BURROW_HEIGHT; i++)
        grid[i][BURROW_WIDTH] = '\n';

    for (int i = 3; i < BURROW_HEIGHT; i++)
        grid[i][0] = grid[i][1] = grid[i][BURROW_WIDTH-2] = grid[i][BURROW_WIDTH-1] = ' ';

    grid[BURROW_HEIGHT-1][BURROW_WIDTH] = 0; 

    k = 1;
    for (int j = 0; j < HW_COMPACT_LEN; j++) {
        u16 code = (burrow->hallway >> (j << 2)) & 0xF;
        grid[1][k++] = code2ch(code);
        if (1 <= j && j <= 4)
            grid[1][k++] = '.';
    }

    for (int r = 0; r < 4; r++) {
        int stk = rev_stk(burrow->room_stacks[r]);
        int i = BURROW_HEIGHT - 2;
        int j = ROOM_JS[r];
        char room_amph = code2ch(r2c(r));
        int fill = get4(burrow->room_fills, r);
        while (fill--)
            grid[i--][j] = room_amph;
        while (stk) {
            u16 code = stk & 0xF;
            stk >>= 4;
            grid[i--][j] = code2ch(code);
        }
        while (i > 0)
            grid[i--][j] = '.';
    }

    puts((const char *) grid);
}

static inline int is_amph(u16 code) 
{ 
    dbgassert (code <= 5);
    return code >= 1; 
}

static inline u64 amph_move_cost(u16 code)
{
    static u64 costs[] = { 0, 1, 10, 100, 1000 };
    dbgassert (1 <= code && code <= 5);
    return costs[code];
}

#define ROOMS_FILLED ((ROOM_HEIGHT << 12) | (ROOM_HEIGHT << 8)\
                      | (ROOM_HEIGHT << 4) | ROOM_HEIGHT)

static int amphipods_organized(const burrow_t *burrow)
{
    return burrow->room_fills == ROOMS_FILLED;
}


#define GOLDEN_RATIO_64 0x61C8864680B583EBull

static inline u64 hash_u64(u64 x)
{
    x = (x ^ (x >> 30)) * 0xbf58476d1ce4e5b9ULL;
    x = (x ^ (x >> 27)) * 0x94d049bb133111ebULL;
    x = x ^ (x >> 31);
    return x;
}

static u64 hash_burrow(const void *burrow)
{
    const u64 *arr = (const u64 *) burrow;
    u64 seed = hash_u64(arr[0]);
    seed ^= hash_u64(arr[1]) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
    return hash_u64(seed);
}

static int eq_burrow(const void *b1, const void *b2)
{
    const u64 *a1 = (const u64 *) b1, *a2 = (const u64 *) b2;
    return a1[0] == a2[0] && a1[1] == a2[1];
}

struct dhashtable_ops burrow_ops = {
    .hash = hash_burrow,
    .eq = eq_burrow
};

burrow_t *copy_burrow(const burrow_t *burrow)
{
    burrow_t *new = malloc(sizeof(*new));
    memcpy(new, burrow, sizeof(*burrow));
    return new;
}

// Horizontal cost:
// 0 1 | 2 | 3 | 4 | 5 6
// * * | * | * | * | * *
//     0   1   2   3
// How to calculate it?
// Let's convert indices with a lookup table and chill! v
static const u16 ROOM_X[] = { 2, 4, 6, 8 };
static const u16 HALLWAY_X[] = { 0, 1, 3, 5, 7, 9, 10 };

static inline u32 triang(u32 n) 
{ 
    static const u32 arr[] = { 0, 1, 3, 6, 10 };
    dbgassert(n <= 4);
    return arr[n]; 
}

static inline u64 max(u64 x, u64 y) { return x > y ? x : y; }

static u64 no_collision_cost(const burrow_t *burrow)
{
    u16 fills = burrow->room_fills;
    u64 cost = 1111 * triang(ROOM_HEIGHT);

    // Count the pods in the room stacks
    for (int r = 0; r < 4; r++) {
        u16 stk = burrow->room_stacks[r];
        u16 fc = get4(fills, r);
        dbgassert (fc <= 4);
        cost -= amph_move_cost(r2c(r)) * (triang(ROOM_HEIGHT) - triang(ROOM_HEIGHT - fc));
        if (stk) {
            int size = stk_size(stk);
            while (size) {
                u16 top = stk & 0xF;
                int target_r = c2r(top);
                u64 up = ROOM_HEIGHT + 1 - size - fc;
                u64 horiz = max(abs(r - target_r) << 1, 2); // Need to move aside and back even for own room
//                 printf("%lu %lu %lu\n", up, horiz, down);
                cost += amph_move_cost(top) * (up + horiz);
                stk >>= 4;
                size--;
            }
        }
    }

    // Count the pods in the hallway
    for (int i = 0; i < HW_COMPACT_LEN; i++) {
        u16 code = get4(burrow->hallway, i);
        if (is_amph(code)) {
            u16 target_r = c2r(code);
            u64 horiz = abs(ROOM_X[target_r] - HALLWAY_X[i]);
            cost += amph_move_cost(code) * horiz;
            fills = incr4(fills, target_r);
        }
    }

    return cost;
}

static u64 heuristic(const burrow_t *burrow)
{
    return no_collision_cost(burrow);
}

static void add_neighbor_to_search(dhashtable_t *costs, dheap_t *heap, 
                                   const burrow_t *neighbor, u64 tentative_cost)
{
    struct dhashtable_pair *p = dhashtable_lookup_or_insert(costs, neighbor, U64_MAX);
    if (tentative_cost < p->value) {
        // Less than old distance
        p->value = tentative_cost;
        dheap_add(heap, neighbor, tentative_cost + neighbor->h_cost);
    }
}

void day23_main(int part2)
{
    static dheap_t heap;
    burrow_t *initial_burrow = malloc(sizeof(*initial_burrow));
    dhashtable_t costs;

    dhashtable_init(&costs, &burrow_ops);

    read_burrow(initial_burrow, part2);
    initial_burrow->h_cost = heuristic(initial_burrow);
    dhashtable_insert(&costs, initial_burrow, 0);
    dheap_add(&heap, initial_burrow, initial_burrow->h_cost);

    while (!dheap_empty(&heap)) {
        u64 current_f_cost = dheap_min(&heap);
        struct dhashtable_pair *p;
        const burrow_t *burrow = dheap_min_key(&heap); // CAREFUL to restore if modif!!!
        u64 current_cost = current_f_cost - burrow->h_cost;
        dheap_pop_min(&heap);

        if (amphipods_organized(burrow)) {
            printf("%lu\n", current_cost);
            break;
        }

        if ((p = dhashtable_lookup(&costs, burrow)) && current_cost > p->value)
            continue;

        // Check for moves from rooms to hallways
        for (int room = 0; room < 4; room++) {
            u16 stk = burrow->room_stacks[room];
            if (stk) { // Amphs to empty out
                u16 top = stk & 0xF;
                u16 fill = get4(burrow->room_fills, room);
                u64 vert = ROOM_HEIGHT + 1 - stk_size(stk) - fill; // cost to get out
                u64 amc = amph_move_cost(top);

                u16 popd = stk >> 4;
                for (int l = room + 1; l >= 0; l--) {
                    if (get4(burrow->hallway, l)) { // != 0, contains a pod
                        break;
                    } else {
                        burrow_t *neighbor = copy_burrow(burrow);
                        u64 horiz = abs(ROOM_X[room] - HALLWAY_X[l]); 
                        u64 tent_cost = current_cost + amc * (vert + horiz);
                        neighbor->room_stacks[room] = popd;
                        neighbor->hallway = set4(burrow->hallway, l, top);

                        // Need to update the heuristic!
                        // Subtract vert since we're out of the room.
                        // Horizontal needs a more significant update:
                        // we may have gotten closer to the room, or further
                        // from it... Best to just change it completely.
                        int target_room = c2r(top);
                        u64 prev_horiz_diff = max(abs(room - target_room) << 1, 2); 
                        u64 new_horiz_diff = abs(HALLWAY_X[l] - ROOM_X[target_room]);
                        neighbor->h_cost -= amc * (vert + prev_horiz_diff);
                        neighbor->h_cost += amc * new_horiz_diff;
                        dbgassert (neighbor->h_cost == heuristic(neighbor));

                        add_neighbor_to_search(&costs, &heap, neighbor, tent_cost);
                    }
                }
                for (int r = room + 2; r < HW_COMPACT_LEN; r++) {
                    if (get4(burrow->hallway, r)) { // != 0, contains a pod
                        break;
                    } else {
                        // Spot exists, create neighbor & add it
                        burrow_t *neighbor = copy_burrow(burrow);
                        u64 horiz = abs(ROOM_X[room] - HALLWAY_X[r]); 
                        u64 tent_cost = current_cost + amc * (vert + horiz);
                        neighbor->room_stacks[room] = popd;
                        neighbor->hallway = set4(burrow->hallway, r, top);
                       
                        // Same as above
                        int target_room = c2r(top);
                        u64 prev_horiz_diff = max(abs(room - target_room) << 1, 2); 
                        u64 new_horiz_diff = abs(HALLWAY_X[r] - ROOM_X[target_room]);
                        neighbor->h_cost -= amc * (vert + prev_horiz_diff - new_horiz_diff);
                        dbgassert (neighbor->h_cost == heuristic(neighbor));

                        add_neighbor_to_search(&costs, &heap, neighbor, tent_cost);
                    }
                }
            }
        }

        // Now check for moves from the hallway to rooms
        for (int i = 0; i < HW_COMPACT_LEN; i++) {
            u32 hallway = burrow->hallway;
            u16 code = get4(hallway, i);
            if (is_amph(code)) {
                u16 room = c2r(code);
                u16 fill = get4(burrow->room_fills, room);
                if (!burrow->room_stacks[room]) { // Room is emptied and open for visits
                    // Still need to make sure the hallway is clear, though...
                    int roomi = room + 2; // Index just to right of room
                    int path_clear = 1;
                    if (i < roomi) { // To the left
                        for (int j = i + 1; j < roomi; j++) {
                            if (get4(hallway, j)) {
                                path_clear = 0;
                                break;
                            }
                        }
                    } else { // To the right
                        for (int j = i - 1; j >= roomi; j--) {
                            if (get4(hallway, j)) {
                                path_clear = 0;
                                break;
                            }
                        }
                    }

                    if (path_clear) {
                        burrow_t *neighbor = copy_burrow(burrow);
                        u64 hcost = abs(ROOM_X[room] - HALLWAY_X[i]);
                        u64 vcost = ROOM_HEIGHT - fill;
                        u64 move_cost = amph_move_cost(code) * (vcost + hcost);
                        u64 tent_cost = current_cost + move_cost;
                        neighbor->hallway = clear4(burrow->hallway, i);
                        neighbor->room_fills = incr4(burrow->room_fills, room);

                        // Updating the heuristic for this one is easier...
                        // Just subtract the move cost, we were not blocked anyway!
                        neighbor->h_cost -= move_cost;
                        dbgassert (neighbor->h_cost == heuristic(neighbor));

                        add_neighbor_to_search(&costs, &heap, neighbor, tent_cost);
                    }
                }
            }
        }
    }

    fprintf(stderr, "States explored: %lu\n", costs.num_entries);
    fprintf(stderr, "Hash collisions: %u\n", costs.collision_count);
    
    dhashtable_destroy(&costs);
}

