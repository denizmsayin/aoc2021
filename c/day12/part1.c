#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#define MAX_LEN 8
#define MAX_NODES 128

typedef unsigned char uchar;
typedef char name_t[MAX_LEN];

size_t node_add_or_get(name_t node_names[], size_t *nptr, name_t name)
{
    size_t i, n = *nptr;
    for (i = 0; i < n; i++)
        if (!strcmp(node_names[i], name))
            return i;
    strcpy(node_names[n], name);
    *nptr = n + 1;
    return n;
}

int is_small_cave(const char *name)
{
    const char *itr;
    for (itr = name; *itr; itr++)
        if (isupper(*itr))
            return 0;
    return 1;
}

struct graph {
    uchar adj_lists[MAX_NODES][MAX_NODES];
    size_t adj_lens[MAX_NODES];
    name_t node_names[MAX_NODES];
    char is_small[MAX_NODES];
    size_t num_nodes;
};

size_t count_paths(const struct graph *g, size_t from, size_t to, char visited[])
{
    if (from == to) {
        return 1;
    } else {
        size_t i, total = 0;
        for (i = 0; i < g->adj_lens[from]; i++) {
            size_t neighbor = g->adj_lists[from][i];
            if (!(g->is_small[neighbor] && visited[neighbor])) {
                visited[neighbor] = 1;
                total += count_paths(g, neighbor, to, visited);
                visited[neighbor] = 0;
            }
        }
        return total;
    }
}

int main(void)
{
    name_t buf1, buf2;
    struct graph g = {};
    char visited[MAX_NODES] = {};
    size_t i, start_i, end_i;

    while (scanf(" %[^-]-%s", buf1, buf2) != EOF) {
        size_t a = node_add_or_get(g.node_names, &g.num_nodes, buf1);
        size_t b = node_add_or_get(g.node_names, &g.num_nodes, buf2);
        g.adj_lists[a][g.adj_lens[a]++] = b;
        g.adj_lists[b][g.adj_lens[b]++] = a;
    }

    for (i = 0; i < g.num_nodes; i++)
        if (is_small_cave(g.node_names[i]))
            g.is_small[i] = 1;

    start_i = node_add_or_get(g.node_names, &g.num_nodes, "start");
    end_i = node_add_or_get(g.node_names, &g.num_nodes, "end");
    visited[start_i] = 1;
    printf("%lu\n", count_paths(&g, start_i, end_i, visited));

    return 0;
}
