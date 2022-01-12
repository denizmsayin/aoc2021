#include <stdio.h>
#include <assert.h>

#define MAX_LIGHTCUBOIDS 1048576

typedef unsigned char ubyte;

struct range {
    int s, e;
};

struct cuboid { 
    struct range xr, yr, zr;
};

struct lightcuboid {
    ubyte on;
    struct cuboid cuboid;
};

static inline int max(int a, int b) { return a > b ? a : b; }
static inline int min(int a, int b) { return a < b ? a : b; }

static int read_lightcuboid(struct lightcuboid *s)
{
    char buf[4];
    struct cuboid *c = &s->cuboid;
    int r = scanf(" %3s x=%d..%d,y=%d..%d,z=%d..%d", buf, 
                  &c->xr.s, &c->xr.e, &c->yr.s, &c->yr.e, &c->zr.s, &c->zr.e);
    
    if (r != 7)
        return 0;

    s->on = buf[0] == 'o' && buf[1] == 'n';

    return 1;
}

/*
fn intersection_1d(a: (i64, i64), b: (i64, i64)) -> Option<(i64, i64)> {
    if a.0 <= b.1 && b.0 <= a.1 {
        Some((max(a.0, b.0), min(a.1, b.1)))
    } else {
        None
    }
}

fn intersection(s0: &Cuboid, s1: &Cuboid) -> Option<Cuboid> {
    if let Some(xrange) = intersection_1d(s0.xrange, s1.xrange) {
        if let Some(yrange) = intersection_1d(s0.yrange, s1.yrange) {
            if let Some(zrange) = intersection_1d(s0.zrange, s1.zrange) {
                return Some(Cuboid { xrange, yrange, zrange });
            }
        }
    }
    None
}
*/

static int intersects_1d(struct range r1, struct range r2, struct range *r_out)
{
    if (r1.s <= r2.e && r2.s <= r1.e) {
        r_out->s = max(r1.s, r2.s);
        r_out->e = min(r1.e, r2.e);
        return 1;
    } else {
        return 0;
    }
}

static int intersects_3d(const struct cuboid *c1, const struct cuboid *c2, struct cuboid *c_out)
{
    return intersects_1d(c1->xr, c2->xr, &c_out->xr) &&
           intersects_1d(c1->yr, c2->yr, &c_out->yr) &&
           intersects_1d(c1->zr, c2->zr, &c_out->zr); 
}

typedef long long lli;

static inline lli len(struct range r) { return r.e - r.s + 1; }

static inline lli volume(const struct cuboid *c) 
{ 
    return len(c->xr) * len(c->yr) * len(c->zr);
}

void print_lightcuboid(const struct lightcuboid *l)
{
    const struct cuboid *c = &l->cuboid;
    printf("%s (%d->%d,%d->%d,%d->%d\n", l->on ? "on" : "off", c->xr.s, c->xr.e,
           c->yr.s, c->yr.e, c->zr.s, c->zr.e);
}

int main(void)
{
    static struct lightcuboid lcubs[MAX_LIGHTCUBOIDS];
    struct lightcuboid input_lcuboid;
    size_t ncubs = 0;
    lli total_vol = 0;

    while (read_lightcuboid(&input_lcuboid)) 
    {
        size_t j = 0; // how much to enlarge
        for (size_t i = 0; i < ncubs; i++) { // Try to intersect with all _previous_ ncuboids
            struct lightcuboid *intersection = &lcubs[ncubs + j];
            assert (ncubs + j < MAX_LIGHTCUBOIDS);
            if (intersects_3d(&input_lcuboid.cuboid, &lcubs[i].cuboid, &intersection->cuboid)) {
                intersection->on = !lcubs[i].on; // NOR of the two
                j++; // add the cuboid
            }
        }
        if (input_lcuboid.on) // only add on read cuboids
            lcubs[ncubs + (j++)] = input_lcuboid;
        ncubs += j; // enlarge the list of input_lcuboids
    }

    for (size_t i = 0; i < ncubs; i++) {
        lli vol = volume(&lcubs[i].cuboid);
        if (lcubs[i].on)
            total_vol += vol;
        else
            total_vol -= vol;
    }
        
    printf("%lld\n", total_vol);

    return 0;
}

