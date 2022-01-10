#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>        
#include <limits.h>
#include <assert.h>

typedef long long int lli;

#define MAX_VYN 

struct npair {
    lli nmin, nmax;
};

#define VY_MIN (-250000LL)
#define VY_MAX 250000LL
#define NPAIR_CACHE_SIZE ((size_t) (VY_MAX - VY_MIN + 1))

static inline size_t vy2i(lli vy) { assert (VY_MIN <= vy && vy <= VY_MAX); return vy - VY_MIN; }
static inline lli i2vy(size_t i) { assert (i < NPAIR_CACHE_SIZE); return (lli) i + VY_MIN; }

void init_npair_cache(struct npair *cache)
{
    for (size_t i = 0; i < NPAIR_CACHE_SIZE; i++) {
        cache[i].nmin = LLONG_MAX;
        cache[i].nmax = LLONG_MIN;
    }
}

void npair_update(struct npair *p, lli n)
{
    if (n < p->nmin)
        p->nmin = n;
    if (n > p->nmax)
        p->nmax = n;
}

int npair_exists(const struct npair *p)
{
    return p->nmin != LLONG_MAX && p->nmax != LLONG_MIN;
}

// Ideally, I need to keep track of found nmin and nmax only for
// a given vy. Since I do not want to implement a fancy hash table
// just for this, I'm taking the huge array approach. There goes
// cache performance!
void find_viable_vy_n(lli y_target, struct npair *nvalues)
{
    // Equation: 2 * y_target = n * (2*vy - n + 1)
    lli lhs = 2 * y_target;
    lli lhs_abs = llabs(lhs);
    for (lli div = 1; div * div <= lhs_abs; div++) {
        if (lhs % div == 0) {
            // First try: div as candidate n
            lli n_candidate = div;
            lli factor = lhs / n_candidate;
            lli vy_twice = factor + n_candidate - 1;
            if (vy_twice % 2 == 0) {
                lli vy = vy_twice / 2;
                npair_update(&nvalues[vy2i(vy)], n_candidate);
            }
            // Second: Other way around
            if (div * div != lhs_abs) {
                n_candidate = lhs_abs / div;
                factor = lhs / n_candidate;
                vy_twice = factor + n_candidate - 1;
                if (vy_twice % 2 == 0) {
                    lli vy = vy_twice / 2;
                    npair_update(&nvalues[vy2i(vy)], n_candidate);
                }
            }
        }
    }
}

// From Wikipedia
lli isqrt(lli s)
{
	lli x0 = s / 2;			// Initial estimate

	// Sanity check
	if (x0 != 0) {
		lli x1 = (x0 + s / x0) / 2;	// Update
		
		while (x1 < x0)				// This also checks for cycle
		{
			x0 = x1;
			x1 = (x0 + s / x0) / 2;
		}
		
		return x0;
	} else {
		return s;
	}
}

static inline lli cdiv(lli a, lli b) { return (a + b - 1) / b; }
static inline lli max(lli a, lli b) { return a > b ? a : b; }
static inline lli min(lli a, lli b) { return a < b ? a : b; }
static inline lli triang(lli n) { return (n * (n + 1)) / 2; }

lli triroot_f(lli x) 
{
    return (isqrt(1 + 8 * x) - 1) / 2;
}

lli triroot_c(lli x)
{
    lli f = triroot_f(x);
    return (triang(f) == x) ? f : f + 1;
}

int viable_vx_exists(lli xmin, lli xmax, const struct npair *npair)
{
    lli nmin = npair->nmin, nmax = npair->nmax;
    // Crude iteration 
    for (lli n = nmin; n <= nmax; n++) {
        // First inequality: (n <= vx)
        // xmin + tri(n-1) <= n*vx <= xmax + tri(n-1)
        lli tri = triang(n-1);
        lli lower_bound = max(n, cdiv(xmin + tri, n));
        lli upper_bound = (xmax + tri) / n;
        if (lower_bound <= upper_bound)
            return 1;
        // Second inequality: (vx < n)
        // xmin <= vx * (vx + 1) / 2 <= xmax
        lower_bound = triroot_c(xmin);
        upper_bound = min(n - 1, triroot_f(xmax));
        if (lower_bound <= upper_bound)
            return 1;
    }
    return 0;
}

int main(void)
{
    static struct npair vy_pairs[NPAIR_CACHE_SIZE];
    lli xs, xe, ys, ye;

    init_npair_cache(vy_pairs);
    
    if (scanf("target area: x=%lld..%lld, y=%lld..%lld", &xs, &xe, &ys, &ye) < 0)
        perror("scanf failed");
    
    for (lli y_target = ys; y_target <= ye; y_target++)
        find_viable_vy_n(y_target, vy_pairs);

    for (size_t i = NPAIR_CACHE_SIZE - 1; i-- != 0;) {
        const struct npair *pair = &vy_pairs[i];
        if (npair_exists(pair) && viable_vx_exists(xs, xe, pair)) {
            lli vy = i2vy(i);
            lli max_h = triang(vy);
            printf("%lld\n", max_h);
            return 0;
        }
    }

    return 0;
}

