#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>        
#include <string.h>
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
	lli x0 = s / 2;

	if (x0 != 0) {
		lli x1 = (x0 + s / x0) / 2;	// Update
		
		while (x1 < x0)	{
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
    lli f = (isqrt(1 + 8 * x) - 1) / 2;
    return f;
}

lli triroot_c(lli x)
{
    lli f = triroot_f(x);
    lli c = triang(f) == x ? f : f + 1;
    return c;
}


lli count_viable_vxs(lli xmin, lli xmax, const struct npair *npair)
{
    // There's a small mistake somewhere according to big inputs. Hmm!
    // https://the-tk.com/project/aoc2021-bigboys.html#day-17
    
    lli nmin = npair->nmin, nmax = npair->nmax;
    lli lower_bound = LLONG_MAX;
    lli upper_bound = LLONG_MIN;
    lli count = 0;

    // First inequality: (n <= vx)
    // xmin + tri(n-1) <= n*vx <= xmax + tri(n-1)
    // Have to find loose bounds via crude iteration
    // Could probably use derivatives, but n-ranges are
    // not too large anyway, so it's fine.
    for (lli n = nmin; n <= nmax; n++) {
        lli tri = triang(n - 1);
        lli lb = max(n, cdiv(xmin + tri, n));
        lli ub = (xmax + tri) / n;
        
        // Simply taking min of all lower bounds
        // and max of all upper bounds is a little naive, but not too much!
        // We assume that all the ranges always intersect.
        // Let's check that assumption.
        assert (lower_bound == LLONG_MAX || lb > ub || (lb <= upper_bound && lower_bound <= ub));

        // Only update for valid intervals
        if (lb <= ub) {
            lower_bound = min(lower_bound, lb);
            upper_bound = max(upper_bound, ub);
        }
    }

    // Only add to count if updated; subtraction will overflow
    if (lower_bound != LLONG_MAX && upper_bound != LLONG_MIN)
        count += max(0, upper_bound - lower_bound + 1);

    // Second inequality: (vx < n)
    // xmin <= vx * (vx + 1) / 2 <= xmax
    upper_bound = min(min(nmax, lower_bound) - 1, triroot_f(xmax));
    lower_bound = triroot_c(xmin);
    count += max(0, upper_bound - lower_bound + 1);
    
    return count;
}

int main(void)
{
    static struct npair vy_pairs[NPAIR_CACHE_SIZE];
    lli xs, xe, ys, ye;
    lli count = 0;

    init_npair_cache(vy_pairs);
    
    if (scanf("target area: x=%lld..%lld, y=%lld..%lld", &xs, &xe, &ys, &ye) < 0)
        perror("scanf failed");
    
    for (lli y_target = ys; y_target <= ye; y_target++)
        find_viable_vy_n(y_target, vy_pairs);

    for (size_t i = NPAIR_CACHE_SIZE - 1; i-- != 0;) {
        const struct npair *pair = &vy_pairs[i];
        if (npair_exists(pair)) {
            lli viable_vxs = count_viable_vxs(xs, xe, pair);
            count += viable_vxs;
        }
    }

    printf("%lld\n", count);

    return 0;
}

/* Naive counter used for debugging the good counter */
/*
#define VX_MAX 1000000

void cvx_naive_(lli xmin, lli xmax, lli n, char *vxs)
{
    for (lli x = xmin; x <= xmax; x++) {
        lli nvx = (x + triang(n - 1));
        if (nvx % n == 0) {
            lli vx = nvx / n;
            if (n <= vx) {
                assert (vx <= VX_MAX);
                vxs[vx] = 1;
            }
        }
        lli f = triroot_f(x);
        if (triang(f) == x) {
            lli vx = f;
            if (n > vx) {
                assert (vx <= VX_MAX);
                vxs[vx] = 1;
            }
        }
    }
}

lli cvx_naive(lli xmin, lli xmax, const struct npair *npair)
{
    static char vxs[VX_MAX+1];
    lli count = 0;
    memset(vxs, 0, sizeof(vxs));
    
    for (lli n = npair->nmin; n <= npair->nmax; n++)
        cvx_naive_(xmin, xmax, n, vxs);

    for (lli i = 0; i < VX_MAX + 1; i++) {
        if (vxs[i])
            printf("%lld,", i);
        count += vxs[i];
    }
    putchar('\n');

    return count;
}
*/
