### Advent of Code 2021 in Three Languages: Rust, Haskell and C

* **Rust**: This is the first time I've used Rust, and learning a bit of Rust
was actually the reason I started out on this AoC. It was the primary
language I used while solving AoC, which means I was usually in a hurry. The
solutions are the first I could come up with, thus not always optimal, and the
code is not exactly idiomatic in some places. I did have a lot of fun though and
am looking forward to using Rust in other projects as a C++ alternative!
* **Haskell**:  Of course, I just had to try Haskell as a functional programming
alternative and see how different it feels. The approaches were _mostly_ the
same as those I used in Rust. It felt very smooth for many
problems, but suffered in some: mainly those that involve 2D arrays, and _there
are a lot of those buggers_. Performance suffered because I tried to keep things simple:
Always used lists where possible, and made of use `IntMap` for 1D and a
tuple-indexed `Map` for 2D arrays, generally avoiding strictness annotations.
Also, I made use of `State`'s monad instance quite a few of the later
problems if you're looking for examples.  Was fun!
* **C**: Finally, as a challenge, I wanted to see what I could do in C, which is
kind of barebones in terms of algorithms and data structures. 
I also set a goal to optimize my bad solutions and reach
comfortably below 50ms in C for every problem. I was quite successful in the end:
except for Day 23, every day uses only auto/static arrays as core data structures,
and the code does not contain a single dynamic allocation. I had to come up with a much
more optimized algorithm for Day 19, but I only had to do
extra optimization work for a few days:
    * Day 15: I used a _counting heap_ kind of structure instead of a binary
      heap for Dijkstra, since the range of values costs can have is small.
      Each cost index contains a mini-stack in which I put positions. Pushing
      is O(1), whereas popping min is O(MaxCost) in the worst case, but is fast
      in practice since costs only increase by very little every step.
    * Day 23: I had a lot of fun with this one, and ended up with a compact
      binary representation, used A* search with a heuristic computing the cost
      in assuming the amphipods could move through each other. Could stil be
      improved via pruning some locked states though! This is the only day in which I had to
      use actual data structures, with my own relatively generic binary heap
      and quadratic-probed hash-table implementations under lib/ saving the day.
    * Day 25: The initial naive implementation was around 50ms. I used a very
      simple alternative representation where I tracked the position of each
      sea cucumber, and looped over those instead of the whole grid. This
      took the timing down to around a relatively satisfying 15ms.

The timings of different days by language on my own laptop 
(Intel Core i7-8750H CPU, 16 GB RAM) are below. The timings are measured from
a shell script with two calls to `date` and are therefore far from sensitive,
since measurement takes a couple ms. They should be good to approximately +-5ms
though.

| Problem| Rust| Haskell| C|
| :---: | :---: | :---: | :---: |
| Day01 - Part 1 | 6ms | 9ms | 2ms |
| Day01 - Part 2 | 6ms | 8ms | 2ms |
| Day02 - Part 1 | 5ms | 9ms | 2ms |
| Day02 - Part 2 | 6ms | 9ms | 1ms |
| Day03 - Part 1 | 6ms | 10ms | 2ms |
| Day03 - Part 2 | 13ms | 9ms | 2ms |
| Day04 - Part 1 | 12ms | 11ms | 3ms |
| Day04 - Part 2 | 31ms | 16ms | 4ms |
| Day05 - Part 1 | 6ms | 58ms | 4ms |
| Day05 - Part 2 | 13ms | 119ms | 4ms |
| Day06 - Part 1 | 9ms | 61ms | 8ms |
| Day06 - Part 2 | 3ms | 5ms | 2ms |
| Day07 - Part 1 | 3ms | 8ms | 2ms |
| Day07 - Part 2 | 7ms | 11ms | 7ms |
| Day08 - Part 1 | 3ms | 7ms | 3ms |
| Day08 - Part 2 | 5ms | 11ms | 4ms |
| Day09 - Part 1 | 4ms | 16ms | 3ms |
| Day09 - Part 2 | 6ms | 29ms | 9ms |
| Day10 - Part 1 | 3ms | 5ms | 3ms |
| Day10 - Part 2 | 3ms | 7ms | 2ms |
| Day11 - Part 1 | 3ms | 12ms | 10ms |
| Day11 - Part 2 | 4ms | 16ms | 11ms |
| Day12 - Part 1 | 8ms | 8ms | 3ms |
| Day12 - Part 2 | 46ms | 89ms | 7ms |
| Day13 - Part 1 | 3ms | 9ms | 3ms |
| Day13 - Part 2 | 3ms | 13ms | 3ms |
| Day14 - Part 1 | 5ms | 14ms | 2ms |
| Day14 - Part 2 | 4ms | 6ms | 3ms |
| Day15 - Part 1 | 6ms | 23ms | 3ms |
| Day15 - Part 2 | 38ms | 1129ms | 15ms |
| Day16 - Part 1 | 3ms | 5ms | 2ms |
| Day16 - Part 2 | 3ms | 7ms | 2ms |
| Day17 - Part 1 | 3ms | 4ms | 10ms |
| Day17 - Part 2 | 5ms | 5ms | 11ms |
| Day18 - Part 1 | 6ms | 11ms | 8ms |
| Day18 - Part 2 | 49ms | 59ms | 25ms |
| Day19 - Part 1 | 1941ms | 8200ms | 21ms |
| Day19 - Part 2 | 1884ms | 8093ms | 21ms |
| Day20 - Part 1 | 4ms | 34ms | 3ms |
| Day20 - Part 2 | 18ms | 2181ms | 13ms |
| Day21 - Part 1 | 3ms | 3ms | 2ms |
| Day21 - Part 2 | 10ms | 186ms | 11ms |
| Day22 - Part 1 | 8ms | 795ms | 5ms |
| Day22 - Part 2 | 16ms | 29ms | 17ms |
| Day23 - Part 1 | 83ms | 1926ms | 4ms |
| Day23 - Part 2 | 104ms | 880ms | 33ms |
| Day24 - Part 1 | 2ms | 5ms | 2ms |
| Day24 - Part 2 | 3ms | 13ms | 6ms |
| Day25 - Part 1 | 77ms | 3358ms | 15ms |
| Day25 - Part 2 | 2ms | 3ms | 2ms |

I used the following auto-generated table to track my completion automatically 
by hooking automatic tests to every commit, and it's now complete! So, it's kind of useless,
but I can't part ways with it...

| Problem| Rust| Haskell| C|
| :---: | :---: | :---: | :---: |
| Day01 - Part 1 | ✅ | ✅ | ✅ |
| Day01 - Part 2 | ✅ | ✅ | ✅ |
| Day02 - Part 1 | ✅ | ✅ | ✅ |
| Day02 - Part 2 | ✅ | ✅ | ✅ |
| Day03 - Part 1 | ✅ | ✅ | ✅ |
| Day03 - Part 2 | ✅ | ✅ | ✅ |
| Day04 - Part 1 | ✅ | ✅ | ✅ |
| Day04 - Part 2 | ✅ | ✅ | ✅ |
| Day05 - Part 1 | ✅ | ✅ | ✅ |
| Day05 - Part 2 | ✅ | ✅ | ✅ |
| Day06 - Part 1 | ✅ | ✅ | ✅ |
| Day06 - Part 2 | ✅ | ✅ | ✅ |
| Day07 - Part 1 | ✅ | ✅ | ✅ |
| Day07 - Part 2 | ✅ | ✅ | ✅ |
| Day08 - Part 1 | ✅ | ✅ | ✅ |
| Day08 - Part 2 | ✅ | ✅ | ✅ |
| Day09 - Part 1 | ✅ | ✅ | ✅ |
| Day09 - Part 2 | ✅ | ✅ | ✅ |
| Day10 - Part 1 | ✅ | ✅ | ✅ |
| Day10 - Part 2 | ✅ | ✅ | ✅ |
| Day11 - Part 1 | ✅ | ✅ | ✅ |
| Day11 - Part 2 | ✅ | ✅ | ✅ |
| Day12 - Part 1 | ✅ | ✅ | ✅ |
| Day12 - Part 2 | ✅ | ✅ | ✅ |
| Day13 - Part 1 | ✅ | ✅ | ✅ |
| Day13 - Part 2 | ✅ | ✅ | ✅ |
| Day14 - Part 1 | ✅ | ✅ | ✅ |
| Day14 - Part 2 | ✅ | ✅ | ✅ |
| Day15 - Part 1 | ✅ | ✅ | ✅ |
| Day15 - Part 2 | ✅ | ✅ | ✅ |
| Day16 - Part 1 | ✅ | ✅ | ✅ |
| Day16 - Part 2 | ✅ | ✅ | ✅ |
| Day17 - Part 1 | ✅ | ✅ | ✅ |
| Day17 - Part 2 | ✅ | ✅ | ✅ |
| Day18 - Part 1 | ✅ | ✅ | ✅ |
| Day18 - Part 2 | ✅ | ✅ | ✅ |
| Day19 - Part 1 | ✅ | ✅ | ✅ |
| Day19 - Part 2 | ✅ | ✅ | ✅ |
| Day20 - Part 1 | ✅ | ✅ | ✅ |
| Day20 - Part 2 | ✅ | ✅ | ✅ |
| Day21 - Part 1 | ✅ | ✅ | ✅ |
| Day21 - Part 2 | ✅ | ✅ | ✅ |
| Day22 - Part 1 | ✅ | ✅ | ✅ |
| Day22 - Part 2 | ✅ | ✅ | ✅ |
| Day23 - Part 1 | ✅ | ✅ | ✅ |
| Day23 - Part 2 | ✅ | ✅ | ✅ |
| Day24 - Part 1 | ✅ | ✅ | ✅ |
| Day24 - Part 2 | ✅ | ✅ | ✅ |
| Day25 - Part 1 | ✅ | ✅ | ✅ |
| Day25 - Part 2 | ✅ | ✅ | ✅ |
