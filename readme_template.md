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

@include mstimes.md

I used the following auto-generated table to track my completion automatically 
by hooking automatic tests to every commit, and it's now complete! So, it's kind of useless,
but I can't part ways with it...

@include correctness.md
