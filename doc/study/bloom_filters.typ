= Ultra-Fast Bloom Filters using SIMD Techniques (2018)
https://ieeexplore.ieee.org/document/8462781

Instead of doing k random accesses, organize everything into blocks:

something like this:

`blocks[h0(data)] & h1234(data) == h1234(data)`

each access is guaranteed to only access a single cache line.


= Bloom filters (blog)
https://florian.github.io/bloom-filters/

We can do set operations on bloom filters.
Counting bloom filters allow removal.



