jemalloc-haskell
================

Haskell binding for [jemalloc](https://github.com/jemalloc/jemalloc).

Make sure you have built jemalloc and pkg-config can find it. Our benchmarks compared:

+ ghc block allocator
+ glib malloc(CentOS 7.0)
+ jemalloc

They performs pretty much the same. So unless you have special use cases, you probably won't need it. Please do benchmarks for your application to see if it's a improvement or not.
