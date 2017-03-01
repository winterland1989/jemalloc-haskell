cabal build
echo "=============== start benchmark jemalloc ============="
time ./dist/build/jemalloc/jemalloc 128         +RTS -N1 -A128M -RTS
time ./dist/build/jemalloc/jemalloc 4096        +RTS -N1 -A128M -RTS
time ./dist/build/jemalloc/jemalloc 65536       +RTS -N1 -A128M -RTS
time ./dist/build/jemalloc/jemalloc 128         +RTS -N4 -A128M -RTS
time ./dist/build/jemalloc/jemalloc 4096        +RTS -N4 -A128M -RTS
time ./dist/build/jemalloc/jemalloc 65536       +RTS -N4 -A128M -RTS
time ./dist/build/jemalloc/jemalloc 128         +RTS -N24 -A128M -RTS
time ./dist/build/jemalloc/jemalloc 4096        +RTS -N24 -A128M -RTS
time ./dist/build/jemalloc/jemalloc 65536       +RTS -N24 -A128M -RTS
echo "=============== start benchmark haskell alloc ============="
time ./dist/build/ghc-alloc/ghc-alloc 128         +RTS -N1 -A128M -RTS
time ./dist/build/ghc-alloc/ghc-alloc 4096        +RTS -N1 -A128M -RTS
time ./dist/build/ghc-alloc/ghc-alloc 65536       +RTS -N1 -A128M -RTS
time ./dist/build/ghc-alloc/ghc-alloc 128         +RTS -N4 -A128M -RTS
time ./dist/build/ghc-alloc/ghc-alloc 4096        +RTS -N4 -A128M -RTS
time ./dist/build/ghc-alloc/ghc-alloc 65536       +RTS -N4 -A128M -RTS
time ./dist/build/ghc-alloc/ghc-alloc 128         +RTS -N24 -A128M -RTS
time ./dist/build/ghc-alloc/ghc-alloc 4096        +RTS -N24 -A128M -RTS
time ./dist/build/ghc-alloc/ghc-alloc 65536       +RTS -N24 -A128M -RTS
echo "=============== start system malloc ============="
time ./dist/build/malloc/malloc 128         +RTS -N1 -A128M -RTS
time ./dist/build/malloc/malloc 4096        +RTS -N1 -A128M -RTS
time ./dist/build/malloc/malloc 65536       +RTS -N1 -A128M -RTS
time ./dist/build/malloc/malloc 128         +RTS -N4 -A128M -RTS
time ./dist/build/malloc/malloc 4096        +RTS -N4 -A128M -RTS
time ./dist/build/malloc/malloc 65536       +RTS -N4 -A128M -RTS
time ./dist/build/malloc/malloc 128         +RTS -N24 -A128M -RTS
time ./dist/build/malloc/malloc 4096        +RTS -N24 -A128M -RTS
time ./dist/build/malloc/malloc 65536       +RTS -N24 -A128M -RTS
