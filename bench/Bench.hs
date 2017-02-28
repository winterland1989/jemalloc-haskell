{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Criterion.Main

main :: IO ()
main = defaultMain [ ghc_alloc, malloc, jemalloc ]


