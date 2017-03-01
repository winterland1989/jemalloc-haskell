{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import Control.Concurrent.Async
import qualified Data.ByteString as B
import Data.ByteString.Internal (create)
import Control.Monad

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "No size number provided."
        [sizeStr] -> do
            let size = read sizeStr
            replicateM_ 3 $ do
                sums <- forConcurrently [1..10000] $ \ i -> do
                    bs <- create size (const $ return ())
                    return (B.foldl' (\ acc  _ -> acc + 1) 0 bs)
                print (sum sums)




