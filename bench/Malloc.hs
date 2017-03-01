{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import Control.Concurrent.Async
import qualified Data.ByteString as B
import Data.ByteString.Internal (ByteString(..))
import Foreign.Marshal.Alloc
import Foreign.ForeignPtr

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "No size number provided."
        [sizeStr] -> do
            let size = read sizeStr
            sums <- forConcurrently [1..10000] $ \ i -> do
                bs <- mallocByteString size
                return (B.foldl' (\ acc  _ -> acc + 1) 0 bs)
            print (sum sums)

mallocByteString :: Int -> IO ByteString
mallocByteString size = do
    ptr <- mallocBytes size
    fptr <- newForeignPtr finalizerFree ptr
    return (PS fptr 0 size)
{-# INLINE mallocByteString #-}
