{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import Control.Concurrent.Async
import qualified Data.ByteString as B
import Data.ByteString.Internal (ByteString(..))
import Data.ByteString.Internal (create)
import Control.Monad
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Data.Word

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "No size number provided."
        [sizeStr] -> do
            let size = read sizeStr
            replicateM_ 3 $ do
                sums <- forConcurrently [1..10000] $ \ i -> do
                    bs@(PS fp _ _) <- create size (const $ return ())
                    withForeignPtr fp $ \ ptr -> init ptr size 0
                    return (B.foldl' (\ acc  _ -> acc + 1) 0 bs)
                print (sum sums)
  where
    init :: Ptr Word8 -> Int -> Int -> IO ()
    init p n i  =
        when (i < n) $ poke p 0xff >> init (p `plusPtr` 1) n (i+1)





