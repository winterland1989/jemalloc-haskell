-----------------------------------------------------------------------------
-- |
-- Module      :  Data.ByteString.Jemalloc
-- Copyright   :  (c) Winterland 2017
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  drkoster@qq.com
-- Stability   :  provisional
-- Portability :  portable
--
--
-- This module provides operations to allocate 'ByteString' s with
-- <http://jemalloc.net/ jemalloc>. Finalizers are attched so that
-- once there's no reference to 'ByteString', memory will be freed
-- automatically, there is no guarantee of promptness though.
--
-----------------------------------------------------------------------------

module Data.ByteString.Jemalloc where

import Data.ByteString.Internal
import Foreign.Marshall.Jemalloc
import Foreign.ForeignPtr

-- | Create ByteString of given size with jemalloc and attach finalizer.
--
mallocByteString :: Int -> IO ByteString
mallocByteString size = do
    ptr <- mallocBytes size
    fptr <- newForeignPtr finalizerFree ptr
    return (PS fptr 0 size)
{-# INLINE mallocByteString #-}
