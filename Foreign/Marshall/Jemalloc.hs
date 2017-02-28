-----------------------------------------------------------------------------
-- |
-- Module      :  Foreign.Marshal.Jemalloc
-- Copyright   :  (c) The FFI task force 2001
--                (c) Winterland 2017
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  drkoster@qq.com
-- Stability   :  provisional
-- Portability :  portable
--
--
-- The module "Foreign.Marshal.Jemalloc" provides operations to allocate and
-- deallocate blocks of raw memory using <http://jemalloc.net jemalloc>
-- (i.e., unstructured chunks of memory -- outside of the area maintained
-- by the Haskell storage manager).  These
-- memory blocks are commonly used to pass compound data structures to
-- foreign functions or to provide space in which compound result values
-- are obtained from foreign functions.
--
-- If any of the allocation functions fails, an exception is thrown.
-- In some cases, memory exhaustion may mean the process is terminated.
--
-- NOTE: DON'T MIX operations from this module with the ones from
-- "Foreign.Marshall.Alloc" or any other @malloc(3)@ implementations.
-- If 'free' or 'reallocBytes' is applied to a memory area
-- that has been allocated with other implementations, the behaviour is undefined.
-- Any further access to these memory area referenced by a pointer passed to
-- 'realloc', 'reallocBytes', or 'free' entails undefined behaviour, and vice-versa.
--
-- All storage allocated by functions that allocate based on a /size in bytes/
-- must be sufficiently aligned for any of the basic foreign types
-- that fits into the newly allocated storage. All storage allocated by
-- functions that allocate based on a specific type must be sufficiently
-- aligned for that type. Array allocation routines need to obey the same
-- alignment constraints for each array element.
--
-----------------------------------------------------------------------------

module Foreign.Marshall.Jemalloc (
  -- ** Dynamic allocation
  malloc,
  mallocBytes,

  calloc,
  callocBytes,

  realloc,
  reallocBytes,

  free,
  finalizerFree
) where

import Data.Maybe
import Foreign.C.Types          ( CSize(..) )
import Foreign.Storable         ( Storable(sizeOf,alignment) )
import Foreign.ForeignPtr       ( FinalizerPtr )
import GHC.IO.Exception
import GHC.Real
import GHC.Ptr
import GHC.Base

--------------------------------------------------------------------------------

-- |Allocate a block of memory that is sufficient to hold values of type
-- @a@.  The size of the area allocated is determined by the 'sizeOf'
-- method from the instance of 'Storable' for the appropriate type.
--
-- The memory may be deallocated using 'free' or 'finalizerFree' when
-- no longer required.
--
{-# INLINE malloc #-}
malloc :: Storable a => IO (Ptr a)
malloc  = doMalloc undefined
  where
    doMalloc       :: Storable b => b -> IO (Ptr b)
    doMalloc dummy  = mallocBytes (sizeOf dummy)

-- |Like 'malloc' but memory is filled with bytes of value zero.
--
{-# INLINE calloc #-}
calloc :: Storable a => IO (Ptr a)
calloc = doCalloc undefined
  where
    doCalloc       :: Storable b => b -> IO (Ptr b)
    doCalloc dummy = callocBytes (sizeOf dummy)

-- |Allocate a block of memory of the given number of bytes.
-- The block of memory is sufficiently aligned for any of the basic
-- foreign types that fits into a memory block of the allocated size.
--
-- The memory may be deallocated using 'free' or 'finalizerFree' when
-- no longer required.
--
mallocBytes      :: Int -> IO (Ptr a)
mallocBytes size  = failWhenNULL "malloc" (_malloc (fromIntegral size))

-- |Llike 'mallocBytes' but memory is filled with bytes of value zero.
--
callocBytes :: Int -> IO (Ptr a)
callocBytes size = failWhenNULL "calloc" $ _calloc 1 (fromIntegral size)

-- |Resize a memory area that was allocated with 'malloc' or 'mallocBytes'
-- to the size needed to store values of type @b@.  The returned pointer
-- may refer to an entirely different memory area, but will be suitably
-- aligned to hold values of type @b@.  The contents of the referenced
-- memory area will be the same as of the original pointer up to the
-- minimum of the original size and the size of values of type @b@.
--
-- If the argument to 'realloc' is 'nullPtr', 'realloc' behaves like
-- 'malloc'.
--
realloc :: Storable b => Ptr a -> IO (Ptr b)
realloc  = doRealloc undefined
  where
    doRealloc           :: Storable b' => b' -> Ptr a' -> IO (Ptr b')
    doRealloc dummy ptr  = let
                             size = fromIntegral (sizeOf dummy)
                           in
                           failWhenNULL "realloc" (_realloc ptr size)

-- |Resize a memory area that was allocated with 'malloc' or 'mallocBytes'
-- to the given size.  The returned pointer may refer to an entirely
-- different memory area, but will be sufficiently aligned for any of the
-- basic foreign types that fits into a memory block of the given size.
-- The contents of the referenced memory area will be the same as of
-- the original pointer up to the minimum of the original size and the
-- given size.
--
-- If the pointer argument to 'reallocBytes' is 'nullPtr', 'reallocBytes'
-- behaves like 'malloc'.  If the requested size is 0, 'reallocBytes'
-- behaves like 'free'.
--
reallocBytes          :: Ptr a -> Int -> IO (Ptr a)
reallocBytes ptr 0     = do free ptr; return nullPtr
reallocBytes ptr size  =
  failWhenNULL "realloc" (_realloc ptr (fromIntegral size))

-- |Free a block of memory that was allocated with 'malloc',
-- 'mallocBytes', 'realloc', 'reallocBytes', 'Foreign.Marshal.Utils.new'
-- or any of the @new@/X/ functions in "Foreign.Marshal.Array" or
-- "Foreign.C.String".
--
free :: Ptr a -> IO ()
free  = _free

-- auxilliary routines
-- -------------------

-- asserts that the pointer returned from the action in the second argument is
-- non-null
--
failWhenNULL :: String -> IO (Ptr a) -> IO (Ptr a)
failWhenNULL name f = do
   addr <- f
   if addr == nullPtr
      then ioError (IOError Nothing ResourceExhausted name
                                        "out of memory" Nothing Nothing)
      else return addr

-- basic C routines needed for memory allocation
--
foreign import ccall unsafe "jemalloc.h malloc"  _malloc  ::          CSize -> IO (Ptr a)
foreign import ccall unsafe "jemalloc.h calloc"  _calloc  :: CSize -> CSize -> IO (Ptr a)
foreign import ccall unsafe "jemalloc.h realloc" _realloc :: Ptr a -> CSize -> IO (Ptr b)
foreign import ccall unsafe "jemalloc.h free"    _free    :: Ptr a -> IO ()

-- | A pointer to a foreign function equivalent to 'free', which may be
-- used as a finalizer (cf 'Foreign.ForeignPtr.ForeignPtr') for storage
-- allocated with 'malloc', 'mallocBytes', 'realloc' or 'reallocBytes'.
foreign import ccall unsafe "jemalloc.h &free" finalizerFree :: FinalizerPtr a
