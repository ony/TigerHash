{-
    DC++ protocl utls for GHC
    Copyright (C) 2009 Nikolay Orlyuk (virkony _at_ gmail _dot_ com)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

 -}

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
module Data.Digest.TigerHash.Internal (
    TigerHash(..), StreamState,
    newContext,
    updateContext,
    resetContext,
    finalizeContext
    ) where

import Foreign
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types

data TigerHash = TigerHash {-# UNPACK #-} !Word64
                           {-# UNPACK #-} !Word64
                           {-# UNPACK #-} !Word64


-- Level 1

newContext :: IO (ForeignPtr StreamState)
newContext = newContext_ >>= newForeignPtr freeContext_

updateContext :: ForeignPtr StreamState -> Ptr a -> Int -> IO ()
-- updateContext ctx p s = withForeignPtr ctx (\ctx_ -> withForeignPtr p (\p_ -> updateContext_ ctx_ p_ s))
updateContext ctx p_ s = withForeignPtr ctx (\ctx_ -> updateContext_ ctx_ p_ (fromIntegral s))

resetContext :: ForeignPtr StreamState -> IO ()
resetContext ctx = withForeignPtr ctx (\ctx_ -> resetContext_ ctx_)

finalizeContext :: ForeignPtr StreamState -> IO TigerHash
finalizeContext ctx = do
        p <- (mallocForeignPtrArray 3)
        withForeignPtr ctx (\ctx_ -> withForeignPtr p (internal ctx_))
    where
        internal ctx_ p_ = do
            finalizeContext_ ctx_ p_
            a <- peekElemOff p_ 0
            b <- peekElemOff p_ 1
            c <- peekElemOff p_ 2
            return (TigerHash a b c)


-- Level 0

data StreamState

{-# CFILES c_lib/tiger.c #-}

foreign import ccall unsafe "tiger.h tiger_new" newContext_ :: IO (Ptr StreamState)
foreign import ccall unsafe "tiger.h &tiger_free" freeContext_ :: FinalizerPtr StreamState

foreign import ccall unsafe "tiger.h tiger_update" updateContext_ :: Ptr StreamState -> Ptr a -> CSize -> IO ()
foreign import ccall unsafe "tiger.h tiger_finalize" finalizeContext_ :: Ptr StreamState -> Ptr Word64 -> IO ()
foreign import ccall unsafe "tiger.h tiger_reset" resetContext_ :: Ptr StreamState -> IO ()

-- ex:syntax=haskell
