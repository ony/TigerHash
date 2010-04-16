{-# LANGUAGE ForeignFunctionInterface,FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
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

module Data.Digest.TigerHash.Internal (
    TigerHash(..), TigerContext(..), TigerState, TigerTreeState,
    newTigerContext,
    newTigerTreeContext,
    withTigerContext, withTigerTreeContext,
    ) where

import Foreign
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.C.Types

data TigerHash = TigerHash {-# UNPACK #-} !Word64
                           {-# UNPACK #-} !Word64
                           {-# UNPACK #-} !Word64


-- Level 1

class TigerContext c where
    initContext :: c -> IO ()
    updateContext :: c -> Ptr a -> Int -> IO ()
    resetContext :: c -> IO ()
    finalizeContext :: c -> IO TigerHash

newTigerContext :: IO (ForeignPtr TigerState)
newTigerContext = do
    ctx <- mallocBytes sizeofTigerContext >>= newForeignPtr finalizerFree
    initContext ctx
    return ctx

newTigerTreeContext :: IO (ForeignPtr TigerTreeState)
newTigerTreeContext = do
    ctx <- mallocBytes sizeofTigerTreeContext >>= newForeignPtr finalizerFree
    initContext ctx
    return ctx

withTigerContext :: (Ptr TigerState -> IO a) -> IO a
withTigerContext actions = allocaBytes sizeofTigerContext (\ctx_ -> initContext ctx_ >> actions ctx_)

withTigerTreeContext :: (Ptr TigerTreeState -> IO a) -> IO a
withTigerTreeContext actions = allocaBytes sizeofTigerTreeContext (\ctx_ -> initContext ctx_ >> actions ctx_)

instance TigerContext (Ptr TigerState) where
    initContext ctx_ = initTigerContext_ ctx_
    updateContext ctx_ p_ s = updateTigerContext_ ctx_ p_ (fromIntegral s)
    resetContext ctx_ = resetTigerContext_ ctx_
    finalizeContext ctx_ = allocaArray 3 internal
        where
            internal p_ = do
                finalizeTigerContext_ ctx_ p_
                a <- peekElemOff p_ 0
                b <- peekElemOff p_ 1
                c <- peekElemOff p_ 2
                return (TigerHash a b c)

instance TigerContext (Ptr TigerTreeState) where
    initContext ctx_ = initTigerTreeContext_ ctx_
    updateContext ctx_ p_ s = updateTigerTreeContext_ ctx_ p_ (fromIntegral s)
    resetContext ctx_ = resetTigerTreeContext_ ctx_
    finalizeContext ctx_ = allocaArray 3 internal
        where
            internal p_ = do
                finalizeTigerTreeContext_ ctx_ p_
                a <- peekElemOff p_ 0
                b <- peekElemOff p_ 1
                c <- peekElemOff p_ 2
                return (TigerHash a b c)

instance (TigerContext (Ptr a)) => TigerContext (ForeignPtr a) where
    initContext ctx = withForeignPtr ctx initContext
    updateContext ctx p_ s = withForeignPtr ctx (\ctx_ -> updateContext ctx_ p_ s)
    resetContext ctx = withForeignPtr ctx resetContext
    finalizeContext ctx = withForeignPtr ctx finalizeContext


-- Level 0

data TigerState
data TigerTreeState

{-# CFILES c_lib/tiger.c #-}

foreign import ccall unsafe "tiger.h tiger_context_size" sizeofTigerContext :: Int
foreign import ccall unsafe "tiger.h tiger_new" newTigerContext_ :: IO (Ptr TigerState)
foreign import ccall unsafe "tiger.h tiger_init" initTigerContext_ :: Ptr TigerState -> IO ()
foreign import ccall unsafe "tiger.h &tiger_free" freeTigerContext_ :: FinalizerPtr TigerState

foreign import ccall unsafe "tiger.h tiger_feed" updateTigerContext_ :: Ptr TigerState -> Ptr a -> CSize -> IO ()
foreign import ccall unsafe "tiger.h tiger_finalize" finalizeTigerContext_ :: Ptr TigerState -> Ptr Word64 -> IO ()
foreign import ccall unsafe "tiger.h tiger_reset" resetTigerContext_ :: Ptr TigerState -> IO ()

{-# CFILES c_lib/tigertree.c #-}

foreign import ccall unsafe "tigertree.h tigertree_context_size" sizeofTigerTreeContext :: Int
foreign import ccall unsafe "tigertree.h tigertree_new" newTigerTreeContext_ :: IO (Ptr TigerTreeState)
foreign import ccall unsafe "tigertree.h tigertree_init" initTigerTreeContext_ :: Ptr TigerTreeState -> IO ()
foreign import ccall unsafe "tigertree.h &tigertree_free" freeTigerTreeContext_ :: FinalizerPtr TigerTreeState

foreign import ccall unsafe "tigertree.h tigertree_feed" updateTigerTreeContext_ :: Ptr TigerTreeState -> Ptr a -> CSize -> IO ()
foreign import ccall unsafe "tigertree.h tigertree_finalize" finalizeTigerTreeContext_ :: Ptr TigerTreeState -> Ptr Word64 -> IO ()
foreign import ccall unsafe "tigertree.h tigertree_reset" resetTigerTreeContext_ :: Ptr TigerTreeState -> IO ()

-- ex:syntax=haskell
