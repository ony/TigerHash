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

{-# LANGUAGE FlexibleInstances #-}
module Data.Digest.TigerHash.ByteString () where

import Data.Digest.TigerHash

import Data.Word

import Foreign.Ptr
import Foreign.ForeignPtr

import System.IO.Unsafe

import Control.Monad

import qualified Data.Digest.TigerHash.Internal as T

import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Lazy.Internal as L
import qualified Data.ByteString.Lazy as LBS

calcHashS :: ForeignPtr T.StreamState -> S.ByteString -> IO TigerHash
calcHashL :: ForeignPtr T.StreamState -> L.ByteString -> IO TigerHash

calcHashS ctx (S.PS _ _ 0) = T.finalizeContext ctx
calcHashS ctx (S.PS p s l) = withForeignPtr p $ \p_ -> do
    T.updateContext ctx (p_ `plusPtr` s) (l*8)
    T.finalizeContext ctx
{-# INLINE calcHashS #-}

calcHashL ctx lbs = do
        {-
        go ctx lbs where
            go ctx lbs = walk lbs where
                walk L.Empty = T.finalizeContext ctx
                walk (L.Chunk (S.PS _ _ 0) cs) = walk cs
                walk (L.Chunk (S.PS p s l) cs) = withForeignPtr p (\p_ -> T.updateContext ctx p_ ((l-1)*8)) >> walk cs

        -}
        let updateBS (_, _, 0) m = m
            updateBS (p, s, l) m = withForeignPtr p (\p_ -> T.updateContext ctx (p_ `plusPtr` s) (l*8)) >> m
            updatePlan = L.foldrChunks (updateBS . S.toForeignPtr) (return ()) where
        updatePlan lbs
        T.finalizeContext ctx
{-# INLINE calcHashL #-}

instance TigerHashable S.ByteString where
    tigerHash bs = S.inlinePerformIO $ T.newContext >>= flip calcHashS bs
    {-# INLINE tigerHash #-}

    tigerHashList xs@(_:_) = S.inlinePerformIO $ do
            ctx <- T.newContext
            {-
            let safeCalc x = unsafeInterleaveIO $ do
                h <- calcHashS ctx x
                T.resetContext ctx -- reset to initial state
                return h
            mapM safeCalc xs
            -}
            let go [x] = unsafeInterleaveIO $ do
                    hash <- calcHashS ctx x
                    return [hash]
                go (x:xs) = unsafeInterleaveIO $ do
                    hash <- calcHashS ctx x
                    T.resetContext ctx
                    liftM (hash:) (go xs)
            go xs
    {-# INLINE tigerHashList #-}

instance TigerHashable L.ByteString where
    tigerHash lbs = S.inlinePerformIO $ T.newContext >>= flip calcHashL lbs
    {-# INLINE tigerHash #-}

    tigerHashList [] = []
    tigerHashList xs = S.inlinePerformIO $ do
            ctx <- T.newContext
            {-
            let go [x] = do
                    hash <- calcHashL ctx x
                    return [hash]
                go (x:xs) = do
                    hash <- calcHashL ctx x
                    T.resetContext ctx
                    return . (hash:) =<< go xs
            go xs
            -}
            let go [x] = unsafeInterleaveIO $ do
                    hash <- calcHashL ctx x
                    return [hash]
                go (x:xs) = unsafeInterleaveIO $ do
                    hash <- calcHashL ctx x
                    T.resetContext ctx
                    liftM (hash:) (go xs)
            go xs

    {-# INLINE tigerHashList #-}

instance TigerHashable [Word8] where
    tigerHash xs = tigerHash $ LBS.pack xs
    {-# INLINE tigerHash #-}
    tigerHashList xs = tigerHashList $ map LBS.pack xs
    {-# INLINE tigerHashList #-}


-- test1 = take 10 . tigerHashList $ repeat [0::Word8,1,2]
