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

{-# LANGUAGE BangPatterns #-}
module Data.Digest.TigerHash (TigerHash, TigerHashable(..), hexTigerHash, b32TigerHash) where
import System.IO.Unsafe
import Foreign.ForeignPtr
import Foreign.Ptr

import Text.Show
import Data.ByteString.Internal (inlinePerformIO)
import qualified Data.ByteString.Lazy as LBS
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import qualified Codec.Binary.Base16 as B16
import qualified Codec.Binary.Base32 as B32
import Control.Monad

import Data.Digest.TigerHash.Internal

hexTigerHash, b32TigerHash :: TigerHash -> String
hexTigerHash = B16.encode . LBS.unpack . runPut . put
b32TigerHash = B32.encode . LBS.unpack . runPut . put

instance Show TigerHash where
    showsPrec _ th = (++) (b32TigerHash th)

instance Binary TigerHash where
    put (TigerHash a b c) = putWord64host a >> putWord64host b >> putWord64host c
    get = do
        a <- getWord64host
        b <- getWord64host
        c <- getWord64host
        return (TigerHash a b c)

class TigerHashable a where
    -- | Each "hashable" object should implement this using TigerContext class
    --   from Data.Digest.TigerHash.Internal. 
    tigerHashUpdate :: (TigerContext (Ptr c)) => Ptr c -> a -> IO ()

    -- | Instant Tiger Hash calculated with stack allocated context.
    tigerHash :: a -> TigerHash
    tigerHash x = inlinePerformIO . withTigerContext $ \ctx -> do
        tigerHashUpdate ctx x
        finalizeContext ctx

    -- | Same as tgerHash, but with Tiger Tree hashing algorithm
    tigerTreeHash :: a -> TigerHash
    tigerTreeHash x = inlinePerformIO . withTigerTreeContext $ \ctx -> do
        tigerHashUpdate ctx x
        finalizeContext ctx

    -- | Calculate sequence of hashes where each next is calculated on-demand
    --   and only after previous one using one context for all calculations.
    --   Be sure to prepare sequence wich contains only required for hashing
    --   entries.
    tigerHashList :: [a] -> [TigerHash]
    tigerHashList [] = []
    tigerHashList (x0:xs) = unsafePerformIO $ do
        ctx <- newTigerContext

        let mcomb x mys = unsafeInterleaveIO $ do -- list structure is lazy
                y <- withForeignPtr ctx $ \ctx_ -> do
                    resetContext ctx_
                    tigerHashUpdate ctx_ x
                    finalizeContext ctx_
                liftM (y:) mys

        -- no need to resetContext after newContext
        y0 <- withForeignPtr ctx $ \ctx_ -> do
            tigerHashUpdate ctx_ x0
            finalizeContext ctx_

        liftM (y0:) $ foldr mcomb (return []) xs

    {-# NOINLINE tigerHashList #-}

    -- | Same as tigerHashList, but with Tiger Tree hashing algorithm
    tigerTreeHashList :: [a] -> [TigerHash]
    tigerTreeHashList [] = []
    tigerTreeHashList (x0:xs) = unsafePerformIO $ do
        ctx <- newTigerTreeContext

        let mcomb x mys = unsafeInterleaveIO $ do -- list structure is lazy
                y <- withForeignPtr ctx $ \ctx_ -> do
                    resetContext ctx_
                    tigerHashUpdate ctx_ x
                    finalizeContext ctx_
                liftM (y:) mys

        -- no need to resetContext after newContext
        y0 <- withForeignPtr ctx $ \ctx_ -> do
            tigerHashUpdate ctx_ x0
            finalizeContext ctx_

        liftM (y0:) $ foldr mcomb (return []) xs

    {-# NOINLINE tigerTreeHashList #-}


