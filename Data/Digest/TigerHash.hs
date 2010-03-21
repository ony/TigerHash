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
module Data.Digest.TigerHash (TigerHash, TigerHashable, tigerHash, tigerHashList) where
import Data.Digest.TigerHash.Internal

-- import Data.Word
-- import Data.Char
import Text.Show
-- import Text.Read
import qualified Data.ByteString.Lazy as LBS
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import qualified Codec.Binary.Base32 as B32

{-
showNetHex64 :: Word64 -> ShowS
showNetHex64 = walk (8::Int) where
    walk 0 _ r = r
    walk p n r = c0 : c1 : walk (p-1) n' r where
        (n',m) = n `quotRem` 0x100
        (d0,d1) = m `quotRem` 0x10
        c0 = intToDigit $ fromIntegral d0
        c1 = intToDigit $ fromIntegral d1

readNetHex64 :: ReadS Word64
readNetHex64 = walk (8::Int) 0 where
    walk 0 v r = [(v,r)]
    walk _ _ [] = []
    walk _ _ [_] = []
    walk _ _ (c0:c1:_) | not (isHexDigit c0) || not (isHexDigit c1) = []
    walk p v (c0:c1:r) = walk p' v' r where
        d0 = fromIntegral $ digitToInt c0
        d1 = fromIntegral $ digitToInt c1
        v' = (d0 * 0x10 + d1) * 0x100^p' + v
        p' = p-1;


instance Show TigerHash where
    showsPrec _ (TigerHash a b c) = showNetHex64 a . showNetHex64 b . showNetHex64 c

instance Read TigerHash where
    readsPrec _ s = do
        (a,s') <- readNetHex64 s
        (b,s'') <- readNetHex64 s'
        (c,s''') <- readNetHex64 s''
        return (TigerHash a b c, s''')
-}

instance Show TigerHash where
    showsPrec _ th = (++) (B32.encode . LBS.unpack . runPut $ put th)

{-
instance Read TigerHash where
    readsPrec _ s = do
        B32.encode . LBS.unpack . runPut $ put th
-}

instance Binary TigerHash where
    put (TigerHash a b c) = putWord64host a >> putWord64host b >> putWord64host c
    get = do
        a <- getWord64host
        b <- getWord64host
        c <- getWord64host
        return (TigerHash a b c)

class TigerHashable a where
    tigerHash :: a -> TigerHash
    tigerHashList :: [a] -> [TigerHash]


