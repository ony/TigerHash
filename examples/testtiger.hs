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

{-# OPTIONS_GHC -O2 -fignore-asserts -fspec-constr #-}

import Data.Digest.TigerHash
import Data.Digest.TigerHash.ByteString
import Data.Digest.TigerHash.Internal

import Text.Printf

import Data.Word
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Char8 as BC
import Foreign.Ptr
import Foreign.ForeignPtr

import Control.Monad

import System.Posix.Process

iterations = 200000::Word32

explodeBytes :: (Integral n, Bits n) => n -> [Word8]
explodeBytes x = walk (bitSize x `div` 8) x where
    walk 0 _ = []
    walk p x = fromIntegral x : walk (p-1) (x `shiftR` 8)

hash s = do
    let th = tigerHash (BC.pack s)
    putStr $ "Hash of \"" ++ s ++ "\":\n\t" ++ hexTigerHash th ++ "\n"
    
main = do
  -- Hash of short strings
  hash ""
  hash "abc"
  hash "Tiger"
  -- Hash of 512-bit strings
  hash "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+-"
  hash "ABCDEFGHIJKLMNOPQRSTUVWXYZ=abcdefghijklmnopqrstuvwxyz+0123456789"
  hash "Tiger - A Fast New Hash Function, by Ross Anderson and Eli Biham"
  -- Hash of two-block strings strings
  hash "Tiger - A Fast New Hash Function, by Ross Anderson and Eli Biham, proceedings of Fast Software Encryption 3, Cambridge."
  hash "Tiger - A Fast New Hash Function, by Ross Anderson and Eli Biham, proceedings of Fast Software Encryption 3, Cambridge, 1996."
  hash "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+-ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+-"

  -- Hash of a 64K byte string
  let buffer = B.pack [fromIntegral i | i <- [0 .. 65535]]
  buffer `seq` return () -- force calc

  let th = tigerHash buffer 
  putStr $ "Hash of a 64K-byte string:\n\t" ++ hexTigerHash th ++ "\n"

  putStr "Calculating speed...\n"

  t1 <- getProcessTimes
    
  let (p, s, bufSz) = S.toForeignPtr buffer in withForeignPtr p $ \p_ -> do
          let bufPtr = (p_ `plusPtr` s)
          withTigerContext $ \ctx_ -> do
              let body = do
                      resetContext ctx_
                      updateContext ctx_ bufPtr bufSz
                      finalizeContext ctx_
              mapM_ (const body) [1 .. fromIntegral iterations]
  
  t2 <- getProcessTimes

  let rate = fromIntegral iterations * fromIntegral (B.length buffer) * 8 / toRational (userTime t2 - userTime t1)
  let humanRate = walk ["bit","KBit", "MBit", "GBit", "TBit"] where
        walk [x] r = (r, x)
        walk (x:xs) r | r < 512 = (r, x)
                      | otherwise = walk xs (r / 1024)

  let (hRate,hSym) = humanRate rate
  printf "rate = %.3f %s/s\n" (fromRational hRate :: Double) hSym


