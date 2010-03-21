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

import TigerHash
import TigerHash.ByteString

import Text.Printf

import Data.Word
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
-- import Data.List.Stream ()

import Control.Monad

import System.Posix.Process

-- iterations = 1000000000::Word32
iterations = 100000::Word32

explodeBytes :: (Integral n, Bits n) => n -> [Word8]
explodeBytes x = walk (bitSize x `div` 8) x where
    walk 0 _ = []
    walk p x = (fromIntegral x : walk (p-1) (x `shiftR` 8))

hash s = do
    let th = tigerHash (BC.pack s)
    putStr $ "Hash of \"" ++ s ++ "\":\n\t" ++ show th ++ "\n"
    
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
  -- let buffer = [fromIntegral i | i <- [0 .. 65535]] :: [Word8]
  let buffer = B.pack [fromIntegral i | i <- [0 .. 65535]]
  let th = tigerHash buffer 
  putStr $ "Hash of a 64K-byte string:\n\t" ++ show th ++ "\n"

  {-
  let buffers = [ B.pack (explodeBytes n) `B.append` B.drop 4 buffer | n <- [1 .. iterations]]
  foldr1 seq buffers `seq` return ()
  -}
  let buffers = replicate (fromIntegral iterations) buffer

  putStr "Calculating speed...\n"

  t1 <- getProcessTimes
    
  -- foldr1 (>>) . map (return . tigerHash) $ replicate iterations buffer
  -- putStr $ "Last: " ++ show (foldr1 seq $ map tigerHash buffers) ++ "\n"
  -- putStr $ "Last: " ++ show (foldr1 seq $ tigerHashList buffers) ++ "\n"
  putStr $ "Last: " ++ show (foldr1 seq (tigerHashList buffers)) ++ "\n"
  {-
  let iter 1 = tigerHash buffer
      iter n = (tigerHash buffer) `seq` iter (n-1)
  putStr $ "Last: " ++ show (iter iterations) ++ "\n"
  -}
  
  t2 <- getProcessTimes

  let rate = fromIntegral iterations * fromIntegral (B.length buffer) * 8 / toRational (userTime t2 - userTime t1)

  -- putStr $ "rate = " ++ show rate ++ " bit/s\n"
  printf "rate = %.3f bit/s\n" ((fromRational rate)::Double)


