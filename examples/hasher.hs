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

import Text.Show
import Data.List
import Data.Word
import Data.Bits

import qualified Codec.Binary.Base32 as B32

import qualified Data.ByteString.Lazy as B

import Control.Monad
import Control.Arrow

import System
import System.IO
import System.IO.Unsafe
import System.Directory
import System.FilePath
import Data.List.Stream ()

import TigerHash
import TigerHash.ByteString
import TigerHash.Internal (TigerHash(..))

type ListS a = [a] -> [a]

concatListS :: [ListS a] -> ListS a
concatListS = flip (foldr (\f t -> f t))

explode64n :: Word64 -> [Word8]
explode64n v = [fromIntegral (v `shiftR` i) | i <- [64-8,64-16 .. 0]]
-- explode64n v = [fromIntegral (v `shiftR` i) | i <- [0,8 .. 64-8]]

b32show :: Word64 -> ShowS
b32show = showString . B32.encode . explode64n

base32s :: TigerHash -> ShowS
base32s (TigerHash a b c) = showString (B32.encode $ explode64n a ++ explode64n b ++ explode64n c)

showsHash :: String -> TigerHash -> ShowS
showsHash desc th = base32s th . showChar ' ' . showString desc . showChar '\n'

{-
traverseNode path = unsafeInterleaveIO $ do
        haveFile <- doesFileExist path
        if haveFile then return (path:) else do
            haveDir <- doesDirectoryExist path
            if haveDir then traverse path else
                return (id)

traverse "." = getDirectoryContents "." >>= liftM concatListS . mapM traverseNode . filter (`notElem` [".", ".."])
traverse path = getDirectoryContents path >>= liftM concatListS . mapM traverseNode . map (path `combine`) . filter (`notElem` [".", ".."])
-}

traverse path = unsafeInterleaveIO $ do
    nodes <- liftM (map (path </>) . filter (`notElem` [".",".."])) $ getDirectoryContents path
    files <- filterM (unsafeInterleaveIO . doesFileExist) nodes
    subdirs <- filterM (unsafeInterleaveIO . doesDirectoryExist) nodes
    liftM ((files ++) . concat) . unsafeInterleaveIO $ mapM traverse subdirs
{-
traverseX path = liftM snd dirs where
    prefix f g (a,b) = (f a, f b)
    prefixF f (a,b) = (f a, b)
    dirs = liftM (prefixF (path:)) (dirs >>= walk . const [] . fst)
    walk [] = return ([], [])
    walk (x:xs) = unsafeInterleaveIO $ do
        nodes <- liftM (map (x </>) . filter (`notElem` [".",".."])) $ getDirectoryContents x
        subdirs <- filterM doesDirectoryExist nodes
        files <- filterM doesFileExist nodes
        liftM (prefix (subdirs ++) (files ++)) $ walk []
-}
        



{-
traverse, traverseNode :: FilePath -> IO [FilePath]
traverseNode path = unsafeInterleaveIO $ do
        haveFile <- doesFileExist path
        if haveFile then return [path] else do
            haveDir <- doesDirectoryExist path
            if haveDir then traverse path else
                return []

traverse "." = getDirectoryContents "." >>= liftM concat . mapM traverseNode . filter (`notElem` [".", ".."])
traverse path = getDirectoryContents path >>= liftM concat . mapM traverseNode . map (path `combine`) . filter (`notElem` [".", ".."])
-}

hash fn = B.readFile fn >>= putStr . flip (showsHash fn) [] . tigerHash

main_args :: [String] -> IO ()
main_args [] = B.getContents >>= putStr . flip (showsHash "-") [] . tigerHash
main_args ["-"] = B.getContents >>= putStr . flip (showsHash "-") [] . tigerHash
-- main_args args = mapM traverseNode args >>= foldr (>>) (return ()) . map hash . concat
main_args args = do
    filenamesA <- filterM (unsafeInterleaveIO . doesFileExist) args
    let dirnames = filter (`notElem` filenamesA) args
    filenamesB <- liftM concat $ mapM traverse dirnames
    let filenames = filenamesA ++ filenamesB
    -- let filenames = filenamesL []
    --mapM_ (putStrLn) filenames
    let printHash fn hash = putStr $ showsHash fn hash []
    {-
    hashes <- liftM tigerHashList $ mapM (unsafeInterleaveIO . B.readFile) filenames
    sequence_ $ zipWith printHash filenames hashes
    -}
    let readFn fn = return . (,) fn =<< unsafeInterleaveIO (B.readFile fn)
    contents <- mapM (readFn) filenames
    let hashes = uncurry zip (second tigerHashList $ unzip contents)
    --mapM_ print contents
    mapM_ (uncurry printHash) hashes
    

main = getArgs >>= main_args . nub
