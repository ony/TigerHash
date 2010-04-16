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

import qualified Data.ByteString.Lazy as B

import Control.Monad
import Control.Arrow

import System
import System.IO
import System.IO.Unsafe
import System.Directory
import System.FilePath
import Data.List.Stream ()

import Data.Digest.TigerHash
import Data.Digest.TigerHash.ByteString ()

showsHash :: String -> TigerHash -> ShowS
showsHash desc th = shows th . showChar ' ' . showString desc . showChar '\n'

traverse path = unsafeInterleaveIO $ do
    nodes <- liftM (map (path </>) . filter (`notElem` [".",".."])) $ getDirectoryContents path
    files <- filterM (unsafeInterleaveIO . doesFileExist) nodes
    subdirs <- filterM (unsafeInterleaveIO . doesDirectoryExist) nodes
    liftM ((files ++) . concat) . unsafeInterleaveIO $ mapM traverse subdirs

hash fn = B.readFile fn >>= putStr . flip (showsHash fn) [] . tigerTreeHash

main_args :: [String] -> IO ()
main_args [] = main_args ["-"]
main_args ["-"] = B.getContents >>= putStr . flip (showsHash "-") [] . tigerTreeHash
main_args args = do
    --process args
    filenamesA <- filterM (unsafeInterleaveIO . doesFileExist) args
    let dirnames = filter (`notElem` filenamesA) args
    filenamesB <- liftM concat $ mapM traverse dirnames
    let filenames = filenamesA ++ filenamesB

    let printHash fn hash = putStr $ showsHash fn hash []
    let readFn fn = return . (,) fn =<< unsafeInterleaveIO (B.readFile fn)
    contents <- mapM (readFn) filenames
    let hashes = uncurry zip (second tigerTreeHashList $ unzip contents)
    mapM_ (uncurry printHash) hashes
    

main = getArgs >>= main_args . nub
