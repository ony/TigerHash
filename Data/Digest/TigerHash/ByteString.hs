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

-- | This module provides nothing, but instances for 'TigerHashable'
module Data.Digest.TigerHash.ByteString () where


import Data.Word
import Control.Monad

import Foreign.Ptr
import Foreign.ForeignPtr

import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Lazy.Internal as L
import qualified Data.ByteString.Lazy as LBS


import Data.Digest.TigerHash
import qualified Data.Digest.TigerHash.Internal as T

instance TigerHashable S.ByteString where
    tigerHashUpdate ctx = updateBS . S.toForeignPtr where
        updateBS (_, _, 0) = return ()
        updateBS (p, s, l) = withForeignPtr p $ \p_ -> T.updateContext ctx (p_ `plusPtr` s) l
        {-# INLINE updateBS #-}
    {-# INLINE tigerHashUpdate #-}

instance TigerHashable L.ByteString where
    tigerHashUpdate ctx = L.foldrChunks ((>>) . tigerHashUpdate ctx) (return ())
    {-# INLINE tigerHashUpdate #-}

instance TigerHashable [Word8] where
    tigerHashUpdate ctx = tigerHashUpdate ctx . LBS.pack
    {-# INLINE tigerHashUpdate #-}

    tigerHash = tigerHash . LBS.pack
    {-# INLINE tigerHash #-}

    tigerHashList = tigerHashList . map LBS.pack
    {-# INLINE tigerHashList #-}

