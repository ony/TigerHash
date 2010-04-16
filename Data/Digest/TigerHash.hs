{-# LANGUAGE BangPatterns #-}
{- |
   Module      :  Data.Digest.TigerHash
   Copyright   :  (c) Orlyuk Nikolay 2010
   License     :  GPL-2
   
   Maintainer  :  virkony@gmail.com
   Stability   :  provisional
   
   There comes some kind of description how to use this module.
  
   Assume next import:

   > import Data.Digest.TigerHash
  
   Typical instant usage:
 
   > instance TigerHashable k => TigerHashable (k, Message) where
   >     tigerHashUpdate ctx_ (key, Message {sender = data0, body = data1}) = do
   >         tigerHashUpdate ctx_ key
   >         tigerHashUpdate ctx_ data0
   >         tigerHashUpdate ctx_ data1
   >
   > signMessage :: TigerHashable k => k -> Message -> SignedMessage
   > signMessage pkey msg = SignedMessage { message = msg, sign = tigerHash (pkey, msg) }
 
   This is pretty useful when you need to send signed messages over public channel.
  
   But using this in a such functional way have its drawbacks. Each time system
   requires calculation of @hash@ it will issue prepearing of new context for each
   calculation instead of using the same context.
  
   To solve that there is function for processing lazy list:
 
   > hashMessageSenders :: [Message] -> [(TigerHash, Message)]
   > hashMessageSenders msgs = zip (tigerHashList senders) msgs
   >     where senders = map sender msgs
 
   This can be used for building hashed storage, which requires to hash each element of the list.
   Notice that while you expand each node of the least 'tigerHashList' will calculate it's @head@
   for you. That's done with intention to loose maximum overhead while hashing files for DC++ .
 -}
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

-- | render 'TigerHash' to 'String' as hex-dump
hexTigerHash :: TigerHash -> String
hexTigerHash = B16.encode . LBS.unpack . runPut . put

-- | render 'TigerHash' to 'String' using Base32 encoding (as used in magnet-links and etc.)
b32TigerHash :: TigerHash -> String
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
    -- | Each 'TigerHashable' data should implement this using 'updateContext' of
    --   'TigerContext' class from "Data.Digest.TigerHash.Internal". But usually
    --   there is enough to just call 'tigerHashUpdate' for data which already
    --   have instance for 'TigerHashable'.
    tigerHashUpdate :: (TigerContext (Ptr c)) => Ptr c -> a -> IO ()

    -- | Instant caluculation of Tiger Hash with stack allocated context.
    tigerHash :: a -> TigerHash
    tigerHash x = inlinePerformIO . withTigerContext $ \ctx -> do
        tigerHashUpdate ctx x
        finalizeContext ctx

    -- | Same as 'tgerHash', but with Tiger Tree hashing algorithm
    tigerTreeHash :: a -> TigerHash
    tigerTreeHash x = inlinePerformIO . withTigerTreeContext $ \ctx -> do
        tigerHashUpdate ctx x
        finalizeContext ctx

    -- | Calculate sequence of hashes where each next is calculated on-demand
    --   and /only after previous one/ using one context for all calculations.
    --   Be sure to prepare sequence wich contains /only required for hashing/
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

    -- | Same as 'tigerHashList', but with Tiger Tree hashing algorithm
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
