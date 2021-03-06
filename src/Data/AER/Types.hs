
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.AER.Types where

import           Control.DeepSeq
import           Control.Applicative

import           Data.Word
import           Data.Serialize
import           Data.Thyme.Clock
import           Data.Thyme.Time
import           Data.Storable.Endian

import qualified Data.Vector.Unboxed as V
import           Data.Vector.Unboxed.Deriving

import           GHC.Generics

import           Foreign.Storable

--------------------------------------------------
-- data types

data Polarity = U | D deriving (Show,Read,Eq,Ord,Generic)

data Event a = Event {
    address   :: a,
    timestamp :: NominalDiffTime
} deriving (Show,Read,Eq,Generic)


data Packet a = Packet {
    seqNum :: Word32,
    events :: [Event a]
} deriving (Show,Read,Eq,Generic)

--------------------------------------------------
-- Serialize instances

instance Serialize a => Serialize (Event a) where
    get = Event <$> get <*> (fromMicroseconds . fromIntegral <$> getWord32be)
    put (Event a t) = put a >> (putWord32be . fromIntegral . toMicroseconds $ t)

instance Serialize a => Serialize (Packet a) where
    get = Packet <$> get <*> many get
    put (Packet n es) = put n >> mapM_ put es


--------------------------------------------------
-- Foreign instances

instance Storable a => Storable (Event a) where
    sizeOf _ = 8 -- 4 bytes address + 4 bytes timestamp
    alignment _ = 4
    peekByteOff addr off = do
      a <- peekByteOff addr off
      (BE t) <- peekByteOff addr (off + 4) :: IO (BigEndian Word32)
      return $ Event a (fromMicroseconds . fromIntegral $ t)
    pokeByteOff addr off (Event a ts) = do
      pokeByteOff addr off a 
      pokeByteOff addr (off + 4) (BE . fromIntegral . toMicroseconds $ ts :: BigEndian Word32)


--------------------------------------------------
-- derive Unbox instances

derivingUnbox "Polarity"
    [t| Polarity -> Bool         |]
    [| (== U)                    |]
    [| \p -> if p then U else D  |]

derivingUnbox "Event"
    [t| forall a. V.Unbox a => Event a -> (a,NominalDiffTime)  |]
    [| \(Event a t) -> (a,t) |]
    [| (uncurry Event) |]

--------------------------------------------------
-- derive NFData instances

instance NFData a => NFData (Event a)
instance NFData a => NFData (Packet a)
instance NFData Polarity


