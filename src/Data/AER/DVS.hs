
{-# LANGUAGE DeriveGeneric #-}

module Data.AER.DVS where

import Control.DeepSeq
import Control.Applicative

import Data.Word
import Data.Word7
import Data.Bits
import Data.Serialize

import GHC.Generics

import qualified Data.AER.Types as AER
import qualified Data.AER.AEDat as AER

-- | DVS address data as provided by the DVS cameras
data Address = Address {
    polarity :: Bool,
    posX     :: Word7,
    posY     :: Word7
} deriving (Show,Read,Eq,Generic)

-- | retype event for convenience
type Event = AER.Event Address

qview (AER.Event (Address p x y) t) = (p,x,y,t)

-- | DVS addresses are encoded in one Word32
instance Serialize Address where
  get = decodeAddress <$> getWord32be
  put = putWord32be . encodeAddress

decodeAddress :: Word32 -> Address
decodeAddress w = Address p x y
    where p = testBit w 0
          x = fromIntegral $ (w `shiftR` 1) .&. 0x7f
          y = fromIntegral $ (w `shiftR` 8) .&. 0x7f

encodeAddress :: Address -> Word32
encodeAddress (Address p x y) = bp .|. bx .|. by
    where bp = if p then 1 else 0
          bx = (fromIntegral x .&. 0x7f) `shiftL` 1
          by = (fromIntegral y .&. 0x7f) `shiftL` 8

-- | NFData is derived
instance NFData Address


readDVSData :: FilePath -> IO (Either String [AER.Event Address])
readDVSData = AER.readAERData

writeDVSData :: FilePath -> [AER.Event Address] -> IO ()
writeDVSData = AER.writeAERData
