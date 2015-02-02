
{-# LANGUAGE DeriveGeneric #-}

module Data.AER.Tmpdiff128StereoPair where

import Control.DeepSeq
import Control.Applicative

import Data.Word
import Data.Word7
import Data.Bits
import Data.Serialize

import GHC.Generics

import qualified Data.AER.Types as AER
import qualified Data.AER.AEDat as AER

-- | Stereo Setup has two cameras: left and right
data Camera = L | R deriving (Show,Read,Eq)

-- | Events have to polarities: up and down
data Polarity = U | D deriving (Show,Read,Eq)

-- | DVS address data as provided by the DVS cameras
data Address = Address {
    polarity :: Polarity,
    posX     :: Word7,
    posY     :: Word7,
    camera   :: Camera
} deriving (Show,Read,Eq,Generic)


-- | DVS addresses are encoded in one Word32
instance Serialize Address where
  get = decodeAddress <$> getWord32be
  put = putWord32be . encodeAddress

decodeAddress :: Word32 -> Address
decodeAddress w = Address p x y c
    where p = case testBit w 0 of False -> D
                                  True  -> U
          x = fromIntegral $ (w `shiftR` 1) .&. 0x7f
          y = fromIntegral $ (w `shiftR` 8) .&. 0x7f
          c = case testBit w 15 of False -> L
                                   True  -> R

encodeAddress :: Address -> Word32
encodeAddress (Address p x y c) = bp .|. bx .|. by .|. bc
    where bp = case p of D -> 0
                         U -> bit 0
          bx = (fromIntegral x .&. 0x7f) `shiftL` 1
          by = (fromIntegral y .&. 0x7f) `shiftL` 8
          bc = case c of L -> 0
                         R -> bit 15

-- | NFData is derived
instance NFData Address


readStereoData :: FilePath -> IO (Either String [AER.Event Address])
readStereoData = AER.readAERData

writeStereoData :: FilePath -> [AER.Event Address] -> IO ()
writeStereoData = AER.writeAERData
