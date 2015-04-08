
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

import           Data.Vector.Unboxed.Deriving

-- | Stereo Setup has two cameras: left and right
data Camera = L | R deriving (Show,Read,Eq)

{--- | Events have to polarities: up and down-}
{-data Polarity = U | D deriving (Show,Read,Eq)-}

-- | DVS address data as provided by the DVS cameras
data Address = Address {
    polarity :: AER.Polarity,
    posX     :: Word7,
    posY     :: Word7,
    camera   :: Camera
} deriving (Show,Read,Eq,Generic)

derivingUnbox "Address"
    [t| Address -> (AER.Polarity,Word7,Word7,Camera) |]
    [| \(Address p x y c) -> (p,x,y,c)       |]
    [| \(p,x,y,c)         -> Address p x y c |]

derivingUnbox "Camera"
    [t| Camera -> Bool |]
    [| \p -> if p == L then True else False |]
    [| \p -> if p then L else R             |]

-- | retype event for convenience
qview (AER.Event (Address p x y c) t) = (p,x,y,c,t)

-- | DVS addresses are encoded in one Word32
instance Serialize Address where
  get = decodeAddress <$> getWord32be
  put = putWord32be . encodeAddress

decodeAddress :: Word32 -> Address
decodeAddress w = Address p x y c
    where p = case testBit w 0 of False -> AER.D
                                  True  -> AER.U
          x = fromIntegral $ (w `shiftR` 1) .&. 0x7f
          y = fromIntegral $ (w `shiftR` 8) .&. 0x7f
          c = case testBit w 15 of False -> L
                                   True  -> R

encodeAddress :: Address -> Word32
encodeAddress (Address p x y c) = bp .|. bx .|. by .|. bc
    where bp = case p of AER.D -> 0
                         AER.U -> bit 0
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
