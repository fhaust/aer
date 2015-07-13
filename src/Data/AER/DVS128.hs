
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.AER.DVS128 
  ( module Data.AER.Types
  , Address(..)
  , qview
  , readDVSData
  , mmapDVSData
  , writeDVSData
  ) where

import           Control.DeepSeq

import           Data.Word
import           Data.Word7
import           Data.Bits
import           Data.Serialize

import           GHC.Generics

import           Data.AER.Types
import qualified Data.AER.AEDat as AER

import           Data.Vector.Unboxed.Deriving (derivingUnbox)
import qualified Data.Vector.Storable as S

import           Data.Thyme.Clock

import           Foreign.Storable
import           Data.Storable.Endian

-- | DVS address data as provided by the DVS cameras
data Address = Address {
    polarity :: Polarity,
    posX     :: Word7,
    posY     :: Word7
} deriving (Show,Read,Eq,Generic)

derivingUnbox "Address"
    [t| Address -> (Polarity,Word7,Word7) |]
    [| \(Address p x y) -> (p,x,y)        |]
    [| \(p,x,y)         -> Address p x y  |]

instance Storable Address where
    sizeOf    _ = 4
    alignment _ = 4
    peek addr   = do
      (BE w) <- peekByteOff addr 0 :: IO (BigEndian Word32)
      return $ decodeAddress w
    poke addr a = pokeByteOff addr 0 (BE $ encodeAddress a)

-- | retype event for convenience
qview :: Event Address -> (Polarity, Word7, Word7, NominalDiffTime)
qview (Event (Address p x y) t) = (p,x,y,t)

-- | DVS addresses are encoded in one Word32
instance Serialize Address where
  get = decodeAddress <$> getWord32be
  put = putWord32be . encodeAddress

decodeAddress :: Word32 -> Address
decodeAddress w = Address p x y
    where p = if testBit w 0 then U else D
          x = fromIntegral $ (w `shiftR` 1) .&. 0x7f
          y = fromIntegral $ (w `shiftR` 8) .&. 0x7f

encodeAddress :: Address -> Word32
encodeAddress (Address p x y) = bp .|. bx .|. by
    where bp = if p == U then 1 else 0
          bx = (fromIntegral x .&. 0x7f) `shiftL` 1
          by = (fromIntegral y .&. 0x7f) `shiftL` 8

-- | NFData is derived
instance NFData Address


readDVSData :: FilePath -> IO (Either String [Event Address])
readDVSData = AER.readAERData

mmapDVSData :: FilePath -> IO (S.Vector (Event Address))
mmapDVSData = AER.mmapAERData

writeDVSData :: FilePath -> [Event Address] -> IO ()
writeDVSData = AER.writeAERData
