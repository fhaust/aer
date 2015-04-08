

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Word7 where

import Data.Word
import Data.Bits
import Data.Serialize

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Deriving as UV


import Control.DeepSeq
import Control.Applicative

import Text.Read

-- | a 7 bit word, used in the dvs address representation
newtype Word7 = Word7 { unWord7 :: Word8 }
    deriving (Enum, Eq, Integral, Num, Ord, Real, NFData, Bits, Serialize, FiniteBits)


UV.derivingUnbox "Word7"
    [t| Word7 -> Word8   |]
    [| \(Word7 w8) -> w8 |]
    [| Word7             |]



instance Bounded Word7 where
    minBound = 0
    maxBound = 127

instance Show Word7 where
    show (Word7 w8) = show w8

instance Read Word7 where
    readPrec = Word7 <$> readPrec
