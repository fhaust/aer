

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Word7 where

import Data.Word
import Data.Bits
import Data.Serialize

import Control.DeepSeq
import Control.Applicative

import Text.Read

-- | a 7 bit word, used in the dvs address representation
newtype Word7 = Word7 Word8
    deriving (Enum, Eq, Integral, Num, Ord, Real, NFData, Bits, Serialize, FiniteBits)

instance Bounded Word7 where
    minBound = 0
    maxBound = 127

instance Show Word7 where
    show (Word7 w8) = show w8

instance Read Word7 where
    readPrec = Word7 <$> readPrec
