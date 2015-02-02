


module Arbitraries where

import Control.Applicative

import Test.QuickCheck


import qualified Data.AER.Types as AER
import qualified Data.AER.DVS   as DVS

import           Data.Thyme.Time

import Data.Word7
import Data.Word

instance Arbitrary Word7 where
    arbitrary = arbitrarySizedBoundedIntegral

instance Arbitrary DVS.Address where
    arbitrary = DVS.Address <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (AER.Event a) where
    arbitrary = AER.Event <$> arbitrary <*> arbitraryTimestamp


-- this is necessary because aer only supports word32 timestamps
arbitraryTimestamp :: Gen NominalDiffTime
arbitraryTimestamp = fromMicroseconds . fromIntegral <$> (arbitrary :: Gen Word32)

instance Arbitrary a => Arbitrary (AER.Packet a) where
    arbitrary = AER.Packet <$> arbitrary <*> arbitrary
