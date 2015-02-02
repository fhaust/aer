

module Main where

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)

import Test.QuickCheck()
import Test.HUnit (Assertion, assertBool)

import Control.Applicative

import Data.Serialize

import qualified Data.AER.Types  as AER
import qualified Data.AER.DVS128 as DVS
import qualified Network.AER.UDP as UDP

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

import Arbitraries()



main :: IO ()
main = defaultMain tests


tests :: [Test]
tests = [ testGroup "DVS tests"
          [ testProperty "dvs addr codec" propDvsAddrCodec
          , testProperty "dvs codec" propDvsCodec
          , testProperty "aer packet codec" propAerPacketCodec
          , testCase     "dvs aerdat io" testDvsDatIO
          {-, testCase     "aer over udp" testAEROverUDP-}
          ]
        ]


propDvsAddrCodec :: DVS.Address -> Bool
propDvsAddrCodec e = Right e == (decode . encode $ e)

propDvsCodec :: AER.Event DVS.Address -> Bool
propDvsCodec e = Right e == (decode . encode $ e)


testDvsDatIO :: Assertion
testDvsDatIO = do

    let sampleFile = "data/DVS128-2014-09-16T15-49-51+0200-20000-0.aedat"
        testFile   = "data/test-output.aedat"

    (Right es) <- DVS.readDVSData sampleFile

    -- rewrite the data
    DVS.writeDVSData testFile es
    {-let t = readTime defaultTimeLocale "%a %b %d %H:%M:%S %Z %Y" "Tue Sep 16 15:49:51 CEST 2014"-}
    {-AER.writeAERData' "test-output.aedat" t es-}

    -- compare the files
    originalData <- dropLinesWhile ((== '#') . B8.head) <$> B.readFile sampleFile
    testData     <- dropLinesWhile ((== '#') . B8.head) <$> B.readFile testFile

    assertBool "aedat filewriting and reading does not work" (originalData == testData)

testAEROverUDP :: Assertion
testAEROverUDP = do

    let sampleFile = "data/DVS128-2014-09-16T15-49-51+0200-20000-0.aedat"

    (Right es) <- DVS.readDVSData sampleFile

    -- open server socket
    recvEs <- UDP.withAEROverUDP "9999" $ return

    -- send events
    UDP.sendAEROverUDP "localhost" "9999" [es]

    assertBool "aer over udp does not work" (es == recvEs)

dropLinesWhile :: (B.ByteString -> Bool) -> B.ByteString -> B.ByteString
dropLinesWhile f = B8.unlines . dropWhile f . B8.lines

propAerPacketCodec :: AER.Packet DVS.Address -> Bool
propAerPacketCodec p = Right p == (decode . encode $ p)
