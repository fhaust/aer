

{-# LANGUAGE OverloadedStrings #-}

module Data.AER.AEDat where


import           Control.Applicative

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8


import qualified Data.AER.Types as AER

import           Data.Thyme.Clock
import           Data.Thyme.Format
import           System.Locale

import           Data.Serialize (Serialize, runPut, runGet, put, get)

-- | decode aerdat file contents into events
decode :: Serialize a => B.ByteString -> Either String [AER.Event a]
decode = runGet (many get)

-- | read aerdata file into events
readAERData :: Serialize a => FilePath -> IO (Either String [AER.Event a])
readAERData name = decode . dropHeader <$> B.readFile name

-- | drop the first lines that contain the AER header
dropHeader :: B.ByteString -> B.ByteString
dropHeader = B8.unlines . dropWhile isComment . B8.lines
    where isComment bs = B8.head bs == '#'


-- | serialize a series of events into a bytestring
encode :: Serialize a => [AER.Event a] -> B.ByteString
encode es = runPut $ mapM_ put es

-- | write aerdat file from events
writeAERData :: Serialize a => FilePath -> [AER.Event a] -> IO ()
writeAERData name es = do
    t <- getCurrentTime
    writeAERData' name t es

writeAERData' :: Serialize a => FilePath -> UTCTime -> [AER.Event a] -> IO ()
writeAERData' name t es = do
    B.writeFile name $ header t
    B.appendFile name $ encode es

-- | standard AER-DAT header
header :: UTCTime -> B.ByteString
header t = B.concat ["#!AER-DAT2.0\r\n"
                    ,"# This is a raw AE data file - do not edit\r\n"
                    ,"# Data format is int32 address, int32 timestamp (8 bytes total), repeated for each event\r\n"
                    ,"# Timestamps tick is 1 us\r\n"
                    ,"# created ", timeFormat t, "\r\n"
                    ]

timeFormat :: UTCTime -> B.ByteString
timeFormat = B8.pack . formatTime defaultTimeLocale "%a %b %d %H:%M:%S %Z %Y"
