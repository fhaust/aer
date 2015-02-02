

module Main where

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import Data.Serialize
import Data.Int
import Data.Word
import Data.Bits

{-import           Data.Array.Repa  (Z(..), (:.)(..))-}
{-import qualified Data.Array.Repa as R-}

import Control.Applicative
import Control.Monad

{-import           Data.ByteString (ByteString)-}
{-import qualified Data.ByteString as B-}

{-import qualified FRP.Yampa as Y-}

import GHC.Generics


data Address = Address {
    polarity :: Bool,
    posX     :: Word8,
    posY     :: Word8
} deriving (Show,Read,Eq)

data Event = Event {
    address :: Address,
    timestamp :: Word32 
} deriving (Show,Read,Eq)

data Packet = Packet {
    seqNum :: Int32,
    events :: [Event]
} deriving (Show,Read,Eq)

--------------------------------------------------

instance Serialize Address where
  get = do
    addr <- getWord32be
    let p = testBit addr 0
    let x = fromIntegral $ (addr `shiftR` 1) .&. 0x7f
    let y = fromIntegral $ (addr `shiftR` 8) .&. 0x7f
    return $ Address p x y
  put = undefined

instance Serialize Event where
  get = Event <$> get <*> get
  put = undefined

instance Serialize Packet where
  get = Packet <$> get <*> some get
  put = undefined


--------------------------------------------------




main = do

    es <- map read . lines <$> readFile "chessboard.aer" :: IO [Packet]


    print "stuff"


--------------------------------------------------

networkToStdoutMain = withSocketsDo $ do
    putStr "started ... "

    -- open socket
    addrInfo <- getAddrInfo
                  (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                  Nothing (Just "8991")
    let serverAddr = head addrInfo

    sock <- socket (addrFamily serverAddr) Datagram defaultProtocol

    bindSocket sock (addrAddress serverAddr)

    putStrLn "started" 
    _ <- procMessages sock

    print "done"




procMessages sock = do
    (msg, addr) <- recvFrom sock 1024
    let prsd = decode msg :: Either String Packet

    case prsd of
      Right p  -> print p
      Left err -> error err

    procMessages sock


--------------------------------------------------
