


module Network.AER.UDP where


import           Control.Monad.Loops

import           Network.Socket hiding (send, sendTo, recv, recvFrom)
import           Network.Socket.ByteString

import           Data.Serialize
import           Data.IORef
import           Data.Word

import           Data.AER.Types


withAEROverUDP :: Serialize a => ServiceName -> ([Event a] -> IO b) -> IO b
withAEROverUDP port f = withSocketsDo $ do

    --open socket
    addrInfo <- getAddrInfo
                  (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                  Nothing
                  (Just port)
    let serverAddr = head addrInfo

    sock <- socket (addrFamily serverAddr) Datagram defaultProtocol

    bindSocket sock (addrAddress serverAddr)

    es <- unfoldM $ recvEvents sock

    f (concat es)

recvEvents :: Serialize a => Socket -> IO (Maybe [Event a])
recvEvents sock = do
    (msg, _) <- recvFrom sock 4096

    case decode msg of
      Right p  -> return (Just (events p))
      Left err -> error err


sendAEROverUDP :: Serialize a => HostName -> ServiceName -> [[Event a]] -> IO ()
sendAEROverUDP hostname port es = withSocketsDo $ do

    seqRef <- newIORef 0

    addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
    let serverAddr = head addrinfos
    sock <- socket (addrFamily serverAddr) Datagram defaultProtocol

    mapM_ (sendEvents sock seqRef) es

sendEvents :: Serialize a => Socket -> IORef Word32 -> [Event a] -> IO ()
sendEvents sock seqRef es = do
    s' <- atomicModifyIORef' seqRef (\s -> (s+1,s))

    let buf = encode $ Packet s' es

    sendAll sock buf

