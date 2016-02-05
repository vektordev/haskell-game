module NetworkConnection (
  ConnectionState (..),
  mkServer,
  mkClient,
  mkUnconnected
) where
import GameState
import InputState

import Network.Socket
import Control.Monad(unless)
import Text.Read
import Data.Maybe
import qualified Data.Map.Strict as Map

import Control.Concurrent
import Control.Concurrent.STM

data ConnectionState =
  Standalone |
  ClientConnection (TVar GameState) ThreadId (TMVar InputState) ThreadId |
  ServerConnection (TMVar GameState) ThreadId (TVar (Map.Map SockAddr InputState)) ThreadId (TVar [SockAddr])

--TODO: Due to constraints with gloss' main loop, deallocation is currently quite impossible.
--see http://blog.coldflake.com/posts/Simple-Networking/ for a simple example

conditionalPrint :: String -> IO ()
conditionalPrint str = return ()

mkUnconnected :: ConnectionState
mkUnconnected = Standalone

mkClient :: InputState -> GameState -> IO ConnectionState
mkClient inp gamestate = do
  inputTMVar <- atomically $ newTMVar inp
  stateTVar <- atomically $ newTVar gamestate
  recvThread <- forkIO $ clientRecvThread stateTVar
  sendThread <- forkIO $ clientSendThread inputTMVar
  return (ClientConnection stateTVar sendThread inputTMVar recvThread)

mkServer :: GameState -> IO ConnectionState
mkServer gamestate = do
  inputTVar <- atomically $ newTVar Map.empty
  stateTMVar <- atomically $ newTMVar gamestate
  clientListTVar <- atomically $ newTVar []
  sendThread <- forkIO $ serverSendThread stateTMVar clientListTVar
  recvThread <- forkIO $ serverRecvThread inputTVar clientListTVar
  return (ServerConnection stateTMVar sendThread inputTVar recvThread clientListTVar)

clientSendThread :: TMVar InputState -> IO ()
clientSendThread is = do
  conditionalPrint "starting client thread B"
  sock <- mkClientSendSocket
  loop sock
    where
      loop :: Socket -> IO ()
      loop socket = do
        --conditionalPrint "sending message to server"
        inp <- atomically $ takeTMVar is
        send socket (show inp)
        loop socket

clientRecvThread :: TVar GameState -> IO ()
clientRecvThread gs = do
  conditionalPrint "starting client thread A"
  sock <- mkClientRecvSocket
  loop sock
    where
      loop socket = do
        conditionalPrint "waiting for message on thread A"
        sockLoc <- getSocketName socket
        --sockRem <- getPeerName socket
        conditionalPrint ("listening on: " ++ show sockLoc)
        --conditionalPrint ("bound to: " ++ show sockRem)
        (msg,_,_) <- recvFrom socket 1024
        conditionalPrint ("received message " ++ msg)
        let mGS' = readMaybe msg
        unless (isNothing mGS') $ do
          --ignore the previous value of gs
          atomically $ swapTVar gs (fromJust mGS')
          return ()
        loop socket

serverSendThread :: TMVar GameState -> TVar [SockAddr] -> IO ()
serverSendThread gamestate clients = do
  sock <- mkServerSendSocket
  loop sock
    where
      loop :: Socket -> IO ()
      loop socket = do
        gs <- atomically $ takeTMVar gamestate
        cl <- atomically $ readTVar clients
        dispense socket gs cl
        loop socket
      dispense :: Socket -> GameState -> [SockAddr] -> IO()
      dispense socket gamestate clients = unless (null clients) $ do
        lngth <- sendTo socket (show gamestate) (changePort (head clients) 3444)
        conditionalPrint ("dispensing a " ++ show lngth ++ " subset of a " ++ (show $ length $ show gamestate) ++ " long message, changing port to 3444")
        sockLoc <- getSocketName socket
        conditionalPrint ("from: " ++ show sockLoc)
        conditionalPrint ("to: " ++ (show $ head clients))
        dispense socket gamestate (tail clients)

changePort (SockAddrInet _ addr) port = SockAddrInet port addr

serverRecvThread :: TVar (Map.Map SockAddr InputState) -> TVar [SockAddr] -> IO ()
serverRecvThread addToIS clients = do
  sock <- mkServerRecvSocket
  loop sock
    where
      loop socket = do
        (msg,lng,addr) <- recvFrom socket 1024
        conditionalPrint "received message from client"
        --sendTo socket "Hello world" addr
        let mIS' = readMaybe msg
        unless (isNothing mIS') $ atomically $ modifyTVar addToIS (Map.insert addr (fromJust mIS'))
        atomically $ modifyTVar clients (\cl -> addr : filter (/= addr) cl)
        loop socket

uploadPortC :: String
uploadPortC = "3000"
downloadPortC :: String
downloadPortC = "3001"

uploadPortS :: String
uploadPortS = "3002"
downloadPortS :: String
downloadPortS = "3003"

mkClientRecvSocket :: IO Socket
mkClientRecvSocket = do
  (serveraddr:_) <- getAddrInfo Nothing (Just "127.0.0.1") (Just "3444")
  s <- socket (addrFamily serveraddr) Datagram defaultProtocol
  bind s (addrAddress serveraddr) >> return s

mkServerSendSocket :: IO Socket
mkServerSendSocket = do
  (serveraddr:_) <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just "3446")
  s <- socket (addrFamily serveraddr) Datagram defaultProtocol
  bind s (addrAddress serveraddr) >> return s

mkClientSendSocket :: IO Socket
mkClientSendSocket = do
  (serveraddr:_) <- getAddrInfo Nothing (Just "127.0.0.1") (Just "3445")
  s <- socket (addrFamily serveraddr) Datagram defaultProtocol
  connect s (addrAddress serveraddr) >> return s

mkServerRecvSocket :: IO Socket
mkServerRecvSocket = do
  (serveraddr:_) <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just "3445")
  s <- socket (addrFamily serveraddr) Datagram defaultProtocol
  bind s (addrAddress serveraddr) >> return s
