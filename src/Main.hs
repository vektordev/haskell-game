module Main where

import System.Random
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Pure.Display

import System.Environment(getArgs)

import Data.Maybe

import Renderer
import GameState
import InputHandler
import InputState
import NetworkConnection
import Network

import Control.Concurrent
import Control.Concurrent.STM

data ControlState = ControlState {
  is :: InputState,
  gs :: GameState,
  rs :: RendererState,
  cs :: ConnectionState
}

data SessionType = Client String | Server | Standalone

data Settings = Settings {
  session :: SessionType
}

main :: IO ()
main = do
  args <- getArgs
  processArgs args

processArgs :: [String] -> IO ()
processArgs [] = runGame loadInitial
processArgs ["debug_server"] = Network.server
processArgs ["debug_client"] = Network.client
processArgs ["server"] = loadGame (Settings Server) >>= runGame
processArgs ["client"] = loadGame (Settings (Client "127.0.0.1")) >>= runGame

loadGame :: Settings -> IO ControlState
loadGame (Settings(Client "127.0.0.1")) = do
  conn <- mkClient (is loadInitial) (gs loadInitial)
  return $ loadInitial {cs = conn}
loadGame (Settings Server) = do
    conn <- mkServer (gs loadInitial)
    return $ loadInitial {cs = conn}

runGame :: ControlState -> IO()
runGame state = playIO (InWindow "haskell-game" (800,600) (30,30)) white 60 state renderCtl handleEvtIO stepIO

loadInitial :: ControlState
loadInitial = initialCtl {rs = updateRenderer (gs initialCtl) (rs initialCtl)}

initialCtl = ControlState initialInputState initialGameState initialRenderer mkUnconnected

renderCtl :: ControlState -> IO Picture
renderCtl cs = do
  cs'@(ControlState _ gs renderer _) <- clientPullLatestGamestate cs
  return $ render renderer gs

stepIO :: Float -> ControlState -> IO ControlState
stepIO f cs = do
  cs'@(ControlState is gsOld _ _) <- preStep cs
  let stepped = cs {gs = step is gsOld}
  stepped' <- postStep stepped
  return stepped {rs = updateRenderer (gs stepped') (rs stepped')}

handleEvtIO :: Event -> ControlState -> IO ControlState
handleEvtIO f s = clientPullLatestGamestate $ s { is = handleEvt f (is s)}

--clients: push input state to network thread / done
--server: pull input states from network thread / done
preStep :: ControlState -> IO ControlState
preStep cs@(ControlState input _ _ (ClientConnection _ _ inputVar _)) = do
  atomically $ putTMVar inputVar input
  return cs
--TODO: handle multiplicity of inputs. Or else forget about it.
preStep cs@(ControlState _ _ _ (ServerConnection _ _ inputVar _ _)) = do
  inputs <- atomically $ readTVar inputVar
  return cs
preStep cs = return cs

--TODO: Every occasion we get, copy the latest game state into client-side ControlState
-- will be done just before rendering
-- essentially, stepIO won't actually have any effect on client right now.
clientPullLatestGamestate :: ControlState -> IO ControlState
clientPullLatestGamestate cs@(ControlState _ _ _ (ClientConnection gsVar _ _ _)) = do
  gs' <- atomically $ readTVar gsVar
  return cs{gs = gs'}
clientPullLatestGamestate cs = return cs

--server: push game state to network thread
postStep :: ControlState -> IO ControlState
postStep cs@(ControlState _ gs _ (ServerConnection gsVar _ _ _ _)) = do
  atomically $ putTMVar gsVar gs
  return cs
postStep cs = return cs
