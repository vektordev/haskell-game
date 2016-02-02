module Main where

import System.Random
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Pure.Display
import Debug.Trace
import Data.Maybe

import Renderer
import GameState
import InputHandler
import InputState

data ControlState = ControlState {
  is :: InputState,
  gs :: GameState,
  rs :: RendererState
} deriving (Show)

main :: IO ()
main =
  playIO (InWindow "haskell-game" (800,600) (30,30)) white 60 loadedCtl renderCtl handleEvtIO stepIO
    where loadedCtl = loadInitial

loadInitial = initialCtl {rs = updateRenderer (gs initialCtl) (rs initialCtl)}

initialCtl = ControlState initialInputState initialGameState initialRenderer

renderCtl :: ControlState -> IO Picture
renderCtl (ControlState _ gs renderer) = return $ render renderer gs

stepIO :: Float -> ControlState -> IO ControlState
stepIO f cs@(ControlState inp game _) = do
  let stepped = cs {gs = step inp game}
  return stepped {rs = updateRenderer (gs stepped) (rs stepped)}

handleEvtIO :: Event -> ControlState -> IO ControlState
handleEvtIO f s = return $ s { is = handleEvt f (is s)}
