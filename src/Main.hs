module Main where

import System.Random
import Graphics.Gloss.Interface.IO.Game
import Debug.Trace

import Renderer
import GameState
import InputHandler
import InputState
import WorldGen

data ControlState = ControlState {
  is :: InputState,
  gs :: GameState
}

main :: IO ()
--main = playIO (InWindow "haskell-game" (800,600) (30,30)) white 60 initialCtl renderCtl handleEvtIO stepIO
main = test

initialCtl = ControlState initialInputState initial

renderCtl :: ControlState -> IO Picture
renderCtl (ControlState _ gs) = return $ render gs

stepIO :: Float -> ControlState -> IO ControlState
stepIO f cs@(ControlState inp game) = return $ ControlState inp (step inp game)

handleEvtIO :: Event -> ControlState -> IO ControlState
handleEvtIO f s = return $ s { is = handleEvt f (is s)}
