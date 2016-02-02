module Renderer (
  render
) where

import Graphics.Gloss.Data.Picture
import GameState

render :: GameState -> Picture
render (GameState ets) = Pictures (map renderEntity ets)

renderEntity :: Entity -> Picture
renderEntity (Entity x y _) = Translate (fromIntegral x) (fromIntegral y) $ Circle 10
