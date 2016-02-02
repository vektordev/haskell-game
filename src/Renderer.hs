module Renderer (
  render
, RendererState (..)
, initialRenderer
, updateRenderer
, renderWorld
) where

import Data.ByteString as BS
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import GameState
import World

data RendererState = RendererState {
  worldBMP :: Maybe Picture
} deriving (Show)

initialRenderer = RendererState Nothing

render :: RendererState -> GameState -> Picture
render (RendererState mPicture) (GameState ets wrld ) = Pictures (maybe id (:) mPicture (Prelude.map renderEntity ets))

renderEntity :: Entity -> Picture
renderEntity (Entity x y _) = Translate (fromIntegral x) (fromIntegral y) $ Pictures [Circle 10, Color red $ ThickCircle 5 10]

renderWorld :: World -> Picture
renderWorld wrld@(World sigs xres yres) = bitmapOfByteString xres yres (datas wrld) True

updateRenderer :: GameState -> RendererState -> RendererState
updateRenderer gs (RendererState Nothing) = RendererState $ Just  $ renderWorld $ world gs
updateRenderer gs rs = rs

datas sigs = pack $ Prelude.concatMap colorize $ normalize $ sampleArea sigs

sampleArea (World signals xres yres) = [sampleAll signals (Pos x y) | x <- Prelude.take xres [0, (1 / fromIntegral xres) ..], y <- Prelude.take yres [0, (1 / fromIntegral yres) ..]]

sampleAll signals pos = sum $ Prelude.map (\s -> sample s pos) signals
-- = [sample signal (Pos x y) | x <- [0, 0.01 .. 1], y <- [0, 0.01 .. 1]]

--TODO: use statistical normalization instead, to balance out hills and valleys.
normalize :: [Float] -> [Float]
normalize fls = Prelude.map (\f -> (f - low) / delta) fls
  where
    high = Prelude.maximum fls
    low = Prelude.minimum fls
    delta = high - low

postNormalize :: [Float] -> [Float]
postNormalize fls = Prelude.map (\x -> ((x-0.5)^3) * 4 + 0.5) fls

toGrey val = [255, floor (val * 255), floor (val * 255), floor (val * 255)]
colorize val = [255, floor r, floor g, floor b]
  where (b,g,r) = colorizeWithColorSet val colorSet

colorizeWithColorSet val ((a, col):(b, col2):xs) = if val < a then error "too small" else if val < b then interpolate val (a,col) (b,col2) else colorizeWithColorSet val ((b, col2):xs)
colorizeWithColorSet val [(threshold, color)] = color

interpolate val (left, (a,b,c)) (right, (d,e,f)) =
  if val < left || val > right
  then error "value out of bounds in color interpolation"
  else (a + (d-a)*factor,b + (e-b)*factor,c + (f-c)*factor)
    where
      factor = (val-left) / (right-left)

--TODO: Expand to accomodate values outside the range.
seaLevel = 0.35
colorSet = [(0, (0,0,120)), (seaLevel/2, (0, 0, 255)), (seaLevel - 0.01, (120,120,255)), (seaLevel, (128,255,128)), (0.7, (0,255,0)), (0.9, (222, 184,135)), (0.95, (165,42,42)), (1, (255,255,255))]

distance :: Pos -> Pos -> Float
distance (Pos x y) (Pos x2 y2) = sqrt (((x-x2)^2) + ((y-y2)^2))

sample :: Signal -> Pos -> Float
sample (Signal amp wavelen initialPhase p1) p2 = amp * (0.5 + 0.5 * sin (dist/wavelen + initialPhase))
  where
    dist = distance p1 p2
