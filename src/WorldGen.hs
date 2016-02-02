module WorldGen (
  test
) where

import Data.ByteString
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.Pure.Display
import Debug.Trace
import System.Random

test = do
  sigs <- numSignals 100
  display (InWindow "haskell-game" (800,600) (30,30)) red (mkWorld sigs)

res :: Int
res = 600

mkWorld :: [Signal] -> Picture
mkWorld sigs = bitmapOfByteString res res (datas sigs) True

datas sigs = pack $ Prelude.concatMap colorize $ normalize $ sampleArea sigs

numSignals :: Int -> IO [Signal]
numSignals 0 = return []
numSignals n = do
  sigs <- numSignals (n-1)
  sig <- randomSignal
  return (sig:sigs)

randomSignal :: IO Signal
randomSignal = do
  rnd <- randomRIO (0,1) :: IO Float
  if rnd > 0.8 then randomSlowSignal else randomFastSignal

randomSlowSignal = do
  amp <- randomRIO (0, 3)
  wav <- randomRIO (0.1, 0.3)
  phase <- randomRIO (0, 2 * pi)
  print (amp, wav)
  posx <- randomRIO (0,1)
  posy <- randomRIO (0,1)
  return $ Signal amp wav phase $ Pos posx posy

randomFastSignal = do
  amp <- randomRIO (0, 1)
  wav <- randomRIO (0.01, 0.1)
  phase <- randomRIO (0, 2 * pi)
  print (amp, wav)
  posx <- randomRIO (0,1)
  posy <- randomRIO (0,1)
  return $ Signal amp wav phase $ Pos posx posy

sampleArea signals = [sampleAll signals (Pos x y) | x <- Prelude.take res [0, (1 / fromIntegral res) ..], y <- Prelude.take res [0, (1 / fromIntegral res) ..]]

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

data Signal = Signal Float Float Float Pos

data Pos = Pos Float Float

distance :: Pos -> Pos -> Float
distance (Pos x y) (Pos x2 y2) = sqrt (((x-x2)^2) + ((y-y2)^2))

sample :: Signal -> Pos -> Float
sample (Signal amp wavelen initialPhase p1) p2 = amp * (0.5 + 0.5 * sin (dist/wavelen + initialPhase))
  where
    dist = distance p1 p2
