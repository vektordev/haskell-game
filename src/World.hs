module World (
  World (..),
  Signal (..),
  Pos (..),
  mkWorld
) where

import System.Random

res :: Int
res = 600

data World = World [Signal] Int Int deriving (Show, Read)

data Signal = Signal Float Float Float Pos deriving (Show, Read)

data Pos = Pos Float Float deriving (Show, Read)

mkWorld :: Int -> Int -> Int -> Int -> World
mkWorld seed xres yres sigCount = World (numSignals sigCount (mkStdGen seed)) xres yres -- TODO

numSignals :: Int -> StdGen -> [Signal]
numSignals 0 rng = []
numSignals n rng =
  let
    sigs = numSignals (n-1) (fst $ split rng)
    sig = randomSignal (snd $ split rng)
  in (sig:sigs)

randomSignal :: StdGen -> Signal
randomSignal rng =
  let (rnd, rng2) = randomR (0,1) rng :: (Float, StdGen)
  in if rnd > 0.8 then randomSlowSignal rng2 else randomFastSignal rng2

randomSlowSignal :: StdGen -> Signal
randomSlowSignal rng =
  let
    (posx, g1) = random rng
    (posy, g2) = random g1
    (amp,g3) = randomR (0, 3) g2
    (wav, g4) = randomR (0.1, 0.3) g3
    (phase, g5) = randomR (0, 2 * pi) g4
  in Signal amp wav phase $ Pos posx posy

randomFastSignal :: StdGen -> Signal
randomFastSignal rng =
  let
    (posx, g1) = random rng
    (posy, g2) = random g1
    (amp,g3) = randomR (0, 1) g2
    (wav, g4) = randomR (0.01, 0.1) g3
    (phase, g5) = randomR (0, 2 * pi) g4
  in Signal amp wav phase $ Pos posx posy
