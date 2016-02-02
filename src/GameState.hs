module GameState (
  GameState (..),
  Entity (..),
  step,
  initialGameState
) where

import InputState
import World

--initial = GameState [Entity 0 0 True, Entity 40 40 False] (World [Signal 1 1 1 (Pos 0.5 0.5)] 600 600)
initialGameState = GameState [Entity 0 0 True, Entity 40 40 False] (mkWorld 10 400 400 60)

data GameState = GameState {
  entities :: [Entity],
  world :: World
} deriving (Show, Read)

data Entity = Entity {
  posX :: Int,
  posY :: Int,
  controlled :: Bool
} deriving (Show, Read)

step :: InputState -> GameState -> GameState
step inp st = st{entities = map (stepEntity st . applyInput inp st) $ entities st}

applyInput :: InputState -> GameState -> Entity -> Entity
applyInput (InputState right left up down) g (Entity x y True) = Entity (x + deltaX) (y + deltaY) True
  where
    deltaX = move right - move left
    deltaY = move up - move down
    move bool = if bool then speed else 0
    speed = 3
applyInput i g e = e

stepEntity :: GameState -> Entity -> Entity
stepEntity g = id
