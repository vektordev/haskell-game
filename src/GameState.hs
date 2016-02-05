module GameState (
  GameState (..),
  Entity (..),
  step,
  initialGameState
) where

import InputState
import World
import Debug.Trace

--initial = GameState [Entity 0 0 True, Entity 40 40 False] (World [Signal 1 1 1 (Pos 0.5 0.5)] 600 600)
initialGameState :: GameState
initialGameState = GameState [Entity 0 0 0 True, Entity 1 40 40 True] (mkWorld 10 400 400 5)

data GameState = GameState {
  entities :: [Entity],
  world :: World
} deriving (Show, Read)

data Entity = Entity {
  ident :: Int,
  posX :: Int,
  posY :: Int,
  controlled :: Bool
} deriving (Show, Read)

step :: [InputState] -> GameState -> GameState
step inps st = trace ("length input: " ++ (show $ length inps)) st{entities = ents'}
  where
    ents' :: [Entity]
    ents' = map (stepEntity st . applyInputs inps st) $ entities st
    applyInputs :: [InputState] -> GameState -> Entity -> Entity
    applyInputs inputs gs e = foldr (\i ent -> applyInput i gs ent) e inputs

applyInput :: InputState -> GameState -> Entity -> Entity
applyInput (InputState iDEnt right left up down) g ent@(Entity iD x y True) = trace "try" (if iDEnt == iD then trace "apply" $ Entity iD (x + deltaX) (y + deltaY) True else ent)
  where
    deltaX = move right - move left
    deltaY = move up - move down
    move bool = if bool then speed else 0
    speed = 3
applyInput i g e = e

stepEntity :: GameState -> Entity -> Entity
stepEntity g = id
