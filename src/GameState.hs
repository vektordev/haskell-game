module GameState (
  module GameStateTypes,
  step,
  initialGameState,
  getByID
) where

import InputState
import World
import Debug.Trace
import GameStateTypes
import GameStateUtil

--initial = GameState [Entity 0 0 True, Entity 40 40 False] (World [Signal 1 1 1 (Pos 0.5 0.5)] 600 600)
initialGameState :: GameState
initialGameState = GameState [Entity 0 0 0 0, Entity 1 40 40 0] (mkWorld 10 800 800 6)

step :: [InputState] -> GameState -> GameState
step inps st = trace ("length input: " ++ show (length inps)) st{entities = ents'}
  where
    ents' :: [Entity]
    ents' = map (stepEntity st . applyInputs inps st) $ entities st
    applyInputs :: [InputState] -> GameState -> Entity -> Entity
    applyInputs inputs gs e = foldr (\i ent -> applyInput i gs ent) e inputs

applyInput :: InputState -> GameState -> Entity -> Entity
applyInput (InputState iDEnt right left up down strafeL strafeR) g ent@(Entity iD x y ang) = trace "try" (if iDEnt == iD then Entity iD x' y' ang' else ent)
  where
    ang' = ang - (move right - move left)
    speed = move up - move down
    x' = x + round (speed * cos (angleToRad ang'))
    y' = y + round (speed * sin (angleToRad ang'))
    move bool = if bool then controlAmplification else 0
    controlAmplification = 3

angleToRad ang = ang / 180 * pi

stepEntity :: GameState -> Entity -> Entity
stepEntity g = id

getByID :: GameState -> Int -> Maybe Entity
getByID gs id = if null entitiesWithID then Nothing else Just (head entitiesWithID)
  where entitiesWithID = filter (\x -> id == ident x) (entities gs)
