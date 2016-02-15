module GameStateTypes (
  GameState (..),
  Entity (..)
) where

import World

data GameState = GameState {
  entities :: [Entity],
  world :: World
} deriving (Show, Read)

data Entity = Entity {
  ident :: Int,
  posX :: Int,
  posY :: Int,
  rotation :: Float
} deriving (Show, Read, Eq)

--TODO: for robustness, manual Eq Instance by ID?
