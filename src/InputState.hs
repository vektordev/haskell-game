module InputState (
  InputState (..),
  initialInputState
) where

data InputState = InputState {
  controlledEntity :: Int,
  mvRight :: Bool,
  mvLeft :: Bool,
  mvUp :: Bool,
  mvDown :: Bool
} deriving (Show, Read, Eq)

initialInputState = InputState 0 False False False False
