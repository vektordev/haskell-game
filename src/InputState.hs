module InputState (
  InputState (..),
  initialInputState
) where

data InputState = InputState {
  mvRight :: Bool,
  mvLeft :: Bool,
  mvUp :: Bool,
  mvDown :: Bool
} deriving (Show, Read, Eq)

initialInputState = InputState False False False False
