module InputState (
  InputState (..),
  initialInputState
) where

data InputState = InputState {
  mvRight :: Bool,
  mvLeft :: Bool,
  mvUp :: Bool,
  mvDown :: Bool
}

initialInputState = InputState False False False False
