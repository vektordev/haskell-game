module InputHandler (
  handleEvt
) where

import Graphics.Gloss.Interface.IO.Game
import GameState
import InputState


noModifiers = Modifiers Up Up Up

handleEvt :: Event -> InputState -> InputState
handleEvt (EventKey (SpecialKey KeyEsc) Down _ _) state = undefined -- crash the app :/
handleEvt (EventKey (SpecialKey KeyRight) Down noModifiers flts) state = state {mvRight = True}
handleEvt (EventKey (SpecialKey KeyRight) Up noModifiers flts) state = state {mvRight = False}
handleEvt (EventKey (SpecialKey KeyLeft) Down noModifiers flts) state = state {mvLeft = True}
handleEvt (EventKey (SpecialKey KeyLeft) Up noModifiers flts) state = state {mvLeft = False}
handleEvt (EventKey (SpecialKey KeyUp) Down noModifiers flts) state = state {mvUp = True}
handleEvt (EventKey (SpecialKey KeyUp) Up noModifiers flts) state = state {mvUp = False}
handleEvt (EventKey (SpecialKey KeyDown) Down noModifiers flts) state = state {mvDown = True}
handleEvt (EventKey (SpecialKey KeyDown) Up noModifiers flts) state = state {mvDown = False}
handleEvt evt state = state
