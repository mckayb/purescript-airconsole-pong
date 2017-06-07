module AirConsolePong.ControllerMain where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import AirConsole.Global ( getAirConsoleGlobal
                         , orientationPortrait
                         )
import AirConsolePong.Views.ControllerStart (view)
import DOM (DOM)

main :: forall e. Eff (dom :: DOM, console :: CONSOLE | e) Unit
main = do
  ac <- getAirConsoleGlobal { orientation: orientationPortrait }
  view ac
  log "Controller Ready!"
