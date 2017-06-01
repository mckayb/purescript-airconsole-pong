module ControllerMain where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import AirConsole.Global ( getAirConsoleGlobal
                         , orientationPortrait
                         , onMessage
                         , onActivePlayersChange
                         )

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  ac <- getAirConsoleGlobal { orientation: orientationPortrait }
  _ <- onMessage (\d x -> log "In here dood!") ac
  log "Controller Ready"
