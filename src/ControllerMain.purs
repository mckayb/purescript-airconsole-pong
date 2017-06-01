module ControllerMain where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import AirConsole.Global ( getAirConsoleGlobal
                         , orientationPortrait
                         , onMessage
                         )
import Views.ControllerStart (view)
import Views.FFI (renderToSel, onDOMContentLoaded)
import Data.Maybe (Maybe(Nothing))

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = onDOMContentLoaded do
  _ <- renderToSel "body" (view Nothing)
  ac <- getAirConsoleGlobal { orientation: orientationPortrait }
  _ <- onMessage (\d x -> log "In here dood!") ac
  log "Controller Ready"
