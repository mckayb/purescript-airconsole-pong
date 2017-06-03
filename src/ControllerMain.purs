module ControllerMain where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import AirConsole.Global ( getAirConsoleGlobal
                         , orientationPortrait
                         )
import Views.ControllerStart (view)
import Views.FFI (onDOMContentLoaded)
import DOM (DOM)

foreign import isNullOrUndefined :: forall a. a -> Boolean

main :: forall e. Eff (dom :: DOM, console :: CONSOLE | e) Unit
main = onDOMContentLoaded do
  ac <- getAirConsoleGlobal { orientation: orientationPortrait }
  view ac
  log "Controller Ready!"
