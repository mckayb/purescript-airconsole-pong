module ControllerMain where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import AirConsole.Global ( getAirConsoleGlobal
                         , orientationPortrait
                         , screen
                         , onMessage
                         , onActivePlayersChange
                         )
import AirConsole.ControllerInputs (vibrate)
import Views.ControllerStart (view)
import Views.FFI (renderToSel, onDOMContentLoaded)

import Text.Smolder.HTML (button)
import Text.Smolder.Markup (text, on, (#!))
import Text.Smolder.Renderer.String (render)
import DOM (DOM)

foreign import isNullOrUndefined :: forall a. a -> Boolean

main :: forall e. Eff (dom :: DOM, console :: CONSOLE | e) Unit
main = onDOMContentLoaded do
  ac <- getAirConsoleGlobal { orientation: orientationPortrait }
  view ac
  log "Done!"
  -- _ <- onMessage
        -- (\from d -> if d.vibrate && from == screen
                    -- then vibrate ac 1000
                    -- else pure unit) ac
  -- _ <- onActivePlayersChange
        -- (\d -> if isNullOrUndefined d
               -- then renderToSel ".player_id" "It's a 2 player game!"
               -- else renderToSel ".player_id" ("Player " <> show d)) ac
  -- log "Controller Ready"
