module Views.ControllerStart where

import Prelude (discard, ($))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Text.Smolder.HTML (div, button)
import Text.Smolder.HTML.Attributes (className, height)
import Text.Smolder.Markup (text, on, (#!), (!))
import Text.Smolder.Renderer.String (render)
import AirConsole.Types (AirConsoleGlobal)
import AirConsole.Global (screen)
import AirConsole.Messaging (message)

view :: AirConsoleGlobal -> String
view ac = render $ div ! className "view default-view" $ do
    div ! height "1%" $ text ""
    div ! className "button" $ do
        div ! className "button_label" $ text "UP"
    div ! height "8%" $ text ""
    div #! on "touchstart" (\_ -> log "Touch Start!")
        #! on "touchend" (\_ -> log "Touch End!")
        #! on "mousedown" (\_ -> log "Mouse down!")
        #! on "mouseup" (\_ -> log "Mouse up!")
        ! className "button" $ do
        div ! className "button_label" $ text "DOWN"
    div ! className "player_id" $ text "It's a 2 player game"
    button #! on "click" (\_ -> log "CLICKED DOOD!") $ text "click me!"

move :: forall a b c. AirConsoleGlobal -> a -> Eff b c
move ac = \_ -> message ac screen { move: 50 }
