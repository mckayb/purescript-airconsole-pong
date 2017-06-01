module Views.ControllerStart where

import Prelude (discard, ($))
import Text.Smolder.HTML (div)
import Text.Smolder.HTML.Attributes (className, style)
import Text.Smolder.Markup (text, (!))

view player =
    div ! style "height: \"1%\""
    div ! className "button" $ do
        div ! className "button_label" $ text "UP"
    div ! style "height: \"8%\""
    div ! className "button" $ do
        div ! className "button_label" $ text "DOWN"
    div ! className "player_id" $ text "It's a 2 player game"
