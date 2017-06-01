module Views.ControllerStart where

import Prelude (discard, ($))
import Text.Smolder.HTML (div)
import Text.Smolder.HTML.Attributes (className, height)
import Text.Smolder.Markup (Markup, text, (!))
import Text.Smolder.Renderer.String (render)
import Data.Maybe (Maybe(Just, Nothing))

view :: Maybe String -> String
view player = render $ div ! className "view default-view" $ do
    div ! height "1%" $ text ""
    div ! className "button" $ do
        div ! className "button_label" $ text "UP"
    div ! height "8%" $ text ""
    div ! className "button" $ do
        div ! className "button_label" $ text "DOWN"
    div ! className "player_id" $ text playerText
  where
    playerText :: String
    playerText = case player of
                      Just x -> x
                      Nothing -> "It's a 2 player game"
