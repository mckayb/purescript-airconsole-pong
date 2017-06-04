module AirConsolePong.Views.ScreenStart where

import AirConsole.Types (AirConsoleGlobal)
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(Just, Nothing))
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (document)
import DOM.HTML.Document (body)
import DOM.HTML.Types (htmlElementToElement)
import DOM.Event.EventTarget (EventListener)
import Text.Smolder.HTML (div, canvas)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup (Markup, text, (!))
import Text.Smolder.Renderer.DOM (render)
import Prelude hiding (div)

view :: forall eff. AirConsoleGlobal -> Eff (dom :: DOM | eff) Unit
view ac = do
    doc <- window >>= document
    body <- map htmlElementToElement <$> body doc
    _ <- let
            markup :: forall e. Markup (EventListener ( | e))
            markup =
                div ! className "game" $ do
                    canvas ! className "game__canvas" $ text ""
                    div ! className "game__score" $ text "0:0"
                    div ! className "game__wait" $ text ""
          in
            case body of
                 Just x -> render x markup
                 Nothing -> pure unit
    pure unit
