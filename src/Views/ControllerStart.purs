module Views.ControllerStart where

import Control.Monad.Eff (Eff)
import Text.Smolder.HTML (div)
import Text.Smolder.HTML.Attributes (className, height)
import Text.Smolder.Markup (Markup, text, on, (#!), (!))
import Text.Smolder.Renderer.DOM (render)
import AirConsole.Types (AirConsoleGlobal)
import AirConsole.Global (screen)
import AirConsole.Messaging (message)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (document)
import DOM.HTML.Document (body)
import DOM.HTML.Types (htmlElementToElement)
import DOM.Node.Types (Element)
import DOM.Event.EventTarget (EventListener, eventListener)
import Data.Maybe (Maybe(Just, Nothing))
import Prelude hiding (div)

view :: forall eff. AirConsoleGlobal -> Eff (dom :: DOM | eff) Unit
view ac = do
    doc <- window >>= document
    body <- map htmlElementToElement <$> body doc
    _ <- let
            startListener = eventListener (move ac 0)
            endListener = eventListener (move ac 50)

            markup :: forall e. Markup (EventListener ( | e))
            markup =
                div ! className "view default-view" $ do
                div ! height "1%" $ text ""
                div #! on "touchstart" startListener
                    #! on "touchend" endListener
                    #! on "mousedown" startListener
                    #! on "mouseup" endListener
                    ! className "button" $ do
                    div ! className "button_label" $ text "UP"
                div ! height "8%" $ text ""
                div #! on "touchstart" startListener
                    #! on "touchend" endListener
                    #! on "mousedown" startListener
                    #! on "mouseup" endListener
                    ! className "button" $ do
                        div ! className "button_label" $ text "DOWN"
                div ! className "player_id" $ text "It's a 2 player game"

            renderMaybeToElem :: forall e. Element -> Eff (dom :: DOM | e) Unit
            renderMaybeToElem = render' markup
          in
            case body of
                 Just x -> renderMaybeToElem x
                 Nothing -> pure unit
    pure unit

render'
    :: forall e
     . Markup (EventListener (dom :: DOM | e))
    -> Element
    -> Eff (dom :: DOM | e) Unit
render' a m = render m a

move :: forall a b c. AirConsoleGlobal -> Int -> a -> Eff b c
move ac n = \_ -> message ac screen { move: n }
