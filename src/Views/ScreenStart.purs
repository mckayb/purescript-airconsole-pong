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
import Text.Smolder.HTML.Attributes (id, className)
import Text.Smolder.Markup (Markup, text, (!))
import Text.Smolder.Renderer.DOM (render) as S
import AirConsolePong.GameModel (Game)
import AirConsolePong.Views.FFI (getClientHeight, bitwiseOr)
import Graphics.Canvas ( CANVAS
                       , CanvasElement
                       , getContext2D
                       )
import Graphics.Drawing ( render , fillColor
                        , filled, rectangle, circle
                        , Drawing, Shape, FillStyle
                        )
import Color (black, white)
import Prelude hiding (div, id)

view :: forall eff. AirConsoleGlobal -> Eff (dom :: DOM | eff) Unit
view ac = do
    doc <- window >>= document
    body <- map htmlElementToElement <$> body doc
    _ <- let
            markup :: forall e. Markup (EventListener ( | e))
            markup =
                div ! className "game view default-view" $ do
                    canvas ! id "game-canvas" ! className "game__canvas" $ text ""
          in
            case body of
                 Just x -> S.render x markup
                 Nothing -> pure unit
    pure unit

drawGame :: forall eff. Game -> CanvasElement -> Boolean -> Eff (canvas :: CANVAS | eff) Unit
drawGame m canvas clear = do
    ctx <- getContext2D canvas
    ch <- getClientHeight canvas
    divch <- pure (ch / 100.0)
    fs <- pure (fillColor (if clear then black else white))
    zoom <- pure (\x -> bitwiseOr (x * divch))
    render ctx $
        paddleDrawing (zoom m.p1.x) (zoom m.p1.y)
        <> paddleDrawing (zoom m.p2.x) (zoom m.p2.y)
        <> ballDrawing (zoom m.ball.x) (zoom m.ball.y)
    pure unit

ballDrawing :: Number -> Number -> Drawing
ballDrawing x y = filled fillStyle shape
    where
          fillStyle :: FillStyle
          fillStyle = fillColor white

          shape :: Shape
          shape = circle x y 6.0

paddleDrawing :: Number -> Number -> Drawing
paddleDrawing x y = filled fillStyle shape
  where
        fillStyle :: FillStyle
        fillStyle = fillColor white

        shape :: Shape
        shape = rectangle x y 10.0 100.0


{- updateDOMWait :: forall eff. String -> Eff (dom :: DOM | eff) Unit
updateDOMWait str = renderToSel ".game__score" str

updateDOMScore :: forall eff. PlayerScore -> PlayerScore -> Eff (dom :: DOM | eff) Unit
updateDOMScore p1Score p2Score = renderToSel ".game__wait" scoreText
  where scoreText = (show p1Score) <> " : " <> (show p2Score) -}
