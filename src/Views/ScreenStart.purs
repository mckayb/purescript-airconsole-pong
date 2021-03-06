module AirConsolePong.Views.ScreenStart where

import AirConsole.Types (AirConsoleGlobal)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
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
import AirConsolePong.Game (Game)
import AirConsolePong.Views.FFI (clearCanvas)
import Graphics.Canvas ( CANVAS
                       , CanvasElement
                       , getContext2D
                       , getCanvasWidth
                       , getCanvasHeight
                       )
import Graphics.Drawing ( render, fillColor
                        , filled, rectangle, circle
                        , Drawing, Shape, FillStyle
                        )
import Graphics.Drawing (text) as GD
import Graphics.Drawing.Font (fantasy, bold, font, Font)
import Color (white)
import Prelude ( Unit, map, bind, show, pure, unit, discard
               , (/), (-), (*), ($), (<$>), (>>=), (<>)
               )

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

scoreDrawing :: Number -> Number -> Int -> Int -> Drawing
scoreDrawing x y scoreP1 scoreP2 = GD.text f x y fillStyle scoreText
  where
    f :: Font
    f = font fantasy 64 bold

    fillStyle :: FillStyle
    fillStyle = fillColor white

    scoreText :: String
    scoreText = show scoreP1 <> " : " <> show scoreP2

drawGame
    :: forall eff
     . CanvasElement
    -> Boolean
    -> Game
    -> Eff (console :: CONSOLE, canvas :: CANVAS | eff) Unit
drawGame canvas clear m = do
    _ <- clearCanvas canvas
    ctx <- getContext2D canvas
    ch <- getCanvasHeight canvas
    cw <- getCanvasWidth canvas

    -- On a canvas, (0,0) is the top left.
    -- To make it easier for me, I want to reflect
    -- all points to have (0,0) be the bottom left
    -- I also want x in [0, 200] and y in [0, 100]
    let scaleX = \n -> n * (cw / 200.0)
    let scaleY = \n -> (100.0 - n) * (ch / 100.0)

    render ctx $
        scoreDrawing (scaleX 95.0) (scaleY 90.0) m.p1.score m.p2.score
        <> paddleDrawing (scaleX m.p1.x) (scaleY m.p1.y)
        <> paddleDrawing (scaleX m.p2.x) (scaleY m.p2.y)
        <> ballDrawing (scaleX m.ball.x) (scaleY m.ball.y)
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
