module AirConsolePong.ScreenMain where

import Prelude (Unit, negate, discard, bind, pure, unit, (<$>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Timer (TIMER)
import AirConsole.Global ( getAirConsoleGlobal , orientationLandscape
                         , onConnect , onReady , onMessage , onDisconnect
                         )
import AirConsolePong.Views.FFI (updateCanvasDim, showStuff)
import AirConsolePong.Views.ScreenStart (view , drawGame)
import AirConsolePong.GameModel (initialGameState)
import AirConsolePong.GameUpdate (gameLogic)
import Data.Maybe (Maybe(Just, Nothing))
import DOM (DOM)
import Graphics.Canvas (CANVAS, getCanvasElementById)
import Signal (foldp, runSignal, sampleOn)
import Signal.DOM (animationFrame)
import Signal.Channel (channel, send, subscribe, Channel, CHANNEL)

type Input = { p1 :: { move :: Number }
             , p2 :: { move :: Number }
             , ball :: { x :: Number, y :: Number }
             }

startGame
    :: forall eff
     . Channel Input
    -> Eff ( dom :: DOM
           , timer :: TIMER
           , console :: CONSOLE
           , canvas :: CANVAS | eff
           ) Unit
startGame channel = do
    mcanvas <- getCanvasElementById "game-canvas"
    case mcanvas of
        Just canvas -> do
            _ <- updateCanvasDim canvas
            frames <- animationFrame
            let inputs = subscribe channel
            let game = foldp gameLogic initialGameState (sampleOn frames inputs)
            runSignal (drawGame canvas true <$> game)
            pure unit
        Nothing -> pure unit

main
    :: forall e
     . Eff ( channel :: CHANNEL
           , dom :: DOM
           , canvas :: CANVAS
           , console :: CONSOLE
           , timer :: TIMER | e
           ) Unit
main = do
    ac <- getAirConsoleGlobal { orientation: orientationLandscape }
    ch <- channel { p1: { move: 0.0 }
                  , p2: { move: 0.0 }
                  , ball: { x: 0.5, y: 0.2 }
                  }
    view ac
    _ <- onConnect (\d -> log "on Connection") ac
    _ <- onMessage (\d x -> do
                        log "Message: "
                        showStuff d
                        showStuff x
                        send ch { p1: { move: 1.0 }
                                , p2: { move: -1.0 }
                                , ball: { x: 0.5, y: 0.2 }
                                }
                   ) ac
    _ <- onReady (\d -> log "on Ready") ac
    _ <- onDisconnect (\d -> log "on Disconnect") ac
    startGame ch
    log "Running"

