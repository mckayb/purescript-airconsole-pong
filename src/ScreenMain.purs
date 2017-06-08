module AirConsolePong.ScreenMain where

import Prelude (Unit, discard, bind, pure, unit, ($), (<$>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Timer (TIMER)
import AirConsole.Global ( getAirConsoleGlobal , orientationLandscape
                         , onConnect , onReady , onMessage , onDisconnect
                         )
import AirConsole.Types (AirConsoleGlobal)
import AirConsolePong.Views.FFI (updateCanvasDim)
import AirConsolePong.Views.ScreenStart (view , drawGame)
import AirConsolePong.GameModel (initialGameState)
import AirConsolePong.GameUpdate (gameLogic)
import Data.Maybe (Maybe(Just, Nothing))
import DOM (DOM)
import Graphics.Canvas (CANVAS, getCanvasElementById)
import Signal (constant, foldp, runSignal, sampleOn)
import Signal.DOM (animationFrame)
-- import Signal.Channel (channel, send, subscribe, CHANNEL)

startGame
    :: forall eff
     . AirConsoleGlobal
    -> Eff ( dom :: DOM
           , timer :: TIMER
           , console :: CONSOLE
           , canvas :: CANVAS | eff
           ) Unit
startGame ac = do
    mcanvas <- getCanvasElementById "game-canvas"
    case mcanvas of
        Just canvas -> do
            _ <- updateCanvasDim canvas
            frames <- animationFrame
            let inputs = constant { p1: { move: 0.0 }
                                  , p2: { move: 0.0 }
                                  , ball: { x: 0.5, y: 0.2 }
                                  }
            let game = foldp gameLogic initialGameState (sampleOn frames inputs)
            runSignal (drawGame canvas true <$> game)
            pure unit
        Nothing -> pure unit

main :: forall e. Eff (dom :: DOM, canvas :: CANVAS, console :: CONSOLE, timer :: TIMER | e) Unit
main = do
    ac <- getAirConsoleGlobal { orientation: orientationLandscape }
    view ac
    _ <- onConnect (\d -> log "on Connection") ac
    _ <- onMessage (\d x -> do
                        log "Blah"
                        pure $ constant x
                   ) ac
    _ <- onReady (\d -> log "on Ready") ac
    _ <- onDisconnect (\d -> log "on Disconnect") ac
    startGame ac
    log "Running"

