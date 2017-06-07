module AirConsolePong.ScreenMain where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import AirConsole.Global ( getAirConsoleGlobal
                         , orientationLandscape
                         , onConnect
                         , onReady
                         , onMessage
                         , onDisconnect
                         )
import AirConsolePong.Views.FFI (updateCanvasDim)
import AirConsolePong.Views.ScreenStart (view , drawGame)
import AirConsolePong.GameModel (Game, initialGameState)
import Data.Maybe (Maybe(Just, Nothing))
import DOM (DOM)
import Graphics.Canvas (CANVAS, getCanvasElementById)

foreign import requestAnimationFrame :: forall e . (Number -> Eff e Unit) -> Eff e Unit
foreign import showStuff :: forall a. a -> Eff (console :: CONSOLE) Unit

startGame :: forall eff. Game -> Eff (canvas :: CANVAS | eff) Unit
startGame gs = do
    mcanvas <- getCanvasElementById "game-canvas"
    case mcanvas of
         Just canvas -> do
             _ <- updateCanvasDim canvas
             drawGame gs canvas true
         Nothing -> pure unit

main :: forall e. Eff (dom :: DOM, canvas :: CANVAS, console :: CONSOLE | e) Unit
main = do
    ac <- getAirConsoleGlobal { orientation: orientationLandscape }
    view ac
    startGame initialGameState
    _ <- onConnect (\d -> log "on Connection") ac
    _ <- onMessage (\d x -> log "on Message") ac
    _ <- onReady (\d -> log "on Ready") ac
    _ <- onDisconnect (\d -> log "on Disconnect") ac
    log "Running"

