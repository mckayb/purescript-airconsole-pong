module AirConsolePong.ScreenMain where

import Prelude (Unit, discard, bind, pure, unit, (<$>), (<<<), (==), (>=), (&&))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Timer (TIMER)
import AirConsole.Global ( getAirConsoleGlobal, orientationLandscape
                         , onConnect, onMessage, onDisconnect
                         )
import AirConsole.Types (AirConsoleGlobal, DeviceId)
import AirConsole.ActivePlayers (convertDeviceIdToPlayerNumber, getActivePlayerDeviceIds, setActivePlayers)
import AirConsole.Connectivity (getControllerDeviceIds)
import AirConsolePong.Views.FFI (updateCanvasDim, showStuff)
import AirConsolePong.Views.ScreenStart (view , drawGame)
import AirConsolePong.Game.Model (initialGameState)
import AirConsolePong.Game.Update (gameLogic, Input)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Nullable (toMaybe)
import Data.Array (length)
import DOM (DOM)
import Graphics.Canvas (CANVAS, getCanvasElementById)
import Signal (foldp, runSignal, sampleOn)
import Signal.DOM (animationFrame)
import Signal.Channel (channel, send, subscribe, Channel, CHANNEL)

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

handleConnection
    :: forall e
     . AirConsoleGlobal
    -> Channel Input
    -> DeviceId
    -> Eff ( dom :: DOM
           , timer :: TIMER
           , console :: CONSOLE
           , canvas :: CANVAS
           | e ) Unit
handleConnection ac ch d =
    let
        apLength :: Int
        apLength = (length <<< getActivePlayerDeviceIds) ac

        cdLength :: Int
        cdLength = (length <<< getControllerDeviceIds) ac
     in
        if apLength == 0 && cdLength >= 2
           then do
               _ <- setActivePlayers ac 2
               startGame ch
        else if apLength == 0 && cdLength == 1
            then do
               pure unit
        else if apLength == 0 && cdLength == 0
            then do
                pure unit
        else do
            pure unit

handleDisconnect
    :: forall e
     . AirConsoleGlobal
    -> Channel Input
    -> DeviceId
    -> Eff ( dom :: DOM
           , timer :: TIMER
           , console :: CONSOLE
           , canvas :: CANVAS
           | e ) Unit
handleDisconnect ac ch d = do
    mpn <- pure ((toMaybe <<< convertDeviceIdToPlayerNumber ac) d)
    case mpn of
         Just pn -> do
             _ <- setActivePlayers ac 0
             pure unit
         Nothing -> pure unit
    handleConnection ac ch d

handleMessage
    :: forall e a
     . AirConsoleGlobal
    -> Channel Input
    -> DeviceId
    -> { move :: Number | a }
    -> Eff (console :: CONSOLE, channel :: CHANNEL | e) Unit
handleMessage ac ch d x = do
    mpn <- pure ((toMaybe <<< convertDeviceIdToPlayerNumber ac) d)
    case mpn of
         Just 0 -> log "Player 1"
         Just 1 -> log "Player 2"
         _ -> log "Nothing"
    send ch { p1: { move: x.move }
            , p2: { move: 0.0 }
            , ball: { x: 0.0, y: 0.0 }
            }

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
                  , ball: { x: 0.0, y: 0.0 }
                  }
    view ac
    _ <- onConnect (handleConnection ac ch) ac
    _ <- onMessage (handleMessage ac ch) ac
    _ <- onDisconnect (handleDisconnect ac ch) ac
    startGame ch
    log "Running"

