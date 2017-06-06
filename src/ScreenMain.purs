module AirConsolePong.ScreenMain where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import AirConsole.Global ( getAirConsoleGlobal
                         , orientationLandscape
                         , onMessage
                         , onReady
                         , onConnect
                         , onDisconnect
                         )
import AirConsole.Types (AirConsoleGlobal, DeviceId, PlayerNumber)
import AirConsole.ActivePlayers ( getActivePlayerDeviceIds
                                , setActivePlayers
                                , convertDeviceIdToPlayerNumber
                                )
import AirConsole.Connectivity (getControllerDeviceIds)
import AirConsolePong.Views.FFI (onDOMContentLoaded)
import AirConsolePong.Views.ScreenStart ( view
                                        , updateDOMWait
                                        , updateDOMScore
                                        )
import AirConsolePong.Types (GameState)
import Data.Array (length)
import Data.Nullable (toMaybe)
import Data.Maybe (Maybe(Just, Nothing))
import DOM (DOM)

foreign import requestAnimationFrame :: forall e . (Number -> Eff e Unit) -> Eff e Unit
foreign import showStuff :: forall a. a -> Eff (console :: CONSOLE) Unit

{- handleNewConnection :: AirConsoleGlobal -> GameState -> Eff (dom :: DOM) Unit
handleNewConnection ac gs =
    let
        apLength :: Int
        apLength = (length <<< getActivePlayerDeviceIds) ac

        cdLength :: Int
        cdLength = (length <<< getControllerDeviceIds) ac

        numNeeded :: Int
        numNeeded = 2 - cdLength

        playerStr :: String
        playerStr = if numNeeded == 1
                        then "player!"
                    else if numNeeded >= 2
                        then "players!"
                    else ""
     in
         if apLength == 0 && cdLength >= 2
            then do
                _ <- setActivePlayers ac 2
                gs' <- (pure <<< resetBall 50.0 0.0 <<< resetScore) initGameState
                _ <- updateDOMWait ""
                _ <- updateDOMScore gs'.score
                pure unit
        else if apLength == 0
            then do
                _ <- updateDOMWait ("Need " <> show numNeeded <> " more " <> playerStr)
                _ <- pure (resetBall 0.0 0.0 gs)
                pure unit
            else pure unit

handleDisconnect :: AirConsoleGlobal -> GameState -> DeviceId -> Eff (dom :: DOM) Unit
handleDisconnect ac gs d = do
    fp <- (pure <<< toMaybe <<< convertDeviceIdToPlayerNumber ac) d
    _ <- case fp of
              Just pn -> setActivePlayers ac 0
              Nothing -> pure unit
    handleNewConnection ac gs

handleMessage :: AirConsoleGlobal -> GameState -> DeviceId -> { move :: Number } -> Eff (dom :: DOM) Unit
handleMessage ac gs d { move: x } = do
    fp <- (pure <<< toMaybe <<< convertDeviceIdToPlayerNumber ac) d
    pure unit

resetScore :: GameState -> GameState
resetScore gs = gs { score { p1 = 0, p2 = 0 } }

resetBall :: Number -> Number -> GameState -> GameState
resetBall moveX moveY gs = gs { ball { pos { x = 100.0, y = 50.0 }
                                     , move { x = moveX, y = moveY }
                                     }
                              }

main :: Eff (dom :: DOM, console :: CONSOLE) Unit
main = onDOMContentLoaded do
    ac <- getAirConsoleGlobal { orientation: orientationLandscape }
    view ac
    _ <- onConnect (\d -> handleNewConnection ac initGameState) ac
    _ <- onMessage (\d x -> handleMessage ac initGameState d x) ac
    _ <- onReady (\c -> log "Ready Bro") ac
    _ <- onDisconnect (\d -> handleDisconnect ac initGameState d) ac
    log "Screen Is Ready" -}

handleNewConnection :: AirConsoleGlobal -> GameState -> Eff (dom :: DOM) Unit
handleNewConnection ac gs =
    let
        apLength :: Int
        apLength = (length <<< getActivePlayerDeviceIds) ac

        cdLength :: Int
        cdLength = (length <<< getControllerDeviceIds) ac

        numNeeded :: Int
        numNeeded = 2 - cdLength

        playerStr :: String
        playerStr = if numNeeded == 1
                        then "player!"
                    else if numNeeded >= 2
                        then "players!"
                    else ""

     in
         if apLength == 0 && cdLength >= 2
            then do
                _ <- setActivePlayers ac 2
                _ <- runGame ac initGameState
                _ <- updateDOMWait ""
                _ <- updateDOMScore gs'.score
                pure unit
        else if apLength == 0
            then do
                _ <- updateDOMWait ("Need " <> show numNeeded <> " more " <> playerStr)
                _ <- pure (resetBall 0.0 0.0 gs)
                pure unit
        else pure unit


runGame :: forall e. AirConsoleGlobal -> GameEnvironment -> Eff (dom :: DOM | e) Unit
runGame ac env = do

main = onDOMContentLoaded do
    ac <- getAirConsoleGlobal { orientation: orientationLandscape }
    view ac
    _ <- onConnect (\d -> handleNewConnection ac initGameState) ac

