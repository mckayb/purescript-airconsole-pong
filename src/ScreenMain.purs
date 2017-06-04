module AirConsolePong.ScreenMain where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import AirConsole.Global ( getAirConsoleGlobal
                         , orientationLandscape
                         -- , onMessage
                         -- , onReady
                         , onConnect
                         -- , onDisconnect
                         )
import AirConsole.Types (AirConsoleGlobal, DeviceId)
import AirConsole.ActivePlayers ( getActivePlayerDeviceIds
                                , setActivePlayers
                                )
import AirConsole.Connectivity (getControllerDeviceIds)
import AirConsolePong.Views.FFI (onDOMContentLoaded)
import AirConsolePong.Views.ScreenStart (view)
import AirConsolePong.Types (GameState)
import Data.Array (length)
import DOM (DOM)

foreign import requestAnimationFrame :: forall e . (Number -> Eff e Unit) -> Eff e Unit
foreign import showStuff :: forall a. a -> Eff (console :: CONSOLE) Unit

atLeastTwoPlayers :: AirConsoleGlobal -> Boolean
atLeastTwoPlayers ac =
    let apIds = getActivePlayerDeviceIds ac
        cdIds = getControllerDeviceIds ac
        apLength = length apIds
        cdLength = length cdIds
     in
        if apLength == 0 && cdLength >= 2
            then true
            else false

resetScore :: GameState -> GameState
resetScore gs = gs { score { p1 = 0, p2 = 0 } }

initGameState :: GameState
initGameState = { p1 , p2 , ball , score }
  where
    p1 = { pos: { x: 10.0, y: 50.0 }
         , move: { x: 0.0, y: 0.0 }
         }
    p2 = { pos: { x: 190.0, y: 53.0 }
         , move: { x: 0.0, y: 0.0 }
         }
    ball = { pos: { x: 100.0, y: 50.0 }
           , move: { x: 0.0, y: 0.0 }
           }
    score = { p1: 0, p2: 0 }

main :: Eff (dom :: DOM, console :: CONSOLE) Unit
main = onDOMContentLoaded do
    ac <- getAirConsoleGlobal { orientation: orientationLandscape }
    view ac
    _ <- onConnect (\d -> do
                        a <- if atLeastTwoPlayers ac
                                then do
                                    b <- setActivePlayers ac 2
                                    pure b
                                else pure unit
                        pure a
                   ) ac
    _ <- onMessage (\d x -> log "Message Received") ac
    _ <- onReady (\c -> log "Ready Bro") ac
    _ <- onDisconnect (\d -> log "On Disconnect") ac
    log "Screen IS NOT Ready"
