module ScreenMain where

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

main :: Eff (console :: CONSOLE) Unit
main = do
    ac <- getAirConsoleGlobal { orientation: orientationLandscape }
    _ <- onReady (\c -> log "Ready Bro") ac
    _ <- onMessage (\d x -> log "Message Received") ac
    _ <- onConnect (\d -> log "On Connection") ac
    _ <- onDisconnect (\d -> log "On Disconnect") ac
    log "Screen Ready"
