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
import Views.FFI (onDOMContentLoaded)
import Views.ScreenStart (view)
import DOM (DOM)

main :: Eff (dom :: DOM, console :: CONSOLE) Unit
main = onDOMContentLoaded do
    ac <- getAirConsoleGlobal { orientation: orientationLandscape }
    view ac
    _ <- onReady (\c -> log "Ready Bro") ac
    _ <- onMessage (\d x -> log "Message Received") ac
    _ <- onConnect (\d -> log "On Connection") ac
    _ <- onDisconnect (\d -> log "On Disconnect") ac
    log "Screen Ready"
