module ScreenMain where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import AirConsole.Global ( getAirConsoleGlobal
                         , orientationLandscape
                         , onMessage
                         , onReady
                         )

main :: Eff (console :: CONSOLE) Unit
main = do
    ac <- getAirConsoleGlobal { orientation: orientationLandscape }
    _ <- onReady (\c -> log "Ready Bro") ac
    _ <- onMessage (\d x -> log "MESSAGE RECEIVED") ac
    log "Screen Ready"
