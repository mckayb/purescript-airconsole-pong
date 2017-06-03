module AirConsolePong.Views.FFI where

import Prelude (Unit)
import Control.Monad.Eff (Eff)

foreign import renderToSel :: forall e. String -> String -> Eff e Unit
foreign import onDOMContentLoaded :: forall a e. Eff e a -> Eff e Unit
