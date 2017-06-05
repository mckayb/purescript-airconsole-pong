module AirConsolePong.Views.FFI where

import Prelude (Unit)
import Control.Monad.Eff (Eff)
import DOM (DOM)

foreign import renderToSel :: forall e. String -> String -> Eff (dom :: DOM | e) Unit
foreign import onDOMContentLoaded :: forall a e. Eff e a -> Eff e Unit
