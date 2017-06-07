module AirConsolePong.Views.FFI where

import Prelude (Unit)
import Control.Monad.Eff (Eff)
import Graphics.Canvas ( CANVAS
                       , CanvasElement
                       )
import DOM (DOM)

foreign import renderToSel :: forall e. String -> String -> Eff (dom :: DOM | e) Unit
foreign import isNullOrUndefined :: forall a. a -> Boolean
foreign import updateCanvasDim :: forall e. CanvasElement -> Eff (canvas :: CANVAS | e) Unit
foreign import getClientHeight :: forall e. CanvasElement -> Eff (canvas :: CANVAS | e) Number
foreign import bitwiseOr :: Number -> Number
