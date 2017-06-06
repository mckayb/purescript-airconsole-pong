module AirConsolePong.Data.Coords where

newtype Coords = Coords { x :: Number, y :: Number }

coords :: Number -> Number -> Coords
coords x y = Coords { x, y }
