module AirConsolePong.Types where

-- import DOM.Node.Types (Element)

type Coord = { x :: Number, y :: Number }

type GameObj = { pos :: Coord
               , move :: Coord
               -- , el :: Element
               }

type Paddle = GameObj
type Ball = GameObj
type Score = { p1 :: Int, p2 :: Int }

type GameState = { p1 :: Paddle
                 , p2 :: Paddle
                 , ball :: Ball
                 , score :: Score
                 }
