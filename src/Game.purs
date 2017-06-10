module AirConsolePong.Game where

import Prelude ((+))
import Math (min, max)

type Player =
    { x :: Number
    , y :: Number
    , dy :: Number
    , score :: Int
    }

type Ball =
    { x :: Number
    , y :: Number
    , dx :: Number
    , dy :: Number
    }

type Game =
    { p1 :: Player
    , p2 :: Player
    , ball :: Ball
    }

type Coordinate = { x :: Number, y :: Number }

data GameObject = Player1 | Player2 | Ball
type Action = { object :: GameObject
              , move :: Coordinate
              }

initialPlayer1 :: Player
initialPlayer1 = { x: 10.0
                 , y: 50.0
                 , dy: 0.0
                 , score: 0
                 }

initialPlayer2 :: Player
initialPlayer2 = { x: 190.0
                 , y: 50.0
                 , dy: 0.0
                 , score: 0
                 }

initialBall :: Ball
initialBall = { x: 100.0
              , y: 50.0
              , dx: 0.0
              , dy: 0.0
              }

initialGameState :: Game
initialGameState =
    { p1: initialPlayer1
    , p2: initialPlayer2
    , ball: initialBall
    }

movePlayer :: Number -> Player -> Player
movePlayer n p = p { y = min (max (p.y + n) 0.0) 86.0  }

moveBall :: Number -> Number -> Ball -> Ball
moveBall m n b = b { x = b.x + m, y = b.y + n }

playerLogic :: Coordinate -> Player -> Player
playerLogic input = movePlayer input.y

ballLogic :: Player -> Player -> Coordinate -> Ball -> Ball
ballLogic p1 p2 input = moveBall input.x input.y

gameLogic :: Action -> Game -> Game
gameLogic { object: Player1, move: m } gs = gs { p1 = playerLogic m gs.p1 }
gameLogic { object: Player2, move: m } gs = gs { p2 = playerLogic m gs.p2 }
gameLogic { object: Ball, move: m } gs = gs { ball = ballLogic gs.p1 gs.p2 m gs.ball }
