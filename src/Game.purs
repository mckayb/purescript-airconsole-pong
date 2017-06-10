module AirConsolePong.Game where

import Prelude (negate, (+), (||), (>), (<), (*), (-))
import Math (min, max)

type Player =
    { x :: Number
    , y :: Number
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

type Bounds = { top :: Number
              , bottom :: Number
              , left :: Number
              , right :: Number
              }

bounds :: Bounds
bounds = { top: 99.0
         , bottom: 1.0
         , left: 1.0
         , right: 199.0
         }

initialPlayer1 :: Player
initialPlayer1 = { x: 10.0
                 , y: 50.0
                 , score: 0
                 }

initialPlayer2 :: Player
initialPlayer2 = { x: 190.0
                 , y: 50.0
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
movePlayer n p = p { y = max (min bounds.top (p.y + n)) bounds.bottom }
-- movePlayer n p = p { y = p.y + n }

moveBall :: Number -> Number -> Ball -> Ball
moveBall m n b = b { x = b.x + m, y = b.y + n, dx = m, dy = n }
    {- let
        calcY = b.y + n
        collidedWithTop = calcY < 55.0
        collidedWithBottom = calcY > 45.0
        dyMult = if collidedWithTop || collidedWithBottom then -1.0 else 1.0
     in
        { x: b.x + m
        , y: if collidedWithTop || collidedWithBottom then b.y - n else calcY
        , dx: b.dx
        , dy: b.dy * dyMult
    } -}

playerLogic :: Coordinate -> Player -> Player
playerLogic input = movePlayer input.y

ballLogic :: Player -> Player -> Coordinate -> Ball -> Ball
ballLogic p1 p2 input = moveBall input.x input.y

gameLogic :: Action -> Game -> Game
gameLogic { object: Player1, move: m } gs = gs { p1 = playerLogic m gs.p1
                                               -- , ball = ballLogic gs.p1 gs.p2 ({ x: gs.ball.dx, y: gs.ball.dy }) gs.ball
                                               }
gameLogic { object: Player2, move: m } gs = gs { p2 = playerLogic m gs.p2
                                               -- , ball = ballLogic gs.p1 gs.p2 ({ x: gs.ball.dx, y: gs.ball.dy }) gs.ball
                                               }
gameLogic { object: Ball, move: m } gs = gs { ball = ballLogic gs.p1 gs.p2 m gs.ball }
