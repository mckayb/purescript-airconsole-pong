module AirConsolePong.Game where

import Prelude (negate, (+), (||), (>=), (<=), (*), (-), (==), (&&))
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
         , bottom: 15.0
         , left: 1.0
         , right: 199.0
         }

initialPlayer1 :: Player
initialPlayer1 = { x: 10.0
                 , y: 55.0
                 , score: 0
                 }

initialPlayer2 :: Player
initialPlayer2 = { x: 190.0
                 , y: 55.0
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

playerLogic :: Coordinate -> Player -> Player
playerLogic input p = p { y = max (min bounds.top (p.y + input.y)) bounds.bottom }

ballLogic :: Coordinate -> Player -> Player -> Ball -> Ball
ballLogic c p1 p2 b =
    let
        -- Ceiling and Floor Collisions
        collidedWithTop = b.y >= bounds.top
        collidedWithBottom = b.y <= (bounds.bottom - 14.0)
        ySpeed = if b.dy == 0.0 then 1.0 else b.dy
        dy = if collidedWithTop || collidedWithBottom then -ySpeed else ySpeed
        y = b.y + (c.y * dy)

        -- Player Collisions
        withinPaddleX p = (b.x <= (p.x + 2.0)) && (b.x >= (p.x - 2.0))
        withinPaddleY p = (b.y >= (p.y - 14.0)) && (b.y <= p.y)
        withinPaddle p = withinPaddleX p && withinPaddleY p
        collidedWithPaddle = (withinPaddle p1) || (withinPaddle p2)
        xSpeed = if b.dx == 0.0 then 1.0 else b.dx
        dx = if collidedWithPaddle then -xSpeed else xSpeed
        x = b.x + (c.x * dx)

     in { x, y, dx, dy }

gameLogic :: Action -> Game -> Game
gameLogic { object: Player1, move: m } gs =
    let newBall = ballLogic { x: 1.0, y: 1.0 } gs.p1 gs.p2 gs.ball
        p1ScoreAdd = if newBall.x <= bounds.left then 1 else 0
        p2ScoreAdd = if newBall.x >= bounds.right then 1 else 0
        newP1 = playerLogic m (gs.p1 { score = gs.p1.score + p1ScoreAdd })

     in gs { p1 = newP1
           , p2 = gs.p2 { score = gs.p2.score + p2ScoreAdd }
           , ball = newBall
           }
gameLogic { object: Player2, move: m } gs =
    let newBall = ballLogic { x: 1.0, y: 1.0 } gs.p1 gs.p2 gs.ball
        p1ScoreAdd = if newBall.x <= bounds.left then 1 else 0
        p2ScoreAdd = if newBall.x >= bounds.right then 1 else 0
        newP2 = playerLogic m (gs.p2 { score = gs.p2.score + p2ScoreAdd })

     in gs { p1 = gs.p1 { score = gs.p1.score + p1ScoreAdd }
           , p2 = newP2
           , ball = newBall
           }
gameLogic { object: Ball, move: m } gs = gs { ball = ballLogic m gs.p1 gs.p2 gs.ball }
