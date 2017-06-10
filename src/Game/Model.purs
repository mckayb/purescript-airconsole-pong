module AirConsolePong.Game.Model where

-- Model
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

