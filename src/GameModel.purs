module AirConsolePong.GameModel where

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

initialGameState :: Game
initialGameState =
    { p1: { x: 10.0
          , y: 50.0
          , dy: 0.0
          , score: 0
          }
    , p2: { x: 190.0
          , y: 53.0
          , dy: 0.0
          , score: 0
          }
    , ball: { x: 100.0
            , y: 50.0
            , dx: 0.0
            , dy: 0.0
            }
    }
