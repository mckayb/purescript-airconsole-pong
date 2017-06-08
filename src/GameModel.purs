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
