module AirConsolePong.Data.GameState where

import AirConsolePong.Data.Coords (Coords)

type PlayerScore = Int

newtype GameState = GameState
    { p1Score :: PlayerScore
    , p2Score :: PlayerScore
    , p1Pos :: Coords
    , p2Pos :: Coords
    , ballPos :: Coords
    , p1Move :: Coords
    , p2Move :: Coords
    , ballMove :: Coords
    }

initialGameState :: GameState
initialGameState = GameState
    { p1Score: 0
    , p2Score: 0
    , p1Pos: Coords { x: 10.0, y: 50.0 }
    , p2Pos: Coords { x: 190.0, y: 53.0 }
    , ballPos: Coords { x: 100.0, y: 50.0 }
    , p1Move: Coords { x: 0.0, y: 0.0 }
    , p2Move: Coords { x: 0.0, y: 0.0 }
    , ballMove: Coords { x: 0.0, y: 0.0 }
    }
