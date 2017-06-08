module AirConsolePong.GameUpdate where

import Prelude ((+))
import AirConsolePong.GameModel (Player, Ball, Game)

type Input = { p1 :: { move :: Number }
             , p2 :: { move :: Number }
             , ball :: { x :: Number, y :: Number }
             }

movePlayer :: Number -> Player -> Player
movePlayer n p = p { y = p.y + n }

moveBall :: Number -> Number -> Ball -> Ball
moveBall m n b = b { x = b.x + m, y = b.y + n }

playerLogic :: { move :: Number } -> Player -> Player
playerLogic input = movePlayer input.move

ballLogic :: { x :: Number, y :: Number } -> Ball -> Ball
ballLogic input = moveBall input.x input.y

gameLogic :: Input -> Game -> Game
gameLogic inputs gs = gs { p1 = playerLogic inputs.p1 gs.p1
                         , p2 = playerLogic inputs.p2 gs.p2
                         , ball = ballLogic inputs.ball gs.ball
                         }

