module AirConsolePong.GameUpdate where

import AirConsolePong.GameModel (Player, Ball, Model)

movePlayer :: Number -> Player -> Player
movePlayer n p = p { y = p.y + n }

moveBall :: Number -> Number -> Ball -> Ball
moveBall m n b = b { x = b.x + m, y = b.y + n }

playerLogic :: { move :: Number } -> Player -> Player
playerLogic input = movePlayer input.move

ballLogic :: { x :: Number, y :: Number } -> Ball -> Ball
ballLogic input = moveBall input.x input.y
