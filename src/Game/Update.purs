module AirConsolePong.Game.Update where

import Prelude ((+))
import AirConsolePong.Game.Model ( Player
                                 , Ball
                                 , Game
                                 , GameObject(Player1, Player2, Ball)
                                 , Action
                                 , Coordinate
                                 )

movePlayer :: Number -> Player -> Player
movePlayer n p = p { y = p.y + n }

moveBall :: Number -> Number -> Ball -> Ball
moveBall m n b = b { x = b.x + m, y = b.y + n }

playerLogic :: Coordinate -> Player -> Player
playerLogic input = movePlayer input.y

ballLogic :: Coordinate -> Ball -> Ball
ballLogic input = moveBall input.x input.y

gameLogic :: Action -> Game -> Game
gameLogic { object: Player1, move: m } gs = gs { p1 = playerLogic m gs.p1 }
gameLogic { object: Player2, move: m } gs = gs { p2 = playerLogic m gs.p2 }
gameLogic { object: Ball, move: m } gs = gs { ball = ballLogic m gs.ball }
