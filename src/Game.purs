module AirConsolePong.Game where

import Prelude (Unit)
import Control.Monad.State (State)
import AirConsolePong.Data.Coords (coords)
import AirConsolePong.Data.GameState (GameState)
import AirConsolePong.Data.GameObject (GameObject(P1Paddle, P2Paddle, Ball))

type Game = State GameState Unit

updatePos :: Number -> Number -> GameObject -> Game Unit
setPos _ dy P1Paddle = modify (\(GameState state) -> GameState (state { p1Pos = coords (state.p1Pos.x) (state.p1Pos.y + dy) } ))
setPos _ dy P2Paddle = modify (\(GameState state) -> GameState (state { p2Pos = coords (state.p2Pos.x) (state.p2Pos.y + dy) } ))
setPos dx dy Ball = modify (\(GameState state) -> GameState (state { ballPos = coords (state.ballPos.x + dx) (state.ballPos.y + dy) } ))

updateMovement :: Number -> Number -> GameObject -> Game Unit
setMovement _ dy P1Paddle = modify (\(GameState state) -> GameState (state { p1Move = coords (state.p1Move.x) (state.p1Move.y + dy) } ))
setMovement _ dy P2Paddle = modify (\(GameState state) -> GameState (state { p2Move = coords (state.p2Move.x) (state.p2Move.y + dy) } ))
setMovement dx dy Ball = modify (\(GameState state) -> GameState (state { ballMove = coords (state.ballMove.x + dx) (state.ballMove.y + dy) } ))

updateScore :: Number -> GameObject -> Game Unit
updateScore s P1Paddle = modify (\(GameState state) -> GameState (state { p1Score = s }))
updateScore s P2Paddle = modify (\(GameState state) -> GameState (state { p2Score = s }))
updateScore s _ = tell ["Failed to update the score."]
