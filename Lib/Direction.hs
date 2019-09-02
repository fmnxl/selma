module Lib.Direction where

import Prelude hiding (Either, Left, Right)
import Lib.Vector2

data Direction = Up | Down | Left | Right deriving (Show, Eq)

opposite :: Direction -> Direction
opposite Down = Up
opposite Up = Down
opposite Right = Left
opposite Left = Right

isOppositeOf :: Direction -> Direction -> Bool
d1 `isOppositeOf` d2 = d1 == opposite d2

directionVector :: Direction -> Vector2
directionVector dir = case dir of
  Up -> (0, -1)
  Down -> (0, 1)
  Left -> (-1, 0)
  Right -> (1, 0)
