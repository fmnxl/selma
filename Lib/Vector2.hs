module Lib.Vector2 where

type Vector2 = (Int, Int)

createAddVectorModulo :: Int -> Vector2 -> Vector2 -> Vector2
createAddVectorModulo modulo = addVector
  where
    (a, b) `addVector` (x, y) = ((a + x) `mod` modulo, (b + y) `mod` modulo)
