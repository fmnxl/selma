{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module Main where

import Lens.Micro ((^.), (&), (.~), (%~))
import Lens.Micro.TH (makeLenses)
import Control.Monad (void, forever)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import qualified Graphics.Vty as V

import Brick.BChan
import Brick.Util (on, fg, bg)
import Data.Text.Markup ((@@))
import Brick.Main
  ( App(..)
  , showFirstCursor
  , customMain
  , continue
  , halt
  )
import Brick.AttrMap
  ( attrMap
  )
import Brick.Types
  ( Widget
  , Next
  , EventM
  , BrickEvent(..)
  )
import Brick.Markup (markup, (@?))
import Brick.Widgets.Border (border, borderWithLabel)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Core
  ( (<=>)
  , str
  , vBox
  , hBox
  , withAttr
  , withBorderStyle
  )
import System.Random
width = 30

g2 :: IO [Int]
g2 = do
  g <- newStdGen
  return $ randomRs (0, width - 2) g

data CustomEvent = Counter deriving Show

data State =
    State { _stLastBrickEvent :: Maybe (BrickEvent () CustomEvent)
       , _stCounter :: Int
       , _snake :: [(Int, Int)]
       , _stVelocity :: (Int, Int)
       , _stNextVelocity :: (Int, Int)
       , _stFoods :: [(Int, Int)]
       }

makeLenses ''State

cell color = markup ("  " @@ bg color)

initialSnakeLength = 5
initialSnake = reverse [(x,0) | x <- [0..initialSnakeLength]]

renderCell coords st
    | coords `elem` st^.snake = cell V.blue
    | coords `elem` st^.stFoods = cell V.red
    | otherwise = cell V.black

-- This is like virtual dom
drawUI :: State -> [Widget ()]
drawUI st = [grid]
    where
        grid =
          withBorderStyle unicode $
            borderWithLabel (str "Snake") $
              vBox [
                hBox [
                  renderCell (x, y) st
                  | x <- [0..width-1]] 
                | y <- [0..width-1]
              ]
        lastEvent = case st^.stLastBrickEvent of
          Just e -> show e
          _ -> ""

move (dx,dy) (x,y) = ((x+dx) `mod` width, (y+dy) `mod` width)

moveSnake grow newHead (oldHead:oldTail) = newHead:oldHead:(newTail)
          where
            newTail = if grow then oldTail else init oldTail

opposite_direction (x,y ) = (-x, -y)


potentiallyTurn currDirection direction
  | currDirection == opposite_direction(direction) = id
  | otherwise = stNextVelocity .~ direction

isFoodEaten foods snakeHead = snakeHead `elem` foods

-- Monad transformers
loop st = do
  newFoodPos <- liftIO potentiallyRegenerateFood
  continue $ st & stCounter %~ (+1)
                & snake .~ newSnake
                & stFoods .~ newFoodPos
                & stVelocity .~ st^.stNextVelocity
    where
        (oldSnakeHead:_) = st^.snake
        newSnakeHead = move (st^.stNextVelocity) oldSnakeHead
        eaten = isFoodEaten (st^.stFoods) newSnakeHead
        newSnake = moveSnake eaten newSnakeHead (st^.snake)
        potentiallyRegenerateFood
          | isFoodEaten (st^.stFoods) newSnakeHead = do
            coords <- generateRandomFreeCoords st
            return [coords]
          | otherwise = return (st^.stFoods)

appEvent :: State -> BrickEvent () CustomEvent -> EventM () (Next State)
appEvent st e =
    case e of
        VtyEvent (V.EvKey V.KEsc []) -> halt st
        VtyEvent (V.EvKey V.KUp []) -> continue $ st & potentiallyTurn (st^.stVelocity) (0, -1)
        VtyEvent (V.EvKey V.KDown []) -> continue $ st & potentiallyTurn (st^.stVelocity) (0, 1)
        VtyEvent (V.EvKey V.KLeft []) -> continue $ st & potentiallyTurn (st^.stVelocity) (-1, 0)
        VtyEvent (V.EvKey V.KRight []) -> continue $ st & potentiallyTurn (st^.stVelocity) (1, 0)
        VtyEvent _ -> continue $ st & stLastBrickEvent .~ (Just e)
        AppEvent Counter -> loop st
        _ -> continue st

generateRandomCoords = do
    [x,y] <- g2 >>= \x -> return $ take 2 $ x
    return (x,y)

generateRandomFreeCoords st = do
  let allItems = (st^.snake) ++ (st^.stFoods)
  generateAndCheck allItems
  where
    generateAndCheck xs = do
      candidate <- generateRandomCoords
      verifyCandidate xs candidate
    verifyCandidate xs x
      | x `elem` xs = generateAndCheck xs
      | otherwise = return x
      

initialState :: (Int, Int) -> State
initialState randomPos =
    State { _stLastBrickEvent = Nothing
       , _stCounter = 0
       , _snake = initialSnake
       , _stVelocity = (1, 0)
       , _stNextVelocity = (1, 0)
       , _stFoods = [randomPos]
       }

theApp :: App State CustomEvent ()
theApp =
    App { appDraw = drawUI
        , appChooseCursor = showFirstCursor -- what this?
        , appHandleEvent = appEvent
        , appStartEvent = return
        , appAttrMap = const $ attrMap V.defAttr []
        }

main :: IO ()
main = do
    chan <- newBChan 10
    x <- return (Just 10)

    forkIO $ forever $ do
        threadDelay 200100
        writeBChan chan Counter

    let buildVty = V.mkVty V.defaultConfig
    initialVty <- buildVty
    randomPos <- generateRandomCoords
    void $ customMain initialVty buildVty (Just chan) theApp (initialState randomPos)
