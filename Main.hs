{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module Main where

import Prelude hiding (Either, Left, Right)
import Lens.Micro ((^.), (&), (.~), (%~))
import Lens.Micro.TH (makeLenses)
import Control.Monad (void, forever)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import qualified Graphics.Vty as V

import Brick.BChan
import Brick.Util (on, bg)
import Data.Text.Markup ((@@))
import Brick
  ( hLimit
  , padTop
  , padBottom
  , Padding(Pad)
  )
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
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Core
  ( (<=>)
  , str
  , vBox
  , hBox
  , withBorderStyle
  )
import System.Random
import Lib.Vector2
import Lib.Direction

-- Configs
gameName = "Selma"
width = 30 -- Screen width
initialSnakeLength = 5

addVector = createAddVectorModulo width

data CustomEvent = Tick deriving Show

data Snake =
  Snake { _body :: [Vector2]
        , _direction :: Direction
        } deriving Show

moveSnake :: Bool -> Direction -> [Vector2] -> [Vector2]
moveSnake isEating direction (currHead:currTail) = nextHead:nextTail
  where
    nextHead = (directionVector direction) `addVector` currHead
    nextTail | isEating = currHead:currTail 
             | otherwise = currHead:(init currTail)

isSnakeDead :: Snake -> Bool
isSnakeDead (Snake { _body = body, _direction = direction }) = snakeHead `elem` snakeTail
  where
    (snakeHead:snakeTail) = body

data State =
  State { _stSnake :: Snake
        , _stNextDirection :: Direction
        , _stFoods :: [Vector2]
        , _stGameOverFlag :: Bool
        } deriving Show

makeLenses ''State
makeLenses ''Snake

-- Infinite list of random ints
randomInts :: IO [Int]
randomInts = do
  g <- newStdGen
  return $ randomRs (0, width - 2) g

drawUI :: State -> [Widget ()]
drawUI st = [ui]
  where
    ui = withBorderStyle unicode $
          borderWithLabel (str gameName) $
            if st^.stGameOverFlag then drawGameOver else drawGame st

drawGameOver :: Widget ()
drawGameOver =
  hLimit (width * 2)
  $ padTop (Pad (width `div` 2 - 1))
  $ padBottom (Pad (width `div` 2 - 1))
  $ vBox [
    hCenter (str "Game over"),
    hCenter (str "Press SPACE to start a new game")
  ]

drawGame :: State -> Widget ()
drawGame st =
  vBox [
    hBox [
      drawGameCell (x, y) st
      | x <- [0..width-1]] 
    | y <- [0..width-1]
  ]

drawGameCell :: Vector2 -> State -> Widget ()
drawGameCell coords st
  | coords `elem` st^.stSnake^.body = cell V.blue
  | coords `elem` st^.stFoods = cell V.red
  | otherwise = cell V.black
  where cell color = markup ("  " @@ bg color)

keyToDirection key = case key of
  V.KUp -> Just Up
  V.KDown -> Just Down
  V.KLeft -> Just Left
  V.KRight -> Just Right
  _  -> Nothing

updateGame :: State -> EventM () (Next State)
updateGame st = do
  stFoodsSetter <- liftIO $ createFoodsSetter
  continue $ st
    & stFoodsSetter
    & stSnake %~ body %~ moveSnake isEatingFood (st^.stNextDirection)
    & stSnake %~ direction .~ st^.stNextDirection
    & \_st -> _st & stGameOverFlag .~ isSnakeDead (_st^.stSnake)
  where
    (currSnakeHead:_) = st^.stSnake^.body
    snakeLookahead = (directionVector (st^.stNextDirection)) `addVector` currSnakeHead
    isEatingFood = snakeLookahead `elem` (st^.stFoods)
    createFoodsSetter
      | isEatingFood = do
        coords <- generateRandomFreeCell st
        return $ stFoods .~ [coords]
      | otherwise = return id

changeDirectionConditionally :: Direction -> State -> (State -> State)
changeDirectionConditionally nextDirection st
  | (st^.stSnake^.direction) `isOppositeOf` nextDirection = id
  | otherwise = stNextDirection .~ nextDirection

handleKeyEvent :: V.Key -> State -> EventM () (Next State)
handleKeyEvent key st
  | key == V.KEsc = quitGame
  | key == (V.KChar ' ') = restartGame
  | Just direction <- keyToDirection key = continue $ st
    & changeDirectionConditionally direction st
  | otherwise = continue st
  where
    quitGame = halt st
    restartGame = do
      newInitialState <- liftIO generateInitialState
      continue $ newInitialState

appEvent :: State -> BrickEvent () CustomEvent -> EventM () (Next State)
appEvent st e =
  case e of
    VtyEvent (V.EvKey key []) -> handleKeyEvent key st
    AppEvent Tick | st^.stGameOverFlag -> continue st
                  | otherwise -> updateGame st
    _ -> continue st

generateRandomVector :: IO Vector2
generateRandomVector = do
  [x,y] <- randomInts >>= \x -> return $ take 2 $ x
  return (x,y)

generateRandomFreeCell :: State -> IO Vector2
generateRandomFreeCell st = do
  let occupiedCells = (st^.stSnake^.body) ++ (st^.stFoods)
  generateRandomFreeCellExcept occupiedCells

generateRandomFreeCellExcept :: [Vector2] -> IO Vector2
generateRandomFreeCellExcept cells = do
  candidate <- generateRandomVector
  verifyCandidate cells candidate
  where
    verifyCandidate cells x
      | x `elem` cells = generateRandomFreeCellExcept cells
      | otherwise = return x

generateInitialState :: IO State
generateInitialState = do
  randomPos <- generateRandomFreeCellExcept initialSnakeBody
  return $ State { _stSnake = initialSnake
                 , _stNextDirection = initialSnake^.direction
                 , _stFoods = [randomPos]
                 , _stGameOverFlag = False
                 }
  where
    initialSnakeBody = reverse [(x,0) | x <- [0..initialSnakeLength]]
    initialSnake =
      Snake { _body = initialSnakeBody
            , _direction = Right
            }

app :: App State CustomEvent ()
app =
  App { appDraw = drawUI
      , appChooseCursor = showFirstCursor
      , appHandleEvent = appEvent
      , appStartEvent = return
      , appAttrMap = const $ attrMap V.defAttr []
      }

main :: IO ()
main = do
  chan <- newBChan 10
  x <- return (Just 10)

  forkIO $ forever $ do
    threadDelay 200000
    writeBChan chan Tick

  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  initialState <- generateInitialState
  void $ customMain initialVty buildVty (Just chan) app initialState
