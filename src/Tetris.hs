module Main where

import Control.Lens
import Control.Monad.State
import Data.List
import Data.Set (member)
import System.Environment (getArgs)
import Prelude hiding (Left,Right)

import Tetris.Board
import Tetris.Game
import Tetris.UI.NCurses


main :: IO ()
main = runGame

--main :: IO ()
--main = do
--  args <- getArgs
--  let numSteps = read (head args) :: Int
--
--  (finalStepRes,finalState) <- runStateT (dummyActions numSteps) (initGame 0)
--
--  case view currBlock finalState of
--    Nothing -> putStrLn "No current block"
--    Just b  -> do
--      let blockShape = view shape b
--      let blockPos = view pos b
--      let blockOrien = view orien b
--      putStrLn $  "Currently there is a block with shape " ++ show blockShape
--               ++ " in pos " ++ show blockPos
--               ++ " and dir " ++ show blockOrien
--
--
--  putStrLn $ "Step result: " ++ show finalStepRes
--
--
--  let boardState = view board finalState
--
--  putStrLn $ "Board:\n" ++ showBoard boardState
--
--actions :: (Functor m,Monad m) => [GameState m StepResult]
--actions = [foldl1 (>>) (replicate 5 $ tryToMoveCurrBlock Left >> step)
--          ,foldl1 (>>) (replicate 18 step)
--          
--          ,rotate Clockwise >> step
--          ,tryToMoveCurrBlock Left >> step
--          ,foldl1 (>>) (replicate 22 step)
--
--          ,foldl1 (>>) (replicate 3 $ tryToMoveCurrBlock Left >> step)
--          ,foldl1 (>>) (replicate 20 step)
--
--          ,foldl1 (>>) (replicate 3 $ tryToMoveCurrBlock Left >> step)
--          ,foldl1 (>>) (replicate 20 step)
--
--          ,foldl1 (>>) (replicate 23 step)
--
--          ,foldl1 (>>) (replicate 2 $ rotate Clockwise >> step)
--          ,tryToMoveCurrBlock Right >> step
--          ,foldl1 (>>) (replicate 16 step)
--
--          ,foldl1 (>>) (replicate 2 $ tryToMoveCurrBlock Right >> step)
--          ,foldl1 (>>) (replicate 16 step)
--
--          ,rotate Clockwise >> step
--          ,foldl1 (>>) (replicate 4 $ tryToMoveCurrBlock Right >> step)
--          ,foldl1 (>>) (replicate 22 step)
--
--          ,foldl1 (>>) (replicate 4 $ tryToMoveCurrBlock Right >> step)
--          ,foldl1 (>>) (replicate 16 step)
--          ]
--
--dummyActions :: (Functor m,Monad m) => Int -> GameState m StepResult
--dummyActions numSteps | numSteps <= 0 = undefined
--                      | otherwise = go actions numSteps
--  where go _      1 = step
--        go []     n = step >> go [] (n-1)
--        go (a:as) n = a    >> go as (n-1)
--
--showBoard :: Board -> String
--showBoard b = intercalate "\n" (map showRow allRows)
--  where showRow r = map (\c -> if (r,c) `member` b then '#' else '.') allColumns
