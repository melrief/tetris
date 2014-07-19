{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
module Tetris.Board where

import Control.Exception
import Control.Lens
import Control.Monad ((>>=))
import Data.Bool
import Data.Maybe
import Data.Monoid
import Data.Eq
import Data.Function (($),(.))
import Data.Functor (fmap)
import Data.Int
import qualified Data.List as List
import Data.Set
import Data.String
import GHC.Num
import GHC.Show
import Tetris.Block.Dir
import Prelude (undefined)

import Tetris.Coord


-- | The board is a Set that contains occupied blocks
type Board = Set Coord


-- Check if a row of the board is full, ie if all the columns are in the board
isFull :: Board -> Row -> Bool
isFull board row = List.all (\col -> (row,col) `member` board) allColumns

-- Erase rows by removing them from the board
eraseRows :: [Row] -> Board -> Board
eraseRows rows = (\\ coordsToRemove)
  where coordsToRemove = fromList $ List.concatMap (\c -> fmap (,c) rows) allColumns

-- the game is over when the highest row is not empty (ie we touched the ceil)
isGameOver :: Board -> Bool
isGameOver board = List.any (\c -> (Digit Zero,c) `member` board) allColumns

-- We start from the row with the highest "toInt" value (that is the lowest
-- row that change in the board) and the we replace each line when we encounter
-- it.
--
-- We then clean the first n rows where n is the number of rows to remove
collapseRows :: [Row] -> Board -> Board
collapseRows []        board = board
collapseRows uFullRows board = 
  let (i,board',_) = List.foldl' collapseRow (0,board,sFullRows) rowsToChange
  -- i - 1 because, e.g., if all rows must be removed then i == 22 but we need 21 (the maxBound)
  in case fromInt (i-1) of
       -- this error should never occur because i should always be in the
       -- rows range, if happens then raise error
       Nothing      -> throwError $ "The row to erase cannot have number " <> show (i-1)
       -- remove the first rows because they fell below
       Just lastRow -> eraseRows (fromTo (Digit Zero) lastRow) board'
  where 
    sFullRows = List.reverse $ List.sort $ uFullRows
    rowsToChange = List.reverse $ fromTo (Digit Zero) $ List.head sFullRows

    collapseRow :: (Int,Board,[Row]) -> Row -> (Int,Board,[Row])
    collapseRow (i,cBoard,frs) currRow =
      case uncons frs of
        -- when cr == h, the row should be eliminated:
        -- skip it and replace it later
        Just (h,t) -> if currRow == h then (i+1,cBoard,t)
                                    else collapseRow' i cBoard currRow `extendWith` frs
        Nothing    -> collapseRow' i cBoard currRow `extendWith` frs

    extendWith :: (a,b) -> c -> (a,b,c)
    (x,y) `extendWith` z = (x,y,z)

    -- row to keep but move => copy this row i rows below
    collapseRow' :: Int -> Board -> Row -> (Int,Board)
    collapseRow' i cBoard currRow = 
      let maybeRowToReplace = fromInt (i + toInt currRow)
      in case maybeRowToReplace of
           -- should not happen because we start from a row to replace,
           -- if happens then raise error
           Nothing           -> throwError $ "The row to replace cannot have number " <> show (i + toInt currRow)
           Just rowToReplace ->
             -- remove the row to replace and then shift the current row
             let cBoard'  = cBoard \\ makeFullRow rowToReplace
                 newRow = fromList $ fmap (rowToReplace,) $
                          List.foldl' (\l c -> if (currRow,c) `member` cBoard
                                               then c:l
                                               else l)
                                 [] allColumns
             in (i,cBoard' `union` newRow)

    makeFullRow :: Row -> Set Coord
    makeFullRow row = fromList $ fmap (row,) allColumns

    throwError :: String -> a
    throwError = throw . ErrorCall
