{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
module Tetris.Board where

import Control.Lens
import Control.Monad ((>>=))
import Debug.Trace
import Data.Bool
import Data.Eq
import Data.Function ((.),($))
import Data.Functor (fmap)
import Data.Int
import Data.List ((++))
import qualified Data.List as List
import Data.Maybe
import Data.Ord (Ord,(>=),(<=))
import Data.Set
import GHC.Num
import GHC.Show
import Tetris.Block.Dir
import Prelude (undefined)


data Column = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
  deriving (Eq,Ord)

allColumns :: [Column]
allColumns = [Zero,One,Two,Three,Four,Five,Six,Seven,Eight,Nine]

data Row = Digit Column | Ten Column | Twenty | TwentyOne
  deriving (Eq,Ord)

allRows :: [Row]
allRows = Digit `fmap` allColumns ++ Ten `fmap` allColumns ++ [Twenty,TwentyOne]

class IntSubset i where
  toInt :: i -> Int
  fromInt :: Int -> Maybe i

  fromTo :: i -> i -> [i]
  fromTo x y = catMaybes $ fmap fromInt [toInt x..toInt y]

  succ,pred :: i -> Maybe i
  succ = fromInt . (\x -> x + 1) . toInt
  pred = fromInt . (\x -> x - 1) . toInt

instance IntSubset Column where
  toInt Zero  = 0
  toInt One   = 1
  toInt Two   = 2
  toInt Three = 3
  toInt Four  = 4
  toInt Five  = 5
  toInt Six   = 6
  toInt Seven = 7
  toInt Eight = 8
  toInt Nine  = 9

  fromInt 0 = Just Zero
  fromInt 1 = Just One
  fromInt 2 = Just Two
  fromInt 3 = Just Three
  fromInt 4 = Just Four
  fromInt 5 = Just Five
  fromInt 6 = Just Six
  fromInt 7 = Just Seven
  fromInt 8 = Just Eight
  fromInt 9 = Just Nine
  fromInt _ = Nothing

instance IntSubset Row where
  toInt (Digit c) = toInt c
  toInt (Ten   c) = 10 + toInt c
  toInt Twenty    = 20
  toInt TwentyOne = 21

  fromInt i | i >=  0 && i <=  9 = Digit `fmap` (fromInt i     ::Maybe Column)
            | i >= 10 && i <= 19 = Ten   `fmap` (fromInt (i-10)::Maybe Column)
            | i == 20           = Just Twenty
            | i == 21           = Just TwentyOne
            | otherwise        = Nothing


instance Show Column where show = show . toInt
instance Show Row    where show = show . toInt


type Coord = (Row,Column)

succRow,predRow,succColumn,predColumn :: Coord -> Maybe Coord
succRow    (r,c) = (,c) `fmap` succ r
predRow    (r,c) = (,c) `fmap` pred r
succColumn (r,c) = (r,) `fmap` succ c
predColumn (r,c) = (r,) `fmap` pred c

--class DirTo d from to where
--  type Ctx d from to :: Constraint
--  toF :: Ctx d from to => d -> from -> to

data Orientation = North | South | East | West
  deriving (Eq,Show)

instance DirToFun Right Orientation Coord where
  toF Right North = predColumn
  toF Right South = succColumn
  toF Right East  = predRow
  toF Right West  = succRow

instance DirToFun Down Orientation Coord where
  toF Down North = predRow
  toF Down South = succRow
  toF Down East  = succColumn
  toF Down West  = predColumn

instance DirToFun RightUp Orientation Coord where
  toF RightUp North c = predColumn c >>= succRow
  toF RightUp South c = succColumn c >>= predRow
  toF RightUp East  c = predRow c >>= predColumn
  toF RightUp West  c = succRow c >>= succColumn


-- only two ways to rotate a block, clockwise and counterwise
data Rotation = Clockwise | Counterwise
  deriving (Eq,Show)

-- implementation of the rotation
clockwise,counterwise :: Orientation -> Orientation
clockwise   North = East
clockwise   East  = South
clockwise   South = West
clockwise   West  = North
counterwise North = West
counterwise West  = South
counterwise South = East
counterwise East  = North

-- conversion from the declarative form to the function
rotationToFun :: Rotation -> Orientation -> Orientation
rotationToFun Clockwise   = clockwise
rotationToFun Counterwise = counterwise


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
  in case fromInt i of
       -- this error should never occur because i should always be in the
       -- rows range, if happens then raise error
       Nothing      -> undefined
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
           Nothing           -> undefined
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
