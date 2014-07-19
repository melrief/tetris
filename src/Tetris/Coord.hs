{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
module Tetris.Coord where

import Data.Bool
import Data.Eq
import Data.Function (($),(.))
import Data.Functor (fmap)
import Data.Int
import Data.List ((++))
import Data.Maybe
import Data.Ord
import GHC.Enum (Bounded,minBound,maxBound)
import GHC.Num
import GHC.Show


data Column = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
  deriving (Eq,Ord)

instance Bounded Column where
  minBound = Zero
  maxBound = Nine

allColumns :: [Column]
allColumns = [Zero,One,Two,Three,Four,Five,Six,Seven,Eight,Nine]

data Row = Digit Column | Ten Column | Twenty | TwentyOne
  deriving (Eq,Ord)

instance Bounded Row where
  minBound = Digit minBound
  maxBound = TwentyOne

allRows :: [Row]
allRows = Digit `fmap` allColumns ++ Ten `fmap` allColumns ++ [Twenty,TwentyOne]

-- | Safer Enum assuming i is a set smaller than Int
class IntSubset i where
  toInt :: i -> Int
  fromInt :: Int -> Maybe i

  fromTo :: i -> i -> [i]
  fromTo x y = catMaybes $ fmap fromInt [toInt x..toInt y]

  succ,pred :: i -> Maybe i
  succ = fromInt . (+ 1) . toInt
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
