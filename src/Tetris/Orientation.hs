{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Tetris.Orientation where

import Control.Monad
import Data.Eq
import Data.Maybe
import GHC.Show

import Tetris.Block.Dir
import Tetris.Coord


-- | Orientation on the board, used to move from one cell to another
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
--
-- please note that the rotations are reversed because the board is upside down,
-- ie row Digit Zero is the one at the top of the screen and Twentyone the one
-- at the bottom
data Rotation = Clockwise | Counterwise
  deriving (Eq,Show)

-- implementation of the rotation on the orientation (note that counterwise and
-- clockwise are reversed)
clockwise,counterwise :: Orientation -> Orientation
counterwise North = East
counterwise East  = South
counterwise South = West
counterwise West  = North
clockwise   North = West
clockwise   West  = South
clockwise   South = East
clockwise   East  = North

-- conversion from the declarative form to the function
rotationToFun :: Rotation -> Orientation -> Orientation
rotationToFun Clockwise   = clockwise
rotationToFun Counterwise = counterwise

-- implementation of the rotation on the head (note that counterwise and
-- clockwise are reversed)
clockwiseHeadShift,counterwiseHeadShift :: Orientation -> Coord -> Maybe Coord
clockwiseHeadShift North c = succColumn c >>= succColumn
clockwiseHeadShift East  c = succRow    c >>= succRow 
clockwiseHeadShift South c = predColumn c >>= predColumn
clockwiseHeadShift West  c = predRow    c >>= predRow
counterwiseHeadShift   North c = succRow    c >>= succRow
counterwiseHeadShift   East  c = predColumn c >>= predColumn
counterwiseHeadShift   South c = predRow    c >>= predRow
counterwiseHeadShift   West  c = succColumn c >>= succColumn


rotationToShift :: Rotation -> Orientation -> Coord -> Maybe Coord
rotationToShift Clockwise   = clockwiseHeadShift
rotationToShift Counterwise = counterwiseHeadShift
