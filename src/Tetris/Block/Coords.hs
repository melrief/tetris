{-# LANGUAGE MultiParamTypeClasses #-}
module Tetris.Block.Coords where

import Tetris.Block.Dir (DirTo)


data Zero  = Zero 
data One   = One  
data Two   = Two  
data Three = Three

class CanMove t e where move :: t -> e
instance CanMove Zero One   where move Zero = One
instance CanMove One  Two   where move One  = Two
instance CanMove Two  Three where move Two  = Three

class CanMoveUp t e where moveUp :: t -> e
instance CanMoveUp One   Zero where moveUp One   = Zero
instance CanMoveUp Two   One  where moveUp Two   = One
instance CanMoveUp Three Two  where moveUp Three = Two


instance DirTo Right (x,y) (x,y') where
  type Ctx Right (x,y) (x,y') = CanMove y y'
  toF Right (x,y) = (x,move y)

instance DirTo Down (x,y) (x',y) where
  type Ctx Down (x,y) (x',y) = CanMove x x'
  toF Down (x,y) = (move x,y)

instance DirTo RightUp (x,y) (x',y') where
  type Ctx RightUp (x,y) (x',y') = (CanMoveUp x x',CanMove y y')
  toF RightUp (x,y) = (moveUp x,move y)
