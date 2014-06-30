{-# LANGUAGE MultiParamTypeClasses #-}
module Tetris.Block.Dir where

import           GHC.Prim (Constraint)


data Right   = Right
data Down    = Down
data RightUp = RightUp


class    Dir d
instance Dir Right
instance Dir Down
instance Dir RightUp

class    (Dir d) => CanBeFirst d
instance           CanBeFirst Right
instance           CanBeFirst Down

class    CanGo from    to
instance CanGo Right   Right
instance CanGo Right   Down
instance CanGo Down    Right
instance CanGo Down    Down
instance CanGo Down    RightUp
instance CanGo RightUp Right
instance CanGo RightUp Down


class DirToFun dir ctx x where
  toF :: dir -> ctx -> x -> Maybe x
