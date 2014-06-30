{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
module Tetris.Block where

import           Data.Sequence

import           Tetris.Block.Dir
import           Tetris.Block.Shape


blocks :: Seq Shape
blocks = fromList [
    mkShape "block"   Down  RightUp Down
  , mkShape "stick"   Down  Down    Down
  , mkShape "rLBlock" Right Right   Down
  , mkShape "lBlock"  Down  Down    Right
  , mkShape "tBlock"  Down  Down    RightUp
  , mkShape "sBlock"  Down  Right   Down
  , mkShape "zBlock"  Right Down    Right
  --wrong    = mkShape RightUp Down Down
  ]
