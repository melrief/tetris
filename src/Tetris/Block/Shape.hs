{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Tetris.Block.Shape where

import Control.Lens

import Tetris.Block.Dir
import Tetris.Board


data Shape = Shape {
    -- shape name, used for readability
    _name          :: String
    -- the function that transforms the shape into coordinates given a starting point
  , _toCoords :: Orientation -> Coord -> Maybe [Coord]
  }

$(makeLenses ''Shape)

instance Show Shape where
  show s = _name s -- a Shape is represented by the name

mkShape :: (DirToFun d1 Orientation Coord
           ,DirToFun d2 Orientation Coord
           ,DirToFun d3 Orientation Coord
           ,CanBeFirst d1,CanGo d1 d2,CanGo d2 d3)
        => String -> d1 -> d2 -> d3 -> Shape
mkShape n d1 d2 d3 = Shape {
    _name = n
  , _toCoords = \ctx i0 -> do i1 <- toF d1 ctx i0
                              i2 <- toF d2 ctx i1
                              i3 <- toF d3 ctx i2
                              return [i0,i1,i2,i3]
  }
