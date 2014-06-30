{-# LANGUAGE TemplateHaskell #-}
module Tetris.Game where

import           Control.Lens hiding (index)
import           Control.Monad.State
import qualified Data.List   as List
import           Data.Stream (Stream(..))
import qualified Data.Set    as Set
import           Data.Set (member,union)
import qualified Data.Sequence as Seq
import           Data.Sequence (index)
import           System.Random

import           Tetris.Block
import           Tetris.Block.Shape
import           Tetris.Board

import           Prelude hiding (Left,Right)


-- initial coordinates of the "head" of a block
initBlockCoord :: Coord
initBlockCoord = (Digit Zero,Four)

-- initial orientation of the "head" of a block
initBlockDir :: Orientation
initBlockDir = South

-- the current falling block
data CurrentBlock = CB {
    _pos   :: Coord        -- position of the block "head"
  , _orien :: Orientation  -- orientation of the block "head"
  , _shape :: Shape        -- the block shape
  }

$(makeLenses ''CurrentBlock)

instance Show CurrentBlock where
  show cb = show (view pos cb) ++ " " ++ show (view orien cb) ++ " " ++ show (view shape cb)

initPosAndDir :: Shape -> CurrentBlock
initPosAndDir initShape = CB {
    _pos   = initBlockCoord
  , _orien = initBlockDir
  , _shape = initShape
  }

-- convert the current block into a list of coordinates if possible (if the
-- resulting coords are empty)
blockCoords :: CurrentBlock -> Maybe [Coord]
blockCoords cb = (view (shape.toCoords) cb) (view orien cb) (view pos cb)

-- same as blockCoords but return an empty list if the resulting coords are
-- not empty
blockCoords' :: CurrentBlock -> [Coord]
blockCoords' cb = case blockCoords cb of
                    Nothing     -> []
                    Just coords -> coords

-- the current state of the game
data Game = Game {
    _board       :: Board              -- state of the board
  , _currBlock   :: Maybe CurrentBlock -- the current falling block, if any

  , _shapesQueue :: Stream Shape       -- queue of shapes to use as falling blocks
  , _gameOver    :: Bool               -- is the game over? (cache for isGameOver board)
  }

$(makeLenses ''Game)

-- create a "new" game with an empty board and no blocks
initGame :: Int -> Game
initGame seed = Game {
    _board       = Set.empty
  , _currBlock   = Nothing

  , _shapesQueue = mkShapesQueue (mkStdGen seed)
  , _gameOver    = False
  }
  where mkShapesQueue :: RandomGen r => r -> Stream Shape
        mkShapesQueue gen = let (idx,gen') = randomR (0,Seq.length blocks - 1) gen
                            in Cons (blocks `index` idx) (mkShapesQueue gen')


type GameState m = StateT Game m

nextRandomShape :: (Monad m) => GameState m Shape
nextRandomShape = do
  -- maybe could use snd and (<<%=) to do it in one step?
  Cons nextShape shapesQueueTail <- use shapesQueue
  assign shapesQueue shapesQueueTail
  return nextShape

data RotateResult = Rotated Rotation | CannotRotate | BlockToRotateNotFound

-- Rotate the current block clockwise or counterwise.
--
-- Rotation requires caution: the block can be rotated only if it doesn't exit
-- the board and if the resultant coordinates are not occupied in the board
tryToRotateCurrBlock :: (Monad m) => Rotation -> GameState m RotateResult
tryToRotateCurrBlock rotation = do
  curr <- use currBlock
  case curr of
    Nothing -> return BlockToRotateNotFound -- should be BlockNotFound
    Just b  -> do
      let newOrien = rotationToFun rotation $ view orien b
      let maybeNewPos = rotationToShift rotation newOrien $ view pos b
      case maybeNewPos of
        Nothing     -> return CannotRotate
        Just newPos -> do
          let b' = set orien newOrien $ set pos newPos $ b
          case blockCoords b' of
            -- Cannot rotate because by rotating the block exits the board
            Nothing     -> return CannotRotate
            Just coords -> do
              boardCoords <- use board
              if any (`member` boardCoords) coords
                -- cannot rotate because by rotating the block goes on one or
                -- more not empty coordinates in the board
                then return CannotRotate
                else do
                  assign currBlock (Just b')
                  return (Rotated rotation)

-- Declarative way to define the direction possibilities when moving a block.
-- Each direction can then be converted to functions to be used when really
-- moving a block.
data MoveDirection = Left | Right | Down
  deriving Show

-- when a block cannot be moved, should it be merged to the board?
shouldMerge :: MoveDirection -> Bool
shouldMerge Down = True
shouldMerge _    = False

moveDirToFun :: MoveDirection -> (Coord -> Maybe Coord)
moveDirToFun Left  = predColumn
moveDirToFun Right = succColumn
moveDirToFun Down  = succRow

data MoveResult = Moved         MoveDirection
                | CannotMove    MoveDirection
                | Merged        MergeResult
                | BlockNotFound
  deriving Show

-- Try to move the current block (if there is a current block). Note that this
-- function can fail to move the block and can eventually merge the block in
-- the board
tryToMoveCurrBlock :: (Monad m) => MoveDirection -> GameState m MoveResult
tryToMoveCurrBlock md = do
  curr <- use currBlock
  case curr of
    Nothing -> return BlockNotFound
    Just cb -> do
      let shapeCB = view (shape.toCoords) cb
      let orienCB = view orien            cb
      let posCB   = view pos              cb

      case moveDirToFun md posCB of
        -- bottom row, cannot go further
        Nothing     -> mergeOrCannotMove cb
        Just posCB' -> do
          let maybeNewBlockCoords = shapeCB orienCB posCB'
          case maybeNewBlockCoords of
            -- part of the block coords are outside the board
            Nothing             -> mergeOrCannotMove cb
            Just newBlockCoords -> do
              occupiedCoords <- use board
              if any (`member` occupiedCoords) newBlockCoords
                -- touched an occupied coord of the board
                then mergeOrCannotMove cb
                else assign (currBlock.traverse.pos) posCB' >> return (Moved md)

  where
    mergeOrCannotMove b = if shouldMerge md
                            then do
                              mergeResult <- mergeBlock b
                              assign currBlock Nothing
                              return (Merged mergeResult)
                            else return (CannotMove md)

-- result of a merge can be a set of rows full and consequently removed or
-- gameover
data MergeResult = MergeResult [Row] | MergeGameOver
  deriving Show

-- Merge the current block and then remove full rows if any
mergeBlock :: (Monad m) => CurrentBlock -> GameState m MergeResult
mergeBlock cb = do
  -- merge the block in the current board
  let coords = blockCoords' cb
  boardCoords <- use board
  let boardCoords' = boardCoords `union` Set.fromList coords

  -- check the "game over", ie if the highest row (Digit Zero) is full
  if isGameOver boardCoords'
    then do
      assign board boardCoords'
      assign gameOver True -- we "cache" the gameover to avoid testing it every times
      return MergeGameOver -- this is redundant with the gameOver field
    else do
      -- find "full rows" to remove and remove them by moving rows above
      let fullRows = List.filter (isFull boardCoords') (map fst coords)
      let boardCoords'' = collapseRows fullRows boardCoords'
      assign board boardCoords''

      return (MergeResult fullRows)


data StepResult = Move MoveResult | NewShape Shape | GameOver

instance Show StepResult where
  show GameOver     = "GameOver"
  show (NewShape s) = "NewShape(" ++ show s ++ ")"
  show (Move    mr) = show mr

-- advance the state of the game
step :: (Functor m,Monad m) => GameState m StepResult
step = do
  isOver <- use gameOver
  if isOver
    -- avoid further operations if game over and return directly GameOver
    then return GameOver
    else do
      curr <- use currBlock
      case curr of
        Nothing -> do
          nextShape <- nextRandomShape
          assign currBlock (Just $ initPosAndDir nextShape)
          return $ NewShape nextShape
        Just _ -> Move `fmap` tryToMoveCurrBlock Down
