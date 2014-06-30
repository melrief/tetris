{-# LANGUAGE TemplateHaskell #-}
module Tetris.UI.NCurses where

import           Control.Concurrent
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Char
import           Data.Set
import           UI.HSCurses.Curses (Key(..),ChType)
import qualified UI.HSCurses.Curses       as Curses
import qualified UI.HSCurses.CursesHelper as CursesH
import           Prelude hiding (Left,Right)
import           System.Random

import           Tetris.Board
import           Tetris.Game


data GuiState = GS {
    _score         :: Int

  -- current game speed
  , _speed         :: Int
  -- frame to skip before next update
  , _framesToIgnore :: Int
  }

$(makeLenses ''GuiState)

initGuiState :: GuiState
initGuiState = GS {
    _score          = 0
  , _speed          = 100
  , _framesToIgnore = 0
  }

-- two levels state, first for the gui and the second for the game state
type TetrisM m a = StateT GuiState (GameState m) a

stepInterval :: Int
stepInterval = 33000 -- ms, so ~30 fps

runGame :: (Functor m,MonadIO m) => m ()
runGame = liftIO (randomRIO (0,maxBound)) >>= runGameWithSeed

runGameWithSeed :: (Functor m,MonadIO m) => Int -> m ()
runGameWithSeed seed = do
  liftIO $ CursesH.start
  _ <- runStateT (runStateT gameLoop initGuiState) (initGame seed)
  liftIO $ CursesH.end


-- the result of a user input action
data ActionResult = MoveActionResult   MoveResult
                  | RotateActionResult RotateResult
                  | NoAction

class IsActionResult a where
  toActionResult :: a -> ActionResult

instance IsActionResult MoveResult where
  toActionResult = MoveActionResult

instance IsActionResult RotateResult where
  toActionResult = RotateActionResult

gameLoop :: (Functor m,MonadIO m) => TetrisM m ()
gameLoop = do

  -- mvar containing the last key pressed
  keyMVar <- liftIO newEmptyMVar

  -- thread getting the input
  _ <- liftIO $ forkIO $ handleInputThread keyMVar

  -- real gui cycle
  loopUntilEsc keyMVar $ \maybeChar -> do

    -- we force the gui to skip some time to avoid to keep the cpu busy
    liftIO $ threadDelay stepInterval

    -- process user input
    processInputResult <- processInputKey maybeChar
    case processInputResult of
      MoveActionResult (Merged (MergeResult rows)) -> updateSpeedAndScore rows
      _                                            -> return ()

    refreshGui

    -- check how many "frames" are remaining before running a step
    fti <- use framesToIgnore
    case fti `compare` 0 of
      LT -> undefined -- this should never happen
      GT -> assign framesToIgnore (fti - 1)
      EQ -> do stepResult <- lift step
               case stepResult of
                 Move (Merged (MergeResult rows)) -> updateSpeedAndScore rows
                 _                                -> return ()
               use speed >>= assign framesToIgnore

    lift (use gameOver)

  isGameOver <- lift (use gameOver)

  -- print game over and wait for user input before exiting
  when isGameOver $ void $ liftIO $ do
    Curses.mvWAddStr Curses.stdScr  9 0 "$$$$$$$$$$$$"
    Curses.mvWAddStr Curses.stdScr 10 0 "  Game Over "
    Curses.mvWAddStr Curses.stdScr 11 0 "$$$$$$$$$$$$"
    Curses.mvWAddStr Curses.stdScr 24 1 "Press any button to exit"
    Curses.getCh

updateSpeedAndScore :: (Monad m) => [Row] -> TetrisM m ()
updateSpeedAndScore rs = do
  let pointsToAdd = case length rs of
                      0 ->  0
                      1 ->  1
                      2 ->  3
                      3 ->  5
                      4 ->  9
                      _ -> undefined -- this should not happen
  when (pointsToAdd > 0) $ do
    modify $ over score (\x -> x + pointsToAdd)
           . over speed (\x -> max 1 (x - pointsToAdd))

escKey :: Key
escKey = KeyChar 'q'

handleInputThread :: MVar Key -> IO ()
handleInputThread mvar = do
  c <-  Curses.getCh
  putMVar mvar c
  unless (c == escKey) $ handleInputThread mvar

loopUntilEsc :: (MonadIO m) => MVar Key -> (Maybe Key -> TetrisM m Bool) -> TetrisM m ()
loopUntilEsc mvar action = do
  c <- liftIO $ tryTakeMVar mvar
  unless (c == Just escKey) $ do
    isOver <- action c
    unless isOver (loopUntilEsc mvar action)

processInputKey :: (Functor m,Monad m) => Maybe Key -> TetrisM m ActionResult
processInputKey (Just KeyLeft)       = toActionResult `fmap` lift (tryToMoveCurrBlock Left)
processInputKey (Just KeyRight)      = toActionResult `fmap` lift (tryToMoveCurrBlock Right)
processInputKey (Just KeyDown)       = toActionResult `fmap` lift (tryToMoveCurrBlock Down)
processInputKey (Just (KeyChar 'z')) = toActionResult `fmap` lift (tryToRotateCurrBlock Counterwise)
processInputKey (Just (KeyChar 'x')) = toActionResult `fmap` lift (tryToRotateCurrBlock Clockwise)
processInputKey Nothing              = return NoAction
-- we ignore other keys
processInputKey _                    = return NoAction

refreshGui :: (Functor m,MonadIO m) => TetrisM m ()
refreshGui = do

  liftIO Curses.erase

  boardCoords <- lift (use board)

  -- print all the board rows with borders
  forM allRows $ \row -> do
    
    let rowI = toInt row

    -- left border
    mvAddCh rowI 0 (fromIntegral $ ord '|')

    -- board content
    forM allColumns $ \col -> do
      let c = if (row,col) `member` boardCoords then '#' else '.'
      mvAddCh rowI (toInt col + 1) (fromIntegral $ ord c)

    -- right border
    mvAddCh rowI 11 (fromIntegral $ ord '|')

  -- print the floor
  forM_ [0..11] $ \col -> mvAddCh 22 col (fromIntegral $ ord '=')


  -- print the block
  maybeBlock <- lift (use currBlock)
  case maybeBlock of
    Nothing       -> return ()
    Just    block -> forM_ (blockCoords' block) $ \(row,col) -> 
                       mvAddCh (toInt row) (toInt col+1) (fromIntegral $ ord '+')

  -- print the speed
  currSpeed <- use speed
  liftIO $ Curses.mvWAddStr Curses.stdScr 18 13 $ "Speed: " ++ show currSpeed

  -- print the score
  currScore <- use score
  liftIO $ Curses.mvWAddStr Curses.stdScr 20 13 $ "Score: " ++ show currScore

  -- print the block infos
  liftIO $ Curses.mvWAddStr Curses.stdScr 22 13 $ show maybeBlock

  liftIO $ Curses.refresh

  where 
    mvAddCh :: (MonadIO m) => Int -> Int -> ChType -> m ()
    mvAddCh col row c = liftIO $ Curses.mvAddCh col row c
