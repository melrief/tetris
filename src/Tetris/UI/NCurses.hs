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

import           Tetris.Coord
import           Tetris.Orientation
import           Tetris.Game


data GuiState = GS {
  -- the player score
    _score         :: Int

  -- current game speed (related to the score)
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

-- the interval between two game steps
stepInterval :: Int
stepInterval = 16666 -- ms, so ~60 fps

-- run the game using a random seed
runGame :: (Functor m,MonadIO m) => m ()
runGame = liftIO (randomRIO (0,maxBound)) >>= runGameWithSeed

-- run the game using a given seed
runGameWithSeed :: (Functor m,MonadIO m) => Int -> m ()
runGameWithSeed seed = do
  liftIO CursesH.start
  _ <- runStateT (runStateT gameLoop initGuiState) (initGame seed)
  liftIO CursesH.end


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

-- utility function to map toActionResult to a gamestate result
toActionResult' :: (Functor m,Monad m,IsActionResult r) => GameState m r -> TetrisM m ActionResult
toActionResult' = fmap toActionResult . lift

-- the game loop is the main loop where the game progresses and the user input
-- is managed.
--
-- It roughly consists on creating a thread for managing the input and then
-- run the real loop where the input is processed and steps are executed.
gameLoop :: (Functor m,MonadIO m) => TetrisM m ()
gameLoop = do

  -- mvar containing the last key pressed
  keyMVar <- liftIO newEmptyMVar

  -- thread getting the input
  handleInputTId <- liftIO $ forkIO $ handleInputThread keyMVar

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
               refreshGui

    lift (use gameOver) -- if gameover then exit the loop

  -- kill the input thread because we don't need it anymore
  liftIO $ killThread handleInputTId

  -- if the reason for ending the program is "game over", then  print it else
  -- just exit
  gameIsOver <- lift (use gameOver)

  -- print game over and wait for user input before exiting
  when gameIsOver $ void $ liftIO $ do
    Curses.mvWAddStr Curses.stdScr  9 0 "$$$$$$$$$$$$"
    Curses.mvWAddStr Curses.stdScr 10 0 "  Game Over "
    Curses.mvWAddStr Curses.stdScr 11 0 "$$$$$$$$$$$$"
    Curses.mvWAddStr Curses.stdScr 24 1 "Press any button to exit"
    Curses.refresh
    Curses.getCh

-- given a list of rows that have been removed because filled with blocks, this
-- function adds the proper value to the user score and updates the game speed
updateSpeedAndScore :: (Monad m) => [Row] -> TetrisM m ()
updateSpeedAndScore rs = do
  let pointsToAdd = case length rs of
                      0 -> 0
                      1 -> 1
                      2 -> 3
                      3 -> 5
                      4 -> 9
                      _ -> undefined -- this should not happen
  when (pointsToAdd > 0) $
    modify $ over score (+ pointsToAdd)
           . over speed (\x -> max 1 (x - pointsToAdd))

-- the key used to manually exit the game
escKey :: Key
escKey = KeyChar 'q'

-- handle the user input until the escKey is pressed
handleInputThread :: MVar Key -> IO ()
handleInputThread mvar = do
  c <-  Curses.getCh
  putMVar mvar c
  unless (c == escKey) $ handleInputThread mvar

-- a commodity loop that checks both if esc has been pressed or the action
-- returned true (that means that the loop should stop)
loopUntilEsc :: (MonadIO m) => MVar Key -> (Maybe Key -> TetrisM m Bool) -> TetrisM m ()
loopUntilEsc mvar action = do
  c <- liftIO $ tryTakeMVar mvar
  unless (c == Just escKey) $ do
    isOver <- action c
    unless isOver (loopUntilEsc mvar action)

-- process the user input by changing the game state and return a representation of
-- the result of the action inside a ActionResult value
processInputKey :: (Functor m,Monad m) => Maybe Key -> TetrisM m ActionResult
processInputKey (Just KeyLeft)       = toActionResult' $ tryToMoveCurrBlock Left
processInputKey (Just KeyRight)      = toActionResult' $ tryToMoveCurrBlock Right
processInputKey (Just KeyDown)       = toActionResult' $ tryToMoveCurrBlock Down
processInputKey (Just (KeyChar 'x')) = toActionResult' $ tryToRotateCurrBlock Counterwise
processInputKey (Just (KeyChar 'z')) = toActionResult' $ tryToRotateCurrBlock Clockwise
processInputKey Nothing              = return NoAction
-- we ignore other keys, add here another key to extend keys handled
processInputKey _                    = return NoAction

-- refresh the graphical ui by erasing the window and "repainting" the state of
-- the board
refreshGui :: (Functor m,MonadIO m) => TetrisM m ()
refreshGui = do

  liftIO Curses.erase

  boardCoords <- lift (use board)

  -- print all the board rows with borders
  forM_ allRows $ \row -> do
    
    let rowI = toInt row

    -- left border
    mvAddCh rowI 0 (fromIntegral $ ord '|')

    -- board content
    forM_ allColumns $ \col -> do
      let c = if (row,col) `member` boardCoords then '#' else '.'
      mvAddCh rowI (toInt col + 1) (fromIntegral $ ord c)

    -- right border
    mvAddCh rowI 11 (fromIntegral $ ord '|')

  -- print the floor
  forM_ [0..11] $ \col -> mvAddCh 22 col (fromIntegral $ ord '-')


  -- print the block
  maybeBlock <- lift (use currBlock)
  case maybeBlock of
    Nothing       -> return ()
    Just    block -> forM_ (blockCoords' block) $ \(row,col) -> 
                       mvAddCh (toInt row) (toInt col+1) (fromIntegral $ ord 'o')


  -- print the speed
  use speed >>= mvAddStr 18 13 . (++) "Speed: " . show

  -- print the score
  use score >>= mvAddStr 20 13 . (++) "Score: " . show

  -- print the block infos
  mvAddStr 22 13 $ show maybeBlock

  liftIO Curses.refresh

  where 
    mvAddCh :: (MonadIO m) => Int -> Int -> ChType -> m ()
    mvAddCh col row c = liftIO $ Curses.mvAddCh col row c

    mvAddStr :: (MonadIO m) => Int -> Int -> String -> m ()
    mvAddStr col row s = liftIO $ Curses.mvWAddStr Curses.stdScr col row s
