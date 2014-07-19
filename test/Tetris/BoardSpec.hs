{-# LANGUAGE TupleSections #-}
module Tetris.BoardSpec where

import Control.Monad
import qualified Data.List as List
import           Data.List as List ((\\))
import           Data.Maybe
import qualified Data.Set as Set
import           Data.Set (member,notMember)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Prelude hiding (pred,succ)

import Tetris.Board
import Tetris.Coord


spec :: Spec
spec = isFullSpec >> eraseRowsSpec >> collapseRowsSpec

-- this spec tests only one row assuming that `isFull` doesn't change its
-- behaviour based on the state of other rows
isFullSpec :: Spec
isFullSpec = describe "isFull" $ do

  forM_ allRows $ \row -> do
    it ("is False for row " ++ show row ++ " on the empty board") $ do
       not $ isFull Set.empty row

  forM_ allRows $ \row -> do
    it ("is True for row " ++ show row ++ " on the full board") $ do
       isFull fullBoard row

  forM_ allRows $ \row -> do
    forM_ (List.subsequences allColumns) $ \cols -> do
      let board = Set.fromList $ map (row,) cols
      let shouldBeTrue = cols == allColumns
      it ("is " ++ show shouldBeTrue ++ " for row " ++ show row
                ++ " on board with " ++ show cols ++ " columns occupied") $ do
        shouldBeTrue == isFull board row

eraseRowsSpec :: Spec
eraseRowsSpec = describe "eraseRow" $ do
  it "doesn't change the board if the rows list argument is empty" $ do
    fullBoard == eraseRows [] fullBoard

  it "returns the empty board when all rows are removed" $ do
    Set.empty == eraseRows allRows fullBoard

  forM_ allRows $ \row ->
    it ("removes the rows " ++ show row ++ " from the full board") $ do
      checkRemoveOf [row]

  prop "removes the given rows from the full board" $ do
    \rows -> checkRemoveOf rows

  where checkRemoveOf :: [Row] -> Bool
        checkRemoveOf rows = let board = eraseRows rows fullBoard
                                 stillFullRows = allRows \\ rows
                             in all (isEmpty board) rows && all (isFull board) stillFullRows

collapseRowsSpec :: Spec
collapseRowsSpec = describe "collapseRows" $ do
  it "doesn't change the board if the rows list argument is empty" $ do
    fullBoard `shouldBe` collapseRows [] fullBoard

  it "returns the empty board when all rows are removed" $ do
    Set.empty `shouldBe` collapseRows allRows fullBoard

  -- testing the removal and collapsing of rows via properties is complex and
  -- error-prone, we are going to test a few manual cases

  -- testing the simple case: removing just one line
  forM_ allRows $ \row -> do
    let maybePredRows =      fromTo (Digit One) `fmap` pred row
    let maybeSuccRows = flip fromTo maxBound    `fmap` succ row

    let board = collapseRows [row] boardWithAllRowsDiff

    it "removes the first line of the full board when the rows list contains one element" $ do
      board `shouldSatisfy` flip isEmpty minBound

    case maybeSuccRows of
      Nothing       -> return ()
      Just succRows -> 
        it "doesn't change the lines below the one removed" $ do
          board `shouldSatisfy` (\b -> all (\r -> sameRow r b r boardWithAllRowsDiff) succRows)

    case maybePredRows of
      Nothing       -> return ()
      Just predRows -> do
        let predRows' = map (fromJust . succ) predRows
        let predNewOldRows = List.zip predRows' predRows
        it "shift below by one the line above the one removed" $ do
          board `shouldSatisfy` (\b -> all (\(r,o) -> sameRow r b o boardWithAllRowsDiff) predNewOldRows)

  -- testing four sequential rows
  forM_ (tail $ tail $ tail $ allRows) $ \row -> do
    let row'   = fromJust $ fromInt (toInt row - 3)
    let rows   = fromTo row' row
    let maybePredRows =      fromTo (Digit Three) `fmap` pred row'
    let maybeSuccRows = flip fromTo maxBound      `fmap` succ row

    let board = collapseRows rows boardWithAllRowsDiff

    it "removes the first line of the full board when the rows list contains one element" $ do
      board `shouldSatisfy` \b -> (all (isEmpty b) $ fromTo minBound (Digit Three))

    case maybeSuccRows of
      Nothing       -> return ()
      Just succRows -> 
        it "doesn't change the lines below the one removed" $ do
          board `shouldSatisfy` (\b -> all (\r -> sameRow r b r boardWithAllRowsDiff) succRows)

    case maybePredRows of
      Nothing       -> return ()
      Just predRows -> do
        let predRows' = map (fromJust . succ) predRows
        let predNewOldRows = List.zip predRows' predRows
        it "shift below by one the line above the one removed" $ do
          board `shouldSatisfy` (\b -> all (\(r,o) -> sameRow r b o boardWithAllRowsDiff) predNewOldRows)


-- helpers

fullBoard :: Board
fullBoard = Set.fromList [(r,c) | r <- allRows, c <- allColumns]

boardWithAllRowsDiff :: Board
boardWithAllRowsDiff = Set.fromList cells
  where cells = List.concatMap (\(r,cs) -> map (r,) cs) $ List.zip allRows colPerms
        colPerms = List.filter ((>= 1) . length)
                 $ List.permutations allColumns

instance Arbitrary Row where
  arbitrary = elements allRows

isEmpty :: Board -> Row -> Bool
isEmpty b r = all (\c -> (r,c) `notMember` b) allColumns

sameRow :: Row -> Board -> Row -> Board -> Bool
sameRow r1 b1 r2 b2 = all (\col -> (r1,col) `member` b1 == (r2,col) `member` b2) allColumns
