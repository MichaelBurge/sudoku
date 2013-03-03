{-# LANGUAGE UnicodeSyntax #-}

import Prelude.Unicode
import Data.List
import Data.Function
import Data.Maybe

type Row = [[Integer]]
type Board = [Row]

--sample ∷ Board
--sample = take 9 $ repeat [1..9
sample = [
 [ 1, 2, 3, 4, 5, 6, 7, 8, 9],
 [ 1, 2, 3, 4, 5, 6, 7, 8, 9],
 [ 1, 2, 3, 4, 5, 6, 7, 8, 9],
 [ 1, 2, 3, 4, 5, 6, 7, 8, 9],
 [ 1, 2, 3, 4, 5, 6, 7, 8, 9],
 [ 1, 2, 3, 4, 5, 6, 7, 8, 9],
 [ 1, 2, 3, 4, 5, 6, 7, 8, 9],
 [ 1, 2, 3, 4, 5, 6, 7, 8, 9],
 [ 1, 2, 3, 4, 5, 6, 7, 8, 9]]

--sample2 ∷ Board
sample2 = transpose sample

--sample3 ∷ Board
sample3 = [
 [ 2,4,8,3,9,5,7,1,6],
 [ 5,7,1,6,2,8,3,4,9],
 [ 9,3,6,7,4,1,5,8,2],
 [ 6,8,2,5,3,9,1,7,4],
 [ 3,5,9,1,7,4,6,2,8],
 [ 7,1,4,8,6,2,9,5,3],
 [ 8,6,3,4,1,7,2,9,5],
 [ 1,9,5,2,8,6,4,3,7],
 [ 4,2,7,9,5,3,8,6,1]]

type Filter = Board → Bool

rowUnique ∷ Row → Bool
rowUnique ys = 
    let zs = filter (\xs → (length xs) == 1) ys
    in ((==) `on` length) zs (nub zs)

numElemsGood ∷ Row → Bool
numElemsGood ys = (length ys) == 9
rowGood ∷ Row → Bool
rowGood ys = rowUnique ys

allRowsGood ∷ Filter
allRowsGood xss = all rowGood xss

allColsGood ∷ Filter
allColsGood = allRowsGood ∘ transpose

allSegmentsGood ∷ Filter
allSegmentsGood [] = True
allSegmentsGood xss = 
    let checkSegment zss = rowGood $ concat zss
        checkSplitCols ([], []) = True
        checkSplitCols (segment, others) =
            checkSegment segment ∧
            checkSplitCols (splitAt 3 others)
        checkSplitRows ([], []) = True
        checkSplitRows (initss, tailss) = 
            checkSplitCols (splitAt 3 $ transpose initss) ∧
            checkSplitRows (splitAt 3 tailss)
    in checkSplitRows ([], xss)

lift ∷ [[Maybe Integer]] → Board
lift = let
    f Nothing = [1..9]
    f (Just x) = [x]
  in map (map f)

board_set ∷ ([Integer] → [Integer] → [Integer]) → Board → Board → Board
board_set f = zipWith (zipWith (\a b → a `f` b))

board_intersect ∷ Board → Board → Board
board_intersect = board_set intersect

board_union ∷ Board → Board → Board
board_union = board_set union

possibilities ∷ Board → [Board]
possibilities board = let
  result = do
  row_idx ← [1..length board]
  let row = board !! (row_idx-1)
  cell_idx ← [1.. length row]
  let cell = row !! (cell_idx-1)
  if 1 == length cell
    then []
    else do
      possibility ← cell
      let newRow = (take (cell_idx-1) row) ++ [[possibility]] ++ (drop cell_idx row)
      let newBoard = (take (row_idx-1) board) ++ [newRow] ++ (drop row_idx board)
      return newBoard
  in if null result
        then return board
        else result     
  

basicApply ∷ Filter → Board → Board
basicApply f b = foldl1 board_intersect $ filter f $ possibilities b

lift2 :: [[Integer]] -> Board
lift2 = let
  f x = case x of
    0 -> [1..9]
    _ -> [x]
  in map (map f)
sample4 :: [[Integer]]
sample4 = [
 [ 2,4,8,3,9,5,7,1,6],
 [ 5,7,1,6,2,8,3,4,9],
 [ 9,3,6,7,4,1,5,8,2],
 [ 6,8,2,5,3,9,1,7,4],
 [ 3,5,9,1,0,0,6,2,8],
 [ 7,1,4,8,6,2,9,5,3],
 [ 8,6,3,4,1,7,2,9,5],
 [ 1,9,5,2,8,6,4,3,7],
 [ 4,2,7,9,5,3,8,6,1]]

printBoard :: Board -> String
printBoard b = foldl1 (\a b -> a ++ "\n" ++ b) $ map show b

printBoards :: [Board] -> IO ()
printBoards = mapM_ (\s -> putStrLn s >> putStrLn "\n") . map printBoard

sample5 :: [[Integer]]
sample5 = [
 [ 2,4,8,3,9,5,7,1,6],
 [ 5,7,1,6,2,8,3,4,9],
 [ 9,3,6,7,4,1,5,8,2],
 [ 0,0,0,0,0,0,0,0,4],
 [ 0,0,0,0,0,0,0,0,0],
 [ 7,1,4,8,6,2,9,5,3],
 [ 8,6,3,4,1,7,2,9,5],
 [ 1,9,5,2,8,6,4,3,7],
 [ 4,2,7,9,5,3,8,6,1]]

allPossibilities :: Board -> [Board]
allPossibilities b = nub $ let
  ps = possibilities b
  in if length ps == 0
         then [b]
         else concatMap allPossibilities $ possibilities b

finalApply :: Filter -> Board -> [Board]
finalApply f = filter f . allPossibilities

rulesFilter :: Filter
rulesFilter b = foldl1 (&&) $ map ($ b) [allRowsGood, allColsGood, allSegmentsGood]

repeatApply :: Filter -> Board -> Board
repeatApply f b = let
  g [] = [1..9]
  g xs = xs
  h = map (map g)
  result = h $ basicApply f b
  in if result == b
     then result
     else h $ repeatApply f result

answer = printBoards $ finalApply rulesFilter (lift2 sample5)
-- TODO:
-- 1. Write backtrackApply
-- 2. Write more filters
-- 3. Need example sudoku puzzles
-- 4. Need a way to load sukodu puzzles from outside files

--backtrackApply ∷ Filter → Board → Board
--backtrackApply 