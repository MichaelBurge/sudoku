{-# LANGUAGE UnicodeSyntax #-}

import Prelude.Unicode
import Data.List
import Data.Function
import Data.Maybe

type Row = [Maybe Integer]
type Board = [Row]

sample ∷ Board
--sample = take 9 $ repeat [1..9]
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

sample2 ∷ Board
sample2 = transpose sample

sample3 ∷ Board
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

rowUnique ∷ Row → Bool
rowUnique ys = let zs = filter isJust ys in ((==) `on` length) zs (nub zs)
numElemsGood ∷ Row → Bool
numElemsGood ys = (length ys) == 9
rowGood ∷ Row → Bool
--rowGood ys = rowUnique ys ∧ numElemsGood ys
rowGood ys = rowUnique ys

allRowsGood ∷ Board → Bool
allRowsGood xss = all rowGood xss

allColsGood ∷ Board → Bool
allColsGood = allRowsGood ∘ transpose

allSegmentsGood ∷ Board → Bool
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