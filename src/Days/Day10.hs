module Days.Day10 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import qualified Data.Vector as Vec

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text

import Control.Monad

import Control.Monad.ST
import qualified Data.Vector.Mutable as MVec
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = decimal `sepBy1` endOfLine

------------ TYPES ------------
type Input = [Int]

------------ PART A ------------
partA :: Input -> Int
partA ls = let
  ls' = sort (0:maximum ls + 3:ls)
  diffs = zipWith (-) (tail ls') ls'
  in length (filter (== 1) diffs) * length (filter (== 3) diffs)

------------ PART B ------------
partB :: Input -> Integer
partB ls = runST $ do
  let top = maximum ls + 3
  let ls' = sort (top:ls)
  arr <- Vec.thaw $ Vec.replicate (fromIntegral $ top + 1) 0
  MVec.write arr 0 1
  forM_ [1..top] $ \i -> do
    if i `notElem` ls' 
    then pure ()
    else do
      m2 <- if i < 2 then return 0 else MVec.read arr (i-2)
      m3 <- if i < 3 then return 0 else MVec.read arr (i-3)
      m1 <- if i < 1 then return 0 else MVec.read arr (i-1)
      MVec.write arr i $ m1 + m2 + m3
  MVec.read arr top