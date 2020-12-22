module Days.Day10 (runDay, ) where

{- ORMOLU_DISABLE -}
import Data.List
import qualified Data.Vector as Vec
import qualified Data.Map as Map

import qualified Program.RunDay as R (runDay, DayRunner)
import Data.Attoparsec.Text

import Control.Monad

import Control.Monad.ST
import qualified Data.Vector.Mutable as MVec
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO (Maybe Double, Maybe Double)
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
partB ls = 
  let 
    top = maximum ls + 3
    m !? i = Map.findWithDefault 0 i m
    go m n = Map.insert n (m !? (n-1) + m !? (n-2) + m !? (n-3)) m
  in foldl' go (Map.fromList [(0,1)]) (sort $ top:ls) Map.! top