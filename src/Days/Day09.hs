module Days.Day09 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text

import Data.Foldable
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = decimal `sepBy` endOfLine

------------ TYPES ------------
type Input = [Integer]

------------ PART A ------------
partA :: Input -> Integer
partA input = let 
    notInSums :: (Eq a, Num a) => a -> [a] -> Bool
    notInSums x xs = let xs'  = toList xs 
                     in x `notElem` [a + b | a <- xs', b <- xs']

    go xs (y:ys) = if y `notInSums` xs then y else go (y : init xs) ys
    in uncurry go $ splitAt 25 input
    
------------ PART B ------------
partB :: Input -> Integer
partB input = let 
  a = partA input
  weakness x = maximum x + minimum x
  in weakness $ head $ filter (\x -> sum x == a) 
              $ concatMap inits $ tails input