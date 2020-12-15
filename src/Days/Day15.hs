module Days.Day15 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
import Debug.Trace
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = decimal `sepBy1` char ','

------------ TYPES ------------
type Input = [Integer]

num :: (Integer,Integer,Map Integer Integer) 
    -> (Integer,Integer,Map Integer Integer)
num (turn,x,mem) = case Map.lookup x mem of
  Nothing -> (turn+1,0,Map.insert x turn mem)
  Just t' -> (turn+1,turn-t',Map.insert x turn mem)

nums :: Int -> Input -> Integer
nums nth input = let
    seed = Map.fromList $ zip input [1..] 
    getNum (_,x,_) = x
    xs = zip [length input..] 
       $ getNum <$> iterate num (fromIntegral $ length input,last input,seed)
  in fromJust $ lookup nth xs

------------ PART A ------------
partA :: Input -> Integer
partA = nums 2020

------------ PART B ------------
partB :: Input -> Integer
partB = nums 30_000_000
