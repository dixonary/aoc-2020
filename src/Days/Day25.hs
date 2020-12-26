module Days.Day25 (runDay) where

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
import qualified Util.Parsers as U

import qualified Program.RunDay as R (runDay, DayRunner)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO (Maybe Double, Maybe Double)
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = decimal `U.around` endOfLine

------------ TYPES ------------
type Input = (Int,Int)

step :: Int -> Int -> Int 
step subject x = subject*x `mod` 20201227

------------ PART A ------------
partA :: Input -> Int
partA (x,y) = let
  Just loopX = elemIndex x $ iterate (step 7) 1
  in iterate (step y) 1 !! loopX

------------ PART B ------------
partB :: Input -> Yeet
partB = const Yeet

data Yeet = Yeet
instance Show Yeet where
  show = const "Yeet!"