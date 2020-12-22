module Days.Day06 (runDay, ) where

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

import qualified Program.RunDay as R (runDay, DayRunner)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO (Maybe Double, Maybe Double)
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (many1 letter `sepBy` endOfLine) `sepBy` (count 2 endOfLine)

------------ TYPES ------------
type Input = [[String]]

------------ PART A ------------
partA :: Input -> Int
partA = sum . fmap (length . Set.fromList . concat)

------------ PART B ------------
partB :: Input -> Int
partB = sum . fmap (length . foldl1' Set.intersection . fmap Set.fromList)