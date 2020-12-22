module Days.Day05 (runDay, ) where

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
import Control.Applicative.Combinators ((<|>))
import Control.Monad
import Data.Functor
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO (Maybe Double, Maybe Double)
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = let
    seatId =  foldl1' (\x y -> y + (x*2))
        <$> many1 ( ((char 'F' <|> char 'L') $> 0) <|> (letter $> 1) )
  in seatId `sepBy` endOfLine

------------ TYPES ------------
type Input = [Int]

------------ PART A ------------
partA :: Input -> Int
partA = maximum

------------ PART B ------------
partB :: Input -> Int
partB seatIds = head $ complement seatIds
  where
    complement ls = [minimum ls .. maximum ls] \\ ls