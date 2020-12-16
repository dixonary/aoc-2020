module Days.Day16 (runDay) where

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
import qualified Util.Parsers as P
import Data.Void
import Control.Monad
import Debug.Trace
import Data.Function
import Data.Functor
import Data.Bifunctor
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  constraints <- fmap Map.fromList $ (`sepBy1` endOfLine) $ do
    label <- manyTill (notChar ':') (char ':')
    space
    ranges <- (decimal `P.around` char '-') `sepBy1` string " or "
    return (label, ranges)
  skipSpace 
  string "your ticket:"
  skipSpace 
  my <- decimal `sepBy1` char ','
  skipSpace 
  string "nearby tickets:"
  skipSpace 
  nearby <- (decimal `sepBy1` char ',') `sepBy1` endOfLine 
  return (constraints,my,nearby)

------------ TYPES ------------
type Input = (Map String [Range],Ticket,[Ticket])

type Range = (Int,Int)
type Ticket = [Int]

inRange :: Int -> Range -> Bool 
inRange x (lo,hi) = x >= lo && x <= hi

inOneOf :: Int -> [Range] -> Bool
val `inOneOf` ranges = any (val `inRange`) ranges

------------ PART A ------------
partA :: Input -> Int
partA (allRanges,_,nearby) = 
  sum $ filter (not . (`inOneOf` concat allRanges)) $ concat nearby

------------ PART B ------------
partB :: Input -> Int
partB (allRanges,myTicket,nearby) = let
    -- Columns, excluding those tickets with invalid values
    cols = transpose $ filter (all (`inOneOf` concat allRanges)) nearby

    -- Given a column, get those indices which might be a valid assignment
    possibles name = 
      let ranges = allRanges Map.! name
      in filter (all (`inOneOf` ranges) . (cols !!)) [0..length cols - 1]

    -- The below line is fun. Observations:
    -- (1) The nth possibility list differs from the n-1th by ONE element.
    -- (2) `zipWithM f a b` is `sequence . zipWith f a b`
    -- So zipWithM takes the differences (one) at each index and gives
    -- back the list of all such possible differences (hence, one list).
    match res = head $ zipWithM (\\) res ([]:res)

  in Map.keys allRanges                  -- All field names
    & U.pairWith possibles               -- Paired with possible assignments
    & sortOn (length . snd)              -- Ordered by number of possibilities
    & uncurry zip . second match . unzip -- 'Fix' the possibilities, magically
    & filter (isInfixOf "departure" . fst)
    & fmap ((myTicket !!) . snd)         -- Lookup departure fields in ticket
    & product