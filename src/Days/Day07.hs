module Days.Day07 (runDay, ) where

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
import Data.Attoparsec.Text hiding (option,sepBy1)
import Data.Void
import Control.Applicative.Combinators 
import Data.Functor
import Data.Function
import Data.Bifunctor
import Debug.Trace
import Data.Tuple
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO (Maybe Double, Maybe Double)
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = 
  let 
    bg = do
      modifier <- many1 letter
      space
      colour <- many1 letter
      string " bag" >> optional (string "s")
      return (modifier,colour)

    nbg = (,) <$> decimal <*> (space >> bg)
    
    line = do
      container <- bg
      string " contain "
      bags <- (nbg `sepBy1` string ", " <* string ".")
              <|> ("no other bags." $> [])
      return (container,bags)

  in Map.fromList <$> line `sepBy1` endOfLine

------------ TYPES ------------
type Input = Map Colour [(Int,Colour)]

type Colour = (String,String)

collate :: Ord a => [(a,b)] -> Map a [b]
collate = Map.fromListWith (++) . map (second pure)

decollate :: Map a [b] -> [(a,b)]
decollate = concatMap (\(a,bs) -> (a,) <$> bs) . Map.assocs 

------------ PART A ------------
partA :: Input -> Int
partA bags = let
  bagsR = bags & Map.map (map snd) & decollate & map swap & collate
  ancestors col = nub 
    $ col : concatMap ancestors (Map.findWithDefault [] col bagsR)
  in length (ancestors ("shiny","gold")) - 1

------------ PART B ------------
partB :: Input -> Int
partB bags = let
    numBags col = 1 + sum (fmap (\(n,c) -> n * numBags c) (bags Map.! col))
  in numBags ("shiny","gold") - 1