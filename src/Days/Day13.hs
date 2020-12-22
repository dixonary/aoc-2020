module Days.Day13 (runDay, ) where

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
import Data.Functor

import qualified Program.RunDay as R (runDay, DayRunner)
import Data.Attoparsec.Text hiding (take)
import Data.Void
import Control.Applicative
import Data.Ord
import Debug.Trace (traceShow, traceShowId)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO (Maybe Double, Maybe Double)
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do 
  tot <- decimal
  endOfLine
  nums <- ((Just <$> decimal) <|> (char 'x' $> Nothing)) `sepBy1` char ','
  return (tot,zip [0..] nums)

------------ TYPES ------------
type Input = (Integer, [(Integer, Maybe Integer)])

------------ PART A ------------
partA :: Input -> Integer
partA (t,xs) = let 
  (fx, fr) = minimumBy (comparing snd) $ (\x -> (x,x - t `mod` x)) <$> mapMaybe snd xs
  in fx * fr

------------ PART B ------------
partB :: Input -> Integer
partB (_,xs) = let
  restrict ns (ix,x) = let (a:b:_) = filter (\n -> (n+ix) `mod` x == 0) ns in [a,b..]
  in head $ foldl' restrict [0..] $ mapMaybe sequence xs