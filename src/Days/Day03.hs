module Days.Day03 (runDay) where

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
import Control.Applicative
import Control.Applicative.Combinators (many)
import Data.Attoparsec.Text
import Data.Void
import Data.Functor
import Data.Function ((&))
import Control.Arrow ((>>>))
import Debug.Trace
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = 
  let 
    line = many1 $ (char '#' $> True) <|> (char '.' $> False)
  in line `sepBy1` endOfLine

------------ TYPES ------------
type Input = [[Bool]]

------------ PART A ------------
partA :: Input -> Int
partA = trees 3 1

trees :: Int -> Int -> [[Bool]] -> Int
trees dx dy lines = let
  lines' = lines
         & (U.chunksOf dy >>> map head)
         & fmap cycle
  in length $ filter id $ [l !! (i*dx) | (i,l) <- zip [0..] $ lines']


------------ PART B ------------
partB :: Input -> Int
partB lines = product $ (\(a,b) -> trees a b lines) <$> 
  [ (1,1)
  , (3,1)
  , (5,1)
  , (7,1)
  , (1,2)
  ]