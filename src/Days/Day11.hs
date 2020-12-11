module Days.Day11 (runDay) where

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
import qualified Util.Parsers as P

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void ( Void )
import Control.Monad
import Control.Arrow
import Data.Monoid
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = P.coordinateParser chair 0
  where
    chair '.' = Nothing
    chair 'L' = Just False

------------ TYPES ------------
type Input = Chairs

type Coord = (Int,Int)
type Chairs = Map Coord Bool


totalOccupied :: Chairs -> Int
totalOccupied = length . filter id . Map.elems

------------ PART A ------------
partA :: Input -> Int
partA = totalOccupied . U.converge step

step :: Chairs -> Chairs
step c = foldl' update c (Map.keys c)
  where
    update m pos = case neighbours pos c of
      0 -> Map.insert pos True m
      x | x >= 4 -> Map.insert pos False m
      _ -> m

neighbours :: Coord -> Chairs -> Int
neighbours (x,y) c = length 
  [ () 
  | pos' <- (,) <$> [x-1..x+1] <*> [y-1..y+1]
  , pos' /= (x,y)
  , Map.lookup pos' c == Just True
  ]

------------ PART B ------------
partB :: Input -> Int
partB = totalOccupied . U.converge step'

step' :: Chairs -> Chairs
step' c = foldl' update c (Map.keys c)
  where
    update m pos = case neighbours' pos c of
      0 -> Map.insert pos True m
      x | x >= 5 -> Map.insert pos False m
      _ -> m
      
neighbours' :: Coord -> Chairs -> Int
neighbours' (x,y) c = length 
  [ () 
  | dir <- (,) <$> [-1..1] <*> [-1..1]
  , dir /= (0,0)
  , ray (x,y) dir c == Just True
  ]

size :: Chairs -> Int
size c = let (ws,hs) = unzip $ Map.keys c in max (maximum ws) (maximum hs)

ray :: Coord -> (Int,Int) -> Chairs -> Maybe Bool
ray (x,y) (dx,dy) c = msum [Map.lookup (x+i*dx, y+i*dy) c | i <- [1..size c]] 