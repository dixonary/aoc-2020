module Days.Day24 (runDay) where

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
import Data.Bifunctor
import Data.Ratio
import Data.Functor
import Data.Function
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO (Maybe Double, Maybe Double)
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = let
    direction = choice [
      "nw" $> NW, "w"  $> W, "sw" $> SW,
      "ne" $> NE, "e"  $> E, "se" $> SE
      ]
  in many1 direction `sepBy1` endOfLine

------------ TYPES ------------
type Input = [[Direction]]
type Pos = (Rational,Rational)

data Direction
      =NW|NE
    |W      |E
      |SW|SE 
  deriving (Show, Bounded, Enum)

move :: Pos -> Direction -> Pos
move (x,y) NW = (x - 1%2, y + 1%2)
move (x,y) NE = (x + 1%2, y + 1%2)
move (x,y) W  = (x - 1  , y      )
move (x,y) E  = (x + 1  , y      )
move (x,y) SW = (x - 1%2, y - 1%2)
move (x,y) SE = (x + 1%2, y - 1%2)

-- Symmetric difference
(⊕) :: Ord a => Set a -> Set a -> Set a
a ⊕ b = (a `Set.union` b) Set.\\ (a `Set.intersection` b)

makeFloor :: Input -> Set Pos
makeFloor = foldl1' (⊕) . fmap (Set.singleton . foldl' move (0,0)) 

------------ PART A ------------
partA :: Input -> Int
partA = Set.size . makeFloor

------------ PART B ------------
partB :: Input -> Int
partB = Set.size . (!!100) . iterate tick . makeFloor

neighbours :: Pos -> Set Pos
neighbours p = Set.fromList $ map (move p) [minBound..maxBound]

tickTile :: Set Pos -> Pos -> Set Pos
tickTile set p = let
    bn = Set.size $ neighbours p `Set.intersection`  set
    black = p `Set.member` set
  in if
  | black && (bn == 1 || bn == 2) -> Set.empty
  | black                         -> Set.singleton p
  | bn == 2                       -> Set.singleton p
  | otherwise                     -> Set.empty

tick :: Set Pos -> Set Pos
tick set = set
  & Set.map neighbours      -- Take all neighbours of active cells
  & Set.insert set          -- and the active cells themselves
  & Set.unions              -- as a single set.
  & Set.map (tickTile set)  -- Check them all to see if they change
  & Set.unions              -- and collect all the ones that do
  & (⊕) set                -- then change them in the original set.