{-# LANGUAGE TypeSynonymInstances, TypeApplications #-}

module Days.Day17 where

{- ORMOLU_DISABLE -}
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Util.Parsers as U
import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text ( Parser )
import Data.Bool ( bool )
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  space2 <- U.coordinateParser (bool Nothing (Just ()) . (== '#')) 0
  return $ Set.fromList $ Map.keys space2

------------ TYPES ------------
type Input = Space (Int,Int)
type Space a = Set a
type Coord2 = (Int,Int)
type Coord3 = (Int,Int,Int)
type Coord4 = (Int,Int,Int,Int)

class Ord a => Spacey a where
  from2D :: (Int,Int) -> a
  neighbours :: a -> Set a

  update :: Space a -> a -> (Space a -> Space a)
  update space p = 
    let ns = Set.size $ space `Set.intersection` neighbours p
    in if 
      |      p `Set.member` space  && ns /= 2 && ns /= 3  -> Set.delete p 
      | not (p `Set.member` space) && ns == 3             -> Set.insert p
      | otherwise                                         -> id

instance Spacey Coord3 where
  from2D (x,y) = (x,y,0)
  neighbours (x,y,z) = Set.fromList
    [ (x',y',z') 
    | (x',y',z') <- (,,) <$> [x-1..x+1] <*> [y-1..y+1] <*> [z-1..z+1]
    , (x',y',z') /= (x,y,z)
    ]

instance Spacey Coord4 where
  from2D (x,y) = (x,y,0,0)
  neighbours (x,y,z,w) = Set.fromList
    [ (x',y',z',w') 
    | (x',y',z',w') <- (,,,) <$> [x-1..x+1] <*> [y-1..y+1] <*> [z-1..z+1] <*> [w-1..w+1]
    , (x',y',z',w') /= (x,y,z,w)
    ]

allPossibles :: Spacey a => Space a -> Set a
allPossibles s = Set.unions $ Set.insert s $ Set.map neighbours s

tick :: Spacey a => Space a -> Space a
tick space = foldr (update space) space $ allPossibles space 

evolve :: Spacey a => Space Coord2 -> [Space a]
evolve = iterate tick . Set.map from2D

------------ PART A ------------
partA :: Input -> Int
partA space = Set.size $ (!!6) $ evolve @Coord3 space

------------ PART B ------------
partB :: Input -> Int
partB space = Set.size $ (!!6) $ evolve @Coord4 space
