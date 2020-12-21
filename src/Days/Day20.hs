module Days.Day20 (runDay) where

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
import Data.Attoparsec.Text hiding (take)
import Data.Void
import Data.Functor
import Control.Applicative
import Data.Function
import Control.Arrow

import Data.Vector.Mutable (STVector)
import qualified Data.Vector.Mutable as MVec
import Control.Monad.ST (runST)
import Data.STRef
import Control.Monad (unless, when)
import Control.Monad.Loops
import Debug.Trace
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = let
    tile = do
      string "Tile "
      i <- decimal 
      string ":"
      endOfLine 
      rows <- many1 (char '#' $> 1 <|> char '.' $> 0) `sepBy1` endOfLine 
      let
        edgeBits = [head rows, last <$> rows, last rows, reverse $ head <$> rows]

      return $ Tile i edgeBits rows
         
  in tile `sepBy1` (endOfLine >> endOfLine)

------------ TYPES ------------
type Input = [Tile]

type Edge = [Int]

data Tile = Tile 
  { tileId :: Integer
  , edges :: [Edge]
  , image :: [[Int]]
  } deriving (Show)

instance Eq Tile  where (==) = (==) `on` tileId
instance Ord Tile where compare = compare `on` tileId

-- instance Show Tile where 
--   show Tile{..} = "Tile "++show tileId++":\n"++ show image

edgeN, edgeE, edgeS, edgeW :: Tile -> Edge
edgeN Tile{..} = edges !! 0
edgeE Tile{..} = edges !! 1
edgeS Tile{..} = edges !! 2
edgeW Tile{..} = edges !! 3

instance {-# OVERLAPS #-} Show [[Int]] where
  show = ("\n"++) . unlines . fmap (fmap (\case {0 -> '.'; 1 -> '#'}))

rotations :: Tile -> [Tile]
rotations t = take 4 $ flip iterate t $
  \t@Tile{..} -> t
      { edges = let [n,e,s,w] = edges in [reverse w, n, reverse e, s]
      , image = transpose $ reverse image
      }

flipTile :: Tile -> Tile
flipTile t = t 
    { edges = let [n,e,s,w] = edges t in [reverse n, w, reverse s, e]
    , image = fmap reverse (image t)
    }
      
stripped :: [[Int]] -> [[Int]]
stripped = init >>> tail >>> fmap init >>> fmap tail


------------ PART A ------------
partA :: Input -> Integer
partA tiles = let
  allEdges = concatMap (\x -> [x,reverse x]) $ concatMap edges tiles
  edgeFreqs = U.freq allEdges
  singleton e = edgeFreqs Map.! e == 1
  isCorner Tile{..} = length (filter id (singleton <$> edges)) == 2

  in product $ tileId <$> filter isCorner tiles

------------ PART B ------------
partB :: Input -> [[Tile]]
partB tiles = let

  -- Shorthands for detecting edges and corners (borrowed from part A)
  allEdges = concatMap (\x -> [x,reverse x]) $ concatMap edges tiles
  edgeFreqs = U.freq allEdges
  singleton e = edgeFreqs Map.! e == 1
  isCorner Tile{..} = length (filter id (singleton <$> edges)) == 2

  firstCorner = fromJust $ find isCorner tiles
  [cornerNW]  = filter (\t -> singleton (edgeW t) && singleton (edgeN t)) 
              $ rotations firstCorner

  solvedJigsaw :: [[Tile]]
  solvedJigsaw = runST $ do
    jigsaw <- newSTRef []
    pieces <- newSTRef $ Set.fromList tiles

    currentRow <- newSTRef [cornerNW]

    -- Remove the tile with a given ID.
    let deleteTile t = modifySTRef pieces $
                      Set.filter (\t' -> tileId t' /= tileId t)

    deleteTile cornerNW

    -- Add row until piece set empty
    let fillJigsaw = do
          currentRow' <- trace "Row!" <$> readSTRef currentRow
          when (null currentRow') (void addLeftEdge)

          -- Place pieces in the row until we place a piece with no matching 
          -- east side. (Note: this is probably slow!)
          lastPiece <- iterateUntil (singleton . edgeE) addNextPiece

          -- Store row into jigsaw and clear it
          currentRow' <- readSTRef currentRow 
          modifySTRef jigsaw (++ [currentRow']) 
          writeSTRef currentRow []
          
          -- Check if we just placed then last piece on last row,
          -- and if we didn't then run the row procedure again
          unless (singleton $ edgeS lastPiece) fillJigsaw

        -- Get the oriented correct piece for a given hole.
        takePiece edgeFunc e  = do
          pieces' <- readSTRef pieces
          let 
            -- Find all tiles in any flip/rotation which fit in the given hole.
            -- THERE SHOULD ALWAYS BE EXACTLY ONE.
            [tile] = pieces'
                & concatMap (\x -> [x,flipTile x])
                & concatMap rotations
                & filter (\x -> edgeFunc x == e)
                & traceShowId
          deleteTile tile
          return tile

        addPiece edgeVal edgeFunc = do
          tile <- edgeVal >>= takePiece edgeFunc
          modifySTRef currentRow (++[tile])
          return tile

        addLeftEdge  = addPiece (edgeS . head . last <$> readSTRef jigsaw) edgeN
        addNextPiece = addPiece (edgeE . last <$> readSTRef currentRow)    edgeW

    fillJigsaw

    readSTRef jigsaw

  in solvedJigsaw