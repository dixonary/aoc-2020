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
import qualified Util.Parsers as U

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text hiding (take)

import Data.Functor ( ($>) )
import Control.Applicative
import Data.Function ( (&) )
import Control.Arrow

import Control.Monad.ST (ST, runST)
import Data.STRef
import Control.Monad (unless, when)
import Control.Monad.Loops ( unfoldrM )
import Data.Bool ( bool )
import qualified Data.Text as Text
import Data.Bifunctor
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = let
    tile = do
      tileId <- string "Tile " *> decimal <* string ":" 
      endOfLine 
      image <- many1 (char '.' $> False <|> char '#' $> True) `sepBy1` endOfLine 
      return $ mkTile tileId image
  in tile `sepBy1` count 2 endOfLine

------------ TYPES ------------
type Input = [Tile]

type Image = [[Bool]]

renderImage :: Image -> String
renderImage = unlines . fmap (map (bool '.' '#'))

instance {-# OVERLAPS #-} Show [[Bool]] where
  show = ('\n' : ) . renderImage

mkTile :: Int -> [[Bool]] -> Tile
mkTile tileId image = let
  fromBits = foldl' (\x y -> x*2+fromEnum y) 0
  edges = fromBits <$> [head image, last <$> image, last image, head <$> image]
  in Tile{..}

data Tile = Tile 
  { tileId :: Int 
  , edges :: [Int]
  , image :: [[Bool]]
  }
  deriving (Show, Eq)

flipTile :: Tile -> Tile
flipTile Tile{..} = mkTile tileId $ flipImage image

rotations :: Tile -> [Tile]
rotations = let 
  rotate Tile{..} = mkTile tileId $ rotateImage image
  in take 4 . iterate rotate

-- Rotate 90 degrees. (No idea which direction)
rotateImage :: Image -> Image
rotateImage = transpose . reverse

-- Flip around the vertical axis.
flipImage :: Image -> Image
flipImage = fmap reverse
    
allConfigs :: Tile -> [Tile]
allConfigs t = concatMap rotations [t,flipTile t]

corners :: [Tile] -> [Tile]
corners tiles = let
  edgeCounts = U.freq $ tiles >>= allConfigs >>= edges
  isOuter e = edgeCounts Map.! e == 4
  isCorner Tile{..} = length (filter isOuter edges) == 2

  in nub $ filter isCorner tiles

edgeN, edgeE, edgeS, edgeW :: Tile -> Int 
edgeN Tile{..} = edges !! 0
edgeE Tile{..} = edges !! 1
edgeS Tile{..} = edges !! 2
edgeW Tile{..} = edges !! 3

------------ PART A ------------
partA :: Input -> Int
partA tiles = product $ tileId <$> corners tiles

------------ PART B ------------
partB :: Input -> Int
partB tiles = let
  firstCorner:_ = corners tiles
  
  -- Given the first corner, rotate it so that it is the northwest corner.
  edgeCounts = U.freq $ tiles >>= allConfigs >>= edges
  isOuter e = edgeCounts Map.! e == 4
  isCornerOf fa fb t = isOuter (fa t) && isOuter (fb t) 
  [nwCorner] = filter (isCornerOf edgeN edgeW) $ rotations firstCorner 

  -- STEP ONE: Complete the puzzle. --
  finishedPuzzle = runST $ do
    pieces <- newSTRef $ concatMap allConfigs tiles

    let 
      deleteTile t = modifySTRef pieces (filter (\t' -> tileId t' /= tileId t))

      -- Find the single tile which fits alongisde the given edge value.
      findNext edgeVal edgeF = do
        pieces' <- readSTRef pieces
        case find (\t -> edgeF t == edgeVal) pieces' of
          Nothing -> return Nothing 
          Just t  -> deleteTile t >> return (Just t)

      -- Given a seed value, construct a complete line of the puzzle.
      populate newTileF oldTileF startTile = let
        getNext val = fmap (\x -> (x,oldTileF x)) <$> findNext val newTileF
        newTiles = unfoldrM getNext $ oldTileF startTile
        in (startTile :) <$> newTiles

    deleteTile nwCorner

    leftEdge <- populate edgeN edgeS nwCorner 
    mapM (populate edgeW edgeE) leftEdge

  -- STEP TWO: Construct the image. --

  -- Remove borders from one (sub-)image.
  strip :: [[Bool]] -> [[Bool]]
  strip ts = ts & tail & init & fmap tail & fmap init

  -- Construct total image from grid of sub-images
  puzzleImage = finishedPuzzle
    & fmap (fmap (image >>> strip) >>> transpose >>> fmap concat)
    & concat
    
  -- STEP THREE: Identify the sea monsters. --

  -- All rough patches in this orientation.
  roughSet :: Image -> Set (Int,Int)
  roughSet image = let
    imageText  = Text.pack $ renderImage image
    isRough = \case {'#' -> Just (); _ -> Nothing }
    Right roughMap = parseOnly (U.coordinateParser isRough 0) imageText
    in Set.fromList $ Map.keys roughMap

  seaMonsterOffsets :: Set (Int, Int)
  seaMonsterOffsets = Set.fromList
    [                                                                         (18,0)
    , (0,1)     ,     (5,1),(6,1)     ,      (11,1),(12,1)      ,      (17,1),(18,1),(19,1)
    ,      (1,2),(4,2)     ,     (7,2),(10,2)      ,      (13,2),(16,2)
    ]

  seaMonsterPositions :: Image -> Set (Int,Int)
  seaMonsterPositions image = let
    rough = roughSet image
    (minX,maxX) = (minimum &&& maximum) $ Set.map fst rough
    (minY,maxY) = (minimum &&& maximum) $ Set.map snd rough
    smTopLefts = (,) <$> [minX..maxX] <*> [minY..maxY]
    monsterAtOffset (x,y) = Set.map (bimap (+x) (+y)) seaMonsterOffsets
    seaMonsters = monsterAtOffset <$> smTopLefts
    trueSeaMonsters = filter (`Set.isSubsetOf` rough) seaMonsters
    in Set.unions trueSeaMonsters

  allSeaMonsterPositions :: Set (Int,Int)
  allSeaMonsterPositions = [puzzleImage, flipImage puzzleImage]
                         & concatMap (take 4 . iterate rotateImage) 
                         & fmap seaMonsterPositions
                         & Set.unions -- Only one will be nonempty
  
  in Set.size (roughSet puzzleImage) - Set.size allSeaMonsterPositions