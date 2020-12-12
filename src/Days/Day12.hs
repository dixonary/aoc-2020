module Days.Day12 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List ( foldl' )
import Data.Functor ( ($>) )

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Monoid
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = let
  movement = choice
    [ char 'N' $> Dir (0,-1), char 'E' $> Dir (1,0) , char 'S' $> Dir (0,1)
    , char 'W' $> Dir (-1,0) , char 'L' $> L     , char 'R' $> R
    , char 'F' $> F
    ]
  in ((,) <$> movement <*> decimal) `sepBy1` endOfLine

------------ TYPES ------------
type Input = [(Movement,Int)]

data Movement = Dir Coord | L | R | F
  deriving (Show)

type Coord = (Sum Int,Sum Int)

manhattan :: Coord -> Int
manhattan (a,b) = abs (getSum a) + abs (getSum b)

moveDir :: Coord -> Int -> Coord
moveDir c d = mconcat $ replicate d c

rot :: Coord -> Int -> Coord
rot (x,y) amt = case (((fromIntegral amt `div` 90) `mod` 4) + 4) `mod` 4 of
  0 -> (x,y)
  1 -> (-y,x)
  2 -> (-x,-y)
  3 -> (y,-x)

------------ PART A ------------
partA :: Input -> Int
partA = manhattan . snd . foldl' move ((1,0), (0,0))

move :: (Coord,Coord) -> (Movement,Int) -> (Coord,Coord)
move (b,pos) (Dir dr,d) = (b, pos <> moveDir dr d)
move (b,pos) (L,d)      = (rot b (-d),pos)
move (b,pos) (R,d)      = (rot b d   ,pos)
move (b,pos) (F,d)      = (b, pos <> moveDir b d)

------------ PART B ------------
partB :: Input -> Int
partB = manhattan . snd . foldl' move' ((10,-1),(0,0))

move' :: (Coord,Coord) -> (Movement,Int) -> (Coord,Coord)
move' (wp,sp) (Dir dr,d) = (wp <> moveDir dr d, sp)
move' (wp,sp) (L,d)      = (rot wp (-d),sp)
move' (wp,sp) (R,d)      = (rot wp d   ,sp)
move' (wp,sp) (F,d)      = (wp, sp <> moveDir wp d)