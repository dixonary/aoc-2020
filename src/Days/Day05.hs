module Days.Day05 (runDay) where

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
import Data.Void
import Control.Applicative.Combinators ((<|>))
import Control.Monad
import Data.Functor
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = let
    line = do
      rowSig  <- replicateM 7 $ (char 'F' $> fst) <|> (char 'B' $> snd)
      seatSig <- replicateM 3 $ (char 'L' $> fst) <|> (char 'R' $> snd)
      let 
        halveRow  seats r = r $ splitAt (length seats `div` 2) seats
        halveSeat seats c = c $ splitAt (length seats `div` 2) seats
        row  = foldl' halveRow  [0..127] rowSig
        seat = foldl' halveSeat [0..7  ] seatSig
      return $ head row * 8 + head seat
  in line `sepBy` endOfLine

------------ TYPES ------------
type Input = [Int]

------------ PART A ------------
partA :: Input -> Int
partA = maximum

------------ PART B ------------
partB :: Input -> Int
partB seatIds = head $ complement seatIds
  where
    complement ls = [minimum ls .. maximum ls] \\ ls
