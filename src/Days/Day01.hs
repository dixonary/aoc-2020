module Days.Day01 (runDay, partA) where

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

{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = decimal `sepBy` endOfLine

------------ TYPES ------------
type Input = [Int]

------------ PART A ------------
partA :: Input -> Int
-- partA input = head 
--     [ x * y 
--     | x <- input
--     , y <- input
--     , x + y == 2020 
--     ]
partA = subseqSum 2

------------ PART B ------------
partB :: Input -> Int
-- partB input = head 
--     [ x * y * z 
--     | x <- input
--     , y <- input
--     , z <- input
--     , x + y + z == 2020 
--     ]
partB = subseqSum 3

subseqs l 1 = transpose [l]
subseqs l n = (:) <$> l <*> subseqs l (n-1)

subseqSum n l = head [product xs | xs <- subseqs l n, sum xs == 2020]