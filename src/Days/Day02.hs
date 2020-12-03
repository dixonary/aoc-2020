module Days.Day02 (runDay) where

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

import Data.Functor
import Control.Applicative.Combinators hiding (sepBy)
import Data.Bits

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  let line = do
      min <- decimal
      char '-'
      max <- decimal
      char ' '
      val <- letter
      string ": "
      password <- many letter
      return (min,max,val,password)
  line `sepBy` endOfLine


------------ TYPES ------------
type Input = [(Int,Int,Char,String)]

------------ PART A ------------
partA :: Input -> Int
partA = length . filter isValid
  where 
    isValid (min,max,val,password) = 
      let numVal = length $ filter (== val) password
      in numVal >= min && numVal <= max
  

------------ PART B ------------
partB :: Input -> Int
partB = length . filter isValid
  where 
    isValid (a,b,val,password) = 
      let posA = password !! (a - 1)
          posB = password !! (b - 1)
      in (posA == val) `xor` (posB == val)
