{-# LANGUAGE TypeApplications #-}
module Days.Day15 (runDay) where

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
import Debug.Trace

import qualified Data.Vector.Unboxed.Mutable as MVec
import Control.Monad.ST
import Data.Int (Int)
import Data.STRef
import Control.Monad
import Control.Arrow
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = decimal `sepBy1` char ','

------------ TYPES ------------
type Input = [Int]

nums :: Int -> Input -> Int
nums nth input = runST $ do
  nums <- MVec.replicate nth 0
  num  <- newSTRef 0

  -- Insert first values
  forM_ (zip input [1..]) $ \(num',turn') -> do
    MVec.write nums num' turn'
    writeSTRef num num'
  
  -- Insert rest
  forM_ [length input + 1..nth] $ \turn -> do 
    num' <- readSTRef num
    last <- MVec.read nums num'
    MVec.write nums num' (turn-1)
    writeSTRef num $ if last == 0 then 0 else turn - last - 1

  readSTRef num

------------ PART A ------------
partA :: Input -> Int
partA = nums 2020

------------ PART B ------------
partB :: Input -> Int
partB = nums 30_000_000
