module Days.Day23 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List hiding (span,insert,find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector.Unboxed as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, DayRunner)
import Data.Attoparsec.Text hiding (atEnd, take)


import Data.Void
import Data.Function
import Data.Bifunctor
import Data.Foldable (Foldable(toList), msum)
import Debug.Trace

import Data.Vector.Unboxed.Mutable (STVector)
import qualified Data.Vector.Unboxed.Mutable as MVec
import Control.Monad.ST
import Data.STRef
import Control.Monad
import qualified Data.List as List
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO (Maybe Double, Maybe Double)
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many1 (read . pure <$> digit)

------------ TYPES ------------
type Input = [Int]

runGame :: [Int] -> Int -> [Int]
runGame xs iters = runST $ do
  let xs'   = fmap (subtract 1) xs
      nexts = zip xs' (tail xs' ++ [head xs'])
      max   = maximum xs

  vec   <- Vec.thaw $ Vec.replicate (Prelude.length xs) 0 Vec.// nexts
  focus <- newSTRef (head xs')

  let next = MVec.read vec
  
  replicateM_ iters $ do
    pre   <- readSTRef focus
    [post,seg3,seg2,seg1,pre] <-
      foldM (\l f -> (:l) <$> f (head l)) [pre] (replicate 4 next) 

    let inSeg = (`elem` [seg1,seg2,seg3])

    -- Snip out the segment
    MVec.write vec pre post

    let Just destPre = List.find (not.inSeg) $ (`mod` max) <$> [pre-1,pre-2..]
    destPost <- next destPre

    -- Insert the segment in the new spot
    MVec.write vec destPre seg1
    MVec.write vec seg3 destPost
    
    -- Move the focus to the next element
    writeSTRef focus post

  Prelude.take (length xs - 1) . tail . fmap (+1) . (\v -> iterate (v Vec.!) 0) 
    <$> Vec.freeze vec

------------ PART A ------------
partA :: Input -> [Int]
partA ls = runGame ls 100

instance {-# OVERLAPS #-} Show [Int] where show = concatMap show

------------ PART B ------------
partB :: Input -> Int
partB cups = let
  m = maximum cups
  cups' = cups ++ [(m+1)..1_000_000]
  res = runGame cups' 10_000_000 
  in product $ take 2 res