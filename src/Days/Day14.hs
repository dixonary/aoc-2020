module Days.Day14 (runDay) where

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
import Data.Bits
import Control.Applicative
import Data.Functor
import Control.Monad (foldM)
import Data.Function
import Data.Int (Int64)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  let
    memset  = do
      i <- string "mem[" *> decimal <* string "] = "
      v <- decimal
      return $ Left (i,v)

    mask = do
      string "mask = "
      ls <- many1 $ 
          char '1' $> Just 1 <|> char '0' $> Just 0 <|> char 'X' $> Nothing
      let 
        take1 (Just 1) = 1; take1 _ = 0
        take0 (Just 0) = 0; take0 _ = 1
        num = foldl' (\acc x -> acc*2+ x) 0
      return $ Right (num $ take0 <$> ls, num $ take1 <$> ls)

  (memset <|> mask) `sepBy1` endOfLine

------------ TYPES ------------
type Input = [Either Memset Mask]

type Memset = (Int64,Int64)
type Mask   = (Int64,Int64)

------------ PART A ------------
partA :: Input -> Int64
partA = sum . Map.elems . fst . foldl' (&) (Map.empty,(0,0)) . fmap line  
  where
  -- Set new mask
  line (Right mask') (mem,_) = (mem,mask')
  
  -- Apply mask and set values in memory
  line (Left (i,v)) (mem,m@(m0,m1)) = (Map.insert i ((v .|. m1) .&. m0) mem, m)

------------ PART B ------------
partB :: Input -> Int64
partB = sum . Map.elems . fst . foldl' (&) (Map.empty,(0,0)) . fmap line
  where
  -- Set new mask
  line' (Right mask') (mem,_) = (mem,mask')

  -- Apply mask and set values in memory
  line' (Left (i,v)) (mem,m@(m0,m1)) = let

    -- All bits which are not 1 and not 0
    fBits = complement m1 .&. m0

    -- :1 all the 1 bits in the mask, then :0 all floating bits in the mask
    i' = (i .|. m1) .&. complement fBits

    -- Compute the nondeterministic sums of all floating bits, and add base addr
    floaters = sum <$> sequence [[0,2^i] | i <- [0..35], testBit fBits i]
    faddrs   = fmap (+i') floaters

    -- Set the value at all floated addresses
    in (foldl' (\m i -> Map.insert i v m) mem faddrs, m)