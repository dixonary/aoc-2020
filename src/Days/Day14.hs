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
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  let
    memset  = do
      ix <- string "mem[" *> decimal <* string "] = "
      val <- decimal
      return $ \(mem,m@(m0,m1)) -> (Map.insert ix ((val .|. m1) .&. m0) mem,m)

    mask = do
      string "mask = "
      ls <- many1 $ 
          char '1' $> Just 1 <|> char '0' $> Just 0 <|> char 'X' $> Nothing
      let 
        take1 (Just 1) = 1; take1 _ = 0
        take0 (Just 0) = 0; take0 _ = 1
        num = foldl' (\acc x -> acc*2+ x) 0
      return $ \(mem,_) -> (mem,(num $ take1 <$> ls, num $ take0 <$> ls))

  lines <- (memset <|> mask) `sepBy1` endOfLine
  return $ fst $ foldl' (&) (Map.empty,(0,0)) lines 

------------ TYPES ------------
type Input = Map Integer Integer

------------ PART A ------------
partA :: Input -> Integer
partA = sum . Map.elems

------------ PART B ------------
partB :: Input -> Void
partB = error "Not implemented yet!"
