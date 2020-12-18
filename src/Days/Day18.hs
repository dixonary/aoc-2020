module Days.Day18 (runDay) where

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
import Control.Monad.Combinators (between)
import Data.Functor
import Control.Applicative
import Data.Function
import Data.Attoparsec.Combinator (lookAhead)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (,) 
                <$> lookAhead (expr `sepBy1` endOfLine) 
                <*>           (expr' `sepBy1` endOfLine) 
  where
    term x = decimal <|> between (char '(') (char ')') x
    
    expr = do
      first <- term expr
      rest <- many $ (string " + " $> (+) <|> string " * " $> (*)) <*> term expr
      return $ foldl' (&) first rest

    clause' = sum     <$> term expr' `sepBy1` string " + "
    expr'   = product <$> clause'    `sepBy1` string " * "

------------ TYPES ------------
type Input = ([Integer],[Integer])

------------ PART A ------------
partA :: Input -> Integer
partA = sum . fst

------------ PART B ------------
partB :: Input -> Integer
partB = sum . snd
