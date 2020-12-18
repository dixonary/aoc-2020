module Days.Day18 (runDay) where

{- ORMOLU_DISABLE -}
import Data.Function ( (&) )
import Data.Functor ( ($>) )
import Control.Applicative
import Data.List ( foldl' )
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator (lookAhead)
import Control.Monad.Combinators (between)
import qualified Program.RunDay as R (runDay)
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
