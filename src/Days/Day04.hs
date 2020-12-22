{-# LANGUAGE TypeApplications, DuplicateRecordFields #-}
module Days.Day04 (runDay, ) where

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

import qualified Program.RunDay as R (runDay, DayRunner)
import Data.Attoparsec.Text hiding (sepBy, manyTill, choice,count)
import Data.Void
import Data.Maybe as Maybe
import Control.Applicative.Combinators
import Debug.Trace
import Data.Functor
import Data.Char
import Data.Text hiding (length,map, count)
import qualified Data.Text as T
import Data.Either (rights)
import Control.Monad
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO (Maybe Double, Maybe Double)
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = 
  do
    let pp = do
          let pair = do
                a <- many letter
                char ':'
                b <- many (satisfy $ not . isSpace)
                return (a,pack b)

          pairs <- pair `sepBy` space
          let dict = Map.fromList pairs

          return $ Passport
            <$> (Map.lookup "ecl" dict)
            <*> (Map.lookup "byr" dict)
            <*> (Map.lookup "iyr" dict)
            <*> (Map.lookup "eyr" dict)
            <*> (Map.lookup "hgt" dict)
            <*> (Map.lookup "hcl" dict)
            <*> (Map.lookup "pid" dict)
            <*> (Just $ Map.lookup "cid" dict)

    let boundary = endOfLine >> endOfLine

    passports <- pp `sepBy` boundary

    return $ Maybe.catMaybes passports

------------ TYPES ------------
type Input = [Passport]

data Passport = Passport 
  { eyeColor :: Text
  , birthYear :: Text
  , issueYear :: Text
  , expirationYear :: Text
  , height :: Text
  , hairColor :: Text
  , passportId :: Text
  , countryId :: Maybe Text
  }
  deriving Show

data Passport' = Passport'
  { eyeColor :: Text
  , birthYear :: Int
  , issueYear :: Int
  , expirationYear :: Int
  , height :: Height
  , hairColor :: Text
  , passportId :: Integer
  , countryId :: Maybe Text
  }
  deriving Show

data Height = CM Integer | IN Integer
  deriving Show

------------ PART A ------------
partA :: Input -> Int
partA = length

------------ PART B ------------
partB :: Input -> Int
partB = length . rights . map parseport

parseport :: Passport -> Either String Passport'
parseport Passport {..} = 
  let p t psr = parseOnly (psr <* endOfInput) t
  in do
    birthYear <- p birthYear $ do
      byr <- decimal
      guard $ byr >= 1920 && byr <= 2002
      return byr

    issueYear <- p issueYear $ do
      iyr <- decimal
      guard $ iyr >= 2010 && iyr <= 2020
      return iyr

    expirationYear <- p expirationYear $ do
      eyr <- decimal
      guard $ eyr >= 2020 && eyr <= 2030
      return eyr

    height <- p height $ do
      h <- decimal
      let
        cms = do
          string "cm"
          guard $ h >= 150 && h <= 193
          return $ CM h
        ins = do
          string "in"
          guard $ h >= 59 && h <= 76
          return $ IN h
      cms <|> ins

    hairColor <- p hairColor $ do
      char '#' 
      pack <$> count 6 (satisfy (inClass $ "0-9a-f"))
      
    eyeColor <- p eyeColor 
      $ choice $ map string ["amb","blu","brn","gry","grn","hzl","oth"]

    passportId <- p passportId $ do
      pid <- count 9 digit
      return $ read pid
      
    return Passport'{..}