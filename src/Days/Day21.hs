module Days.Day21 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Control.Applicative.Combinators (sepEndBy1)
import Data.Function ( (&) )
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = let
    food = do
      ingredients <- many1 letter `sepEndBy1` char ' '
      allergens <- do
        string "(contains "
        allergens <- many1 letter `sepBy1` string ", "
        string ")"
        return allergens
      return (ingredients, allergens)
  in food `sepBy1` endOfLine

------------ TYPES ------------
type Input = [Food]
type Food = ([String], [String])

allergenMap :: [Food] -> Map String String
allergenMap foods = let
    allAllergens  = nub $ concat $ snd <$> foods

    -- Find those ingredients which might map to each allergen.
    candidates :: Map String [String]
    candidates = allAllergens & U.pairWith ings & Map.fromList
      where 
        ings a = foods   
               & filter ((a `elem`) . snd) -- Those foods with a in allergens
               & map (Set.fromList . fst)  -- Take their ingredient lists
               & foldl1' Set.intersection  -- And find the intersection
               & Set.toList                -- As a list

    computeAllergen cands = 
      case find (\(_,as) -> length as == 1) $ Map.assocs cands of
        Nothing    -> Nothing 
        Just (i,[a]) -> Just ((i,a), cands & Map.delete i & Map.map (delete a))

  in Map.fromList $ unfoldr computeAllergen candidates

------------ PART A ------------
partA :: Input -> Int
partA foods = let
    allIngredients = Set.fromList $ concat $ fst <$> foods
    allergens = allergenMap foods
    nonAllergens = allIngredients Set.\\ Set.fromList (Map.elems allergens)
  in length $ concatMap (filter (`Set.member` nonAllergens) . fst) foods

------------ PART B ------------
partB :: Input -> [String]
partB = Map.elems . allergenMap

-- This is very cursed
instance {-# OVERLAPS #-} Show [String] where
  show = intercalate ","