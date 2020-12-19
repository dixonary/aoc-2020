module Days.Day19 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified Util.Util as U
import qualified Util.Parsers as U

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
import Control.Applicative
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Char (isAlpha)
import Debug.Trace (traceShow, trace, traceShowId)
import Data.Tuple (swap)
import Data.Bifunctor
import Data.Function
import Control.Arrow ((>>>))
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB


------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  let 
    rule :: Parser (Int, [Prod])
    rule = do
      i <- decimal 
      string ": "
      let val = (Left <$> (char '"' *> anyChar <* char '"'))
                <|>
                (Right <$> (decimal `sepBy1` char ' '))
      rhs <- val `sepBy1` string " | "
      return (i,rhs)

  rawRules <- Map.fromList <$> rule `sepBy1` endOfLine 
  endOfLine 
  endOfLine 

  let
    toCNF :: Grammar -> CNF
    toCNF = start                                  -- Remove start production
      >>> term                                     -- Replace non-singleton terms
      >>> bin                                      -- Remove triples, etc
      >>> del                                      -- Remove epsilons
      >>> unit                                     -- Remove singletons
      >>> Map.map (map (second (\[a,b] -> (a,b)))) -- Parse into CNF type

    -- These three are guaranteed not to happen for our input.
    start = id 
    term = id 
    del = id
    
    -- Rewrite nonterminal triples by pairs.
    bin m = foldl' rewriteBins m $ Map.assocs m
      where
        rewriteBins :: Map Int [Prod] -> (Int,[Prod]) -> Map Int [Prod]
        rewriteBins m (k,vs) = foldl' (rewriteBin k) m vs
    
        rewriteBin :: Int -> Map Int [Prod] -> Prod -> Map Int [Prod]
        rewriteBin k m = \case
          Right xs | length xs > 2 -> let
            unusedKey = maybe 0 fst (Map.lookupMax m) + 1

            -- Rewrite productions to make a chain of pairs.
            fixProds (first, r:rs) n = (first, [r,n]):fixProds' n rs
              where
                fixProds' n [x,y] = [(n, [x,y])]
                fixProds' n (x:xs) = (n, [x,n+1]):fixProds' (n+1) xs

            addNewProd :: Map Int [Prod] ->(Int,[Int]) ->  Map Int [Prod]
            addNewProd m (k,v) = Map.insertWith (++) k [Right v] m

            in m
              & Map.adjust (delete (Right xs)) k
              & flip (foldl' addNewProd) (fixProds (k,xs) unusedKey)
          _ -> m

    -- Eliminate unit rules.
    unit m = foldl' removeUnits m $ Map.assocs m
      where
        removeUnits m (k,vs) = foldl' (removeUnit k) m vs

        removeUnit k m = \case
          Right [x] -> m
            & Map.adjust (delete (Right [x])) k
            & Map.adjust (++ (m Map.! x)) k
          _ -> m
 
  strs <- many1 (satisfy isAlpha) `sepBy1` endOfLine 

  -- Replace certain productions for part B
  let rawRules' = rawRules
        & Map.insert 8  [Right [42], Right [42,8]]
        & Map.insert 11 [Right [42,31], Right [42,11,31]]

  return (toCNF rawRules, toCNF rawRules', strs)

------------ TYPES ------------
type Input = (CNF, CNF, [String])

type Prod       = Either Char [Int]
type ProdCNF    = Either Char (Int,Int)
type Grammar    = Map Int [Prod]
type CNF        = Map Int [ProdCNF]
type InverseCNF = Map ProdCNF [Int]

collate :: Ord a => [(a,b)] -> Map a [b]
collate = Map.fromListWith (++) . map (second pure)

decollate :: Map a [b] -> [(a,b)]
decollate = concatMap (\(a,bs) -> (a,) <$> bs) . Map.assocs 

-- Map lookup which defaults to monoidal identity.
(!@) :: (Monoid a, Ord k) => Map k a -> k -> a
(!@) = flip (Map.findWithDefault mempty)


-- Given a grammar in normal form and a string, determine whether
-- the strong is generated by the grammar.
memberCNF :: CNF -> String -> Bool
memberCNF cnf str = let
  strLen = length str

  -- Map from terminal or pair of nonterminals onto the set of nonterminals
  -- which generate it.
  cnfInv :: InverseCNF
  cnfInv = collate $ map swap $ decollate cnf

  -- The valid parsers for substrings of length 1.
  base :: Map (Int,Int) [Int]
  base = Map.fromList $ zipWith (\i c -> ((1,i),cnfInv !@ Left c)) [0..] str

  -- Insert the values for substrings of length n.
  fillRow :: Map (Int,Int) [Int] -> Int -> Map (Int,Int) [Int]
  fillRow m len = Map.union m $ Map.fromList $ map getEntries [0..strLen - len]
    where
      -- Take all pairs (a,b) st a,b>1 and a+b == len.
      getEntries ix = ((len,ix),) $ concat 
        [ cnfInv !@ Right (someA, someB)
        | a <- [1..len-1]
        , someA <- m Map.! (a, ix)
        , someB <- m Map.! (len - a, ix + a)
        ]
      
  in 0 `elem` foldl' fillRow base [2..length str] Map.! (length str,0)


------------ PART A ------------
partA :: Input -> Int
partA (cnfA, _, strs) = length $ filter (memberCNF cnfA) strs 


------------ PART B ------------
partB :: Input -> Int
partB (_, cnfB, strs) = length $ filter (memberCNF cnfB) strs 
