module Days.Day22 (runDay) where

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

import Data.Sequence ((|>), (><), Seq((:<|)))
import qualified Data.Sequence as Seq (take, null, length,  fromList, (|>), Seq((:<|), Empty) )
import Data.Foldable (toList)

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
import Control.Applicative.Combinators
import Control.Monad.State
import Data.Bool
import Debug.Trace
import Control.Monad.Loops
import Control.Arrow
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  string "Player 1:" >> endOfLine 
  deck1 <- decimal `sepEndBy1` endOfLine
  endOfLine 
  string "Player 2:" >> endOfLine 
  deck2 <- decimal `sepEndBy1` endOfLine
  return (Seq.fromList deck1, Seq.fromList deck2)


------------ TYPES ------------
type Input = (Deck,Deck)

type Deck = Seq Card
type Card = Int

score :: (Deck,Deck) -> Int 
score = uncurry max . U.both (sum . zipWith (*) [1..] . reverse . toList)

done :: (Deck,Deck) -> Bool
done = uncurry (||) . U.both null

------------ PART A ------------
partA :: Input -> Int
partA = score . combat

combat :: (Deck,Deck) -> (Deck,Deck)
combat = let 
  runRound (c1 :<| d1, c2 :<| d2) = if c1 > c2 
    then (d1 |> c1 |> c2, d2) 
    else (d1, d2 |> c2 |> c1)
  in until done runRound

------------ PART B ------------
partB :: Input -> Int
partB = score . runRecCombat

runRecCombat :: (Deck,Deck) -> (Deck,Deck)
runRecCombat decks = evalState (recCombat decks) mempty

recCombat :: (Deck,Deck) -> State (Set String) (Deck,Deck)
recCombat = let

  runRound decks = do
    let 
      deckStr = show decks
      -- Draw the top card from each deck
      (c1 :<| d1', c2 :<| d2') = decks
      
      -- Determine who won the round
      win1 = if length d1' >= c1 && length d2' >= c2 
        then not.null $ fst $ runRecCombat (Seq.take c1 d1',Seq.take c2 d2')  
        else c1 >= c2

    seen <- gets (deckStr `Set.member`)
    modify (Set.insert deckStr)

    return $ if
      | seen      -> (uncurry (<>) decks, Seq.Empty) -- Quit on infinite loop
      | win1      -> (d1' |> c1 |> c2, d2')
      | otherwise -> (d1', d2' |> c2 |> c1)

  in iterateUntilM done runRound