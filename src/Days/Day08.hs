module Days.Day08 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text hiding (Result)
import Data.Functor (($>))
import Data.Function (on)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = let
  instr = do
    op <- choice ["acc"$>Acc,"nop"$>Nop,"jmp"$>Jmp]
    space
    dx <- signed decimal
    return $ op dx
  in Vec.fromList <$> instr `sepBy1` endOfLine

------------ TYPES ------------
type Input = Vector Instruction

data Instruction
    = Acc Integer
    | Jmp Integer
    | Nop Integer
    deriving (Show)

toggle :: Instruction -> Instruction
toggle (Acc n) = Acc n
toggle (Jmp n) = Nop n
toggle (Nop n) = Jmp n

data Memory = Memory {
  ptr :: Int,
  acc :: Integer,
  seen :: Set Int
}
instance Eq Memory where
  (==) = (==) `on` ptr

data Result = Loops Integer | Terminates Integer
  deriving (Show)
terminates :: Result -> Bool
terminates (Loops _) = False
terminates (Terminates _) = True

------------ PART A ------------
partA :: Input -> Integer
partA program = (\(Loops x) -> x) $ reduce program $ Memory 0 0 mempty

reduce :: Vector Instruction -> Memory -> Result
reduce program = reduce'
  where 
    reduce' Memory{..} = if 
        | ptr `elem` seen       -> Loops acc
        | ptr == length program -> Terminates acc
        | otherwise             -> 
          let seen' = Set.insert ptr seen
          in case program Vec.! ptr of
            Acc n -> reduce' $ Memory (ptr +              1) (acc + n)  seen' 
            Jmp n -> reduce' $ Memory (ptr + fromIntegral n) acc        seen'
            Nop _ -> reduce' $ Memory (ptr +              1) acc        seen'

------------ PART B ------------
partB :: Input -> Integer
partB program = (\(Just (Terminates x)) -> x) $ find terminates 
    [ reduce program' $ Memory 0 0 mempty 
    | n <- [0..Vec.length program]
    , let program' = program Vec.// [(n,toggle $ program Vec.! n)]
    ] 