{-# LANGUAGE TypeApplications #-}
module Program.RunDay (runDay, DayRunner) where

import Control.Exception (SomeException, catch)
import Control.Monad.Except
import Data.Attoparsec.Text
import Data.Text (pack)
import System.Directory (doesFileExist)
import System.CPUTime (getCPUTime)
import Data.Functor
import System.Console.ANSI
import Data.Time
import Text.Printf

type DayRunner = Bool -> String -> IO (Maybe Double, Maybe Double)

runDay :: (Show a, Show b, Show i) => Parser i -> (i -> a) -> (i -> b) -> DayRunner
runDay inputParser partA partB verbose inputFile = do
  input <- runExceptT $ do
    inputFileExists <- liftIO $ doesFileExist inputFile
    fileContents <-
      if inputFileExists
        then liftIO $ readFile inputFile
        else throwError $ "I couldn't read the input! I was expecting it to be at " ++ inputFile
    case parseOnly inputParser . pack $ fileContents of
      Left e -> throwError $ "Parser failed to read input. Error " ++ e
      Right i -> do
        when verbose $ do
          liftIO $ putStrLn "Parser output:"
          liftIO $ print i
        return i

  case input of
    Left x -> putStrLn x >> return (Nothing,Nothing)
    Right i -> do
      setSGR [SetColor Foreground Vivid Blue]
      putStrLn "Part A:"
      startA <- getCurrentTime
      setSGR [SetDefaultColor Foreground]
      successA <- catch (print (partA i) $> True) $ \m -> do
        setSGR [SetColor Foreground Vivid Red]
        putStrLn "Couldn't run Part A!"
        when verbose $ print @SomeException m
        return False

      mid <- getCurrentTime
      let timeA = realToFrac $ diffUTCTime mid startA
      when verbose $ putStrLn $ printf "(%.2fs)" timeA
      
      setSGR [SetColor Foreground Vivid Blue]
      putStrLn "Part B:"
      
      setSGR [SetDefaultColor Foreground]
      successB <- catch (print (partB i) $> True) $ \m -> do
        setSGR [SetColor Foreground Vivid Red]
        putStrLn "Couldn't run Part B!" 
        when verbose $ print @SomeException m
        return False

      endB <- getCurrentTime
      let timeB = realToFrac $ diffUTCTime endB mid
      when verbose $ putStrLn $ printf "(%.2fs)" timeB

      setSGR [SetDefaultColor Foreground]
      return $ (,)
        (if successA then Just timeA else Nothing)
        (if successB then Just timeB else Nothing)
