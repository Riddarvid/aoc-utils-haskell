module AoCUtils.Interactive (
  aocMain,
  printSolution,
  printSolutions
) where

import           AoCUtils.Config     (Config (cfgCustomEntrypoint, cfgInputDir, cfgSolvers))
import           AoCUtils.Days       (readInput, showSolution)
import           Control.Applicative ((<**>), (<|>))
import           Data.Time           (diffUTCTime, getCurrentTime)
import           Options.Applicative (Parser, ParserInfo, argument, auto,
                                      execParser, fullDesc, header, help,
                                      helper, info, long, metavar, progDesc,
                                      short, switch)

aocMain :: Config -> IO ()
aocMain cfg = do
  options <- execParser opts
  case options of
    Textual day -> printSolutions cfg [day]
    TextualAll  -> printSolutions cfg [1 .. length $ cfgSolvers cfg]
    Custom day  -> runCustom cfg day

-- Opts parsing

data ProgramOpts = Textual Int | TextualAll | Custom Int

opts :: ParserInfo ProgramOpts
opts = info (optsParser <**> helper)
  (fullDesc
  <> progDesc "Solve a day or show visualization"
  <> header "AoC Solver" )

optsParser :: Parser ProgramOpts
optsParser = withDayParser <|> noDayParser

withDayParser :: Parser ProgramOpts
withDayParser = toProgramOpts <$>
  argument auto (
    metavar "DAY"
    <> help "Day to run") <*>
  switch (
    short 'e'
    <> long "entrypoint"
    <> help "Use custom entrypoint")
  where
    toProgramOpts day entrypoint = if entrypoint then Custom day else Textual day

noDayParser :: Parser ProgramOpts
noDayParser = pure TextualAll


-- Textual interface

printSolutions :: Config -> [Int] -> IO ()
printSolutions cfg days = do
  startTime <- getCurrentTime
  mapM_ (printSolution cfg) days
  stopTime <- getCurrentTime
  putStrLn ""
  putStrLn dashLine
  putStrLn $ "\nAll puzzles solved in " ++ show (diffUTCTime stopTime startTime)

printSolution :: Config -> Int -> IO ()
printSolution cfg day = do
  startTime <- getCurrentTime
  putStrLn dashLine
  putStrLn $ "Day " ++ show day
  input <- readInput (cfgInputDir cfg) day
  let solution = (cfgSolvers cfg !! (day - 1)) input
  putStrLn $ showSolution solution
  stopTime <- getCurrentTime
  putStrLn $ "\nSolved in " ++ show (diffUTCTime stopTime startTime)

runCustom :: Config -> Int -> IO ()
runCustom cfg day = do
  input <- readInput (cfgInputDir cfg) day
  cfgCustomEntrypoint cfg input

-- Utils ----------------------------

dashLine :: String
dashLine = "---------------------------------"
