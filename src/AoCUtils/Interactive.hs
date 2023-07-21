module AoCUtils.Interactive (
  aocMain,
  printSolution,
  printSolutions
) where

import           AoCUtils.Days       (Solver, readInput, showSolution)
import           Control.Applicative ((<**>), (<|>))
import           Data.HashMap.Lazy   (HashMap)
import qualified Data.HashMap.Lazy   as HM
import           Data.Time           (diffUTCTime, getCurrentTime)
import           Options.Applicative (Parser, ParserInfo, argument, auto,
                                      execParser, fullDesc, header, help,
                                      helper, info, metavar, progDesc, short,
                                      strOption)

aocMain :: [Solver] -> HashMap String (IO ()) -> IO ()
aocMain solvers graphicalMap = do
  options <- execParser opts
  case options of
    Textual day     -> printSolutions solvers [day]
    Graphical visId -> displayGraphical graphicalMap visId
    TextualAll      -> printSolutions solvers [1 .. length solvers]

-- Opts parsing

data ProgramOpts = Textual Int | Graphical String | TextualAll

opts :: ParserInfo ProgramOpts
opts = info (optsParser <**> helper)
  (fullDesc
  <> progDesc "Solve a day or show visualization"
  <> header "AoC Solver" )

optsParser :: Parser ProgramOpts
optsParser = textualParser <|> graphicalParser <|> allParser

textualParser :: Parser ProgramOpts
textualParser = Textual <$> argument auto (
  metavar "DAY"
  <> help "Day to run.")

graphicalParser :: Parser ProgramOpts
graphicalParser = Graphical <$> strOption
  (short 'g'
  <> metavar "VISUALIZATION"
  <> help "Visualization to run")

allParser :: Parser ProgramOpts
allParser = pure TextualAll

-- Textual interface

printSolutions :: [Solver] -> [Int] -> IO ()
printSolutions solvers days = do
  startTime <- getCurrentTime
  mapM_ (printSolution solvers) days
  stopTime <- getCurrentTime
  putStrLn ""
  putStrLn dashLine
  putStrLn $ "\nAll puzzles solved in " ++ show (diffUTCTime stopTime startTime)

printSolution :: [Solver] -> Int -> IO ()
printSolution solvers day = do
  startTime <- getCurrentTime
  putStrLn dashLine
  putStrLn $ "Day " ++ show day
  input <- readInput day
  let solution = (solvers !! (day - 1)) input
  putStrLn $ showSolution solution
  stopTime <- getCurrentTime
  putStrLn $ "\nSolved in " ++ show (diffUTCTime stopTime startTime)

-- Visualizations

displayGraphical :: HashMap String (IO ()) -> String -> IO ()
displayGraphical graphicalMap str = case HM.lookup str graphicalMap of
  Nothing  -> error $ "No visualization mapped to identifier: " ++ str
  Just vis -> vis

-- Utils ----------------------------

dashLine :: String
dashLine = "---------------------------------"
