module AoCUtils.Days (
  readInput,
  readResults,
  Input,
  ExpectedResult,
  Solver,
  showSolution
) where
import           Control.Monad.Cont (MonadIO (liftIO))

type Solution = (String, String)

type Solver = String -> Solution

showSolution :: Solution -> String
showSolution (part1, part2) = "Part1:\n" ++ part1 ++ "\n\n" ++ "Part2:\n" ++ part2

type Input = String

readInput :: (MonadIO m) => Int -> m Input
readInput day = liftIO $ readFile ("input/input" ++ show day ++ ".txt")

type ExpectedResult = (Maybe String, Maybe String)

readResults :: (MonadIO m) => Int -> m ExpectedResult
readResults day = do
  contents <- liftIO $ readFile ("expected-results/result" ++ show day ++ ".txt")
  case lines contents of
    [r1, r2] -> return (checkNA r1, checkNA r2)
    _        -> error "Expected two lines, N/A if not applicable."
  where
    checkNA :: String -> Maybe String
    checkNA "N/A" = Nothing
    checkNA s     = Just s
