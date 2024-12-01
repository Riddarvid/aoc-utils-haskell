module AoCUtils.Days (
  readInput,
  readResults,
  Input,
  ExpectedResult,
  Solver,
  Solution,
  showSolution
) where
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           System.FilePath        ((</>))

type Solution = (String, String)

type Solver = String -> Solution

showSolution :: Solution -> String
showSolution (part1, part2) = "Part1:\n" ++ part1 ++ "\n\n" ++ "Part2:\n" ++ part2

type Input = String

readInput :: (MonadIO m) => FilePath -> Int -> m Input
readInput path day = liftIO $ readFile (path </> ("input" ++ show day ++ ".txt"))

type ExpectedResult = (Maybe String, Maybe String)

readResults :: (MonadIO m) => FilePath -> Int -> m ExpectedResult
readResults path day = do
  contents <- liftIO $ readFile (path </> ("result" ++ show day ++ ".txt"))
  case lines contents of
    [r1, r2] -> return (checkNA r1, checkNA r2)
    _        -> error "Expected two lines, N/A if not applicable."
  where
    checkNA :: String -> Maybe String
    checkNA "N/A" = Nothing
    checkNA s     = Just s
