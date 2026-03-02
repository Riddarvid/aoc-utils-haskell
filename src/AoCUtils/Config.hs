module AoCUtils.Config (
  Config(..),
  mkConfig
) where
import           AoCUtils.Days (Solver)

data Config = Config {
  cfgSolvers          :: [Solver],
  cfgInputDir         :: FilePath,
  cfgResultsDir       :: FilePath,
  cfgCustomEntrypoint :: String -> IO ()
}

mkConfig :: [Solver] -> FilePath -> FilePath -> (String -> IO ()) -> Config
mkConfig solvers inputDir resultsDir customEntrypoint = Config {
  cfgSolvers = solvers,
  cfgInputDir = inputDir,
  cfgResultsDir = resultsDir,
  cfgCustomEntrypoint = customEntrypoint
  }
