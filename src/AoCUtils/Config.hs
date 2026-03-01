module AoCUtils.Config (
  Config(..),
  mkConfig
) where
import           AoCUtils.Days (Solver)

data Config = Config {
  cfgSolvers        :: [Solver],
  cfgInputDir       :: FilePath,
  cfgResultsDir     :: FilePath,
  cfgVisualizations :: String -> IO ()
}

mkConfig :: [Solver] -> FilePath -> FilePath -> (String -> IO ()) -> Config
mkConfig solvers inputDir resultsDir vis = Config {
  cfgSolvers = solvers,
  cfgInputDir = inputDir,
  cfgResultsDir = resultsDir,
  cfgVisualizations = vis
  }
