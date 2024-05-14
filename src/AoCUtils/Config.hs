module AoCUtils.Config (
  Config(..),
  mkConfig
) where
import           AoCUtils.Days (Solver)

data Config = Config {
  cfgSolvers        :: [Solver],
  cfgInputDir       :: FilePath,
  cfgResultsDir     :: FilePath
}

mkConfig :: [Solver] -> FilePath -> FilePath -> Config
mkConfig solvers inputDir resultsDir = Config {
  cfgSolvers = solvers,
  cfgInputDir = inputDir,
  cfgResultsDir = resultsDir
  }
