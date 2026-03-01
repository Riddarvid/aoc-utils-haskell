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

mkConfig :: [Solver] -> FilePath -> FilePath -> Maybe (String -> IO ()) -> Config
mkConfig solvers inputDir resultsDir vis = Config {
  cfgSolvers = solvers,
  cfgInputDir = inputDir,
  cfgResultsDir = resultsDir,
  cfgVisualizations = vis'
  }
  where
    vis' = case vis of
      Nothing    -> error "No visualizations provided"
      Just vis'' -> vis''
