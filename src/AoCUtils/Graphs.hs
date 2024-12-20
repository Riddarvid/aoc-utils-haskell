{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module AoCUtils.Graphs (
  bfsPath,
  bfsExplore,
  Goal(..),
  BfsState(..)
) where

import           Control.Monad        (join, unless)
import           Control.Monad.Except (Except, MonadError (throwError),
                                       runExcept)
import           Control.Monad.Reader (MonadReader, ReaderT (runReaderT), asks)
import           Control.Monad.State  (MonadState, StateT (runStateT), gets,
                                       modify)
import           Data.Foldable        (toList)
import           Data.List            (find)
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Set             (Set)
import qualified Data.Set             as Set

-- Exported functions

data Goal n = GTarget n | GCond (n -> Bool) | GFull

bfsPath :: (Ord n, Foldable t) => n -> Goal n -> Adjacency t n -> Maybe (Path n)
bfsPath start goal adjacency = constructPath goal =<< endState
  where
    endState = execBFSM explore (mkEnv goal adjacency) (mkState start)

bfsExplore :: (Ord n, Foldable t) => n -> Goal n -> Adjacency t n -> Maybe (BfsState n)
bfsExplore start goal adjacency = execBFSM explore (mkEnv goal adjacency) (mkState start)

-- Internal functions

type Path n = [n]

type Adjacency t n = n -> t n

data BfsEnv t n = BFSE {
  bfsGoal      :: BfsMonad t n Bool,
  bfsAdjacency :: Adjacency t n
}

mkEnv :: (Ord n) => Goal n -> Adjacency t n -> BfsEnv t n
mkEnv goal adjacency = BFSE {
  bfsGoal = goal',
  bfsAdjacency = adjacency}
  where
    goal' = case goal of
      GTarget target -> mkGoalTarget target
      GCond cond     -> mkGoalCond cond
      GFull          -> mkGoalFull

mkGoalTarget :: Ord n => n -> BfsMonad t n Bool
mkGoalTarget target = do
  lastLayer <- gets bfsLastLayer
  return $ Set.member target lastLayer

mkGoalCond :: (n -> Bool) -> BfsMonad t n Bool
mkGoalCond cond = do
  lastLayer <- gets bfsLastLayer
  return $ any cond lastLayer

mkGoalFull :: BfsMonad t n Bool
mkGoalFull = do
  lastLayer <- gets bfsLastLayer
  return $ Set.size lastLayer == 0

data BfsState n = BFSS {
  bfsPreMap    :: Map n (Maybe n),
  bfsLastLayer :: Set n,
  bfsNLayers   :: Integer
}

mkState :: n -> BfsState n
mkState start = BFSS {
  bfsPreMap = Map.singleton start Nothing,
  bfsLastLayer = Set.singleton start,
  bfsNLayers = 0}

newtype BfsMonad t n a = BFSM (ReaderT (BfsEnv t n) (StateT (BfsState n) (Except ())) a)
  deriving (Functor, Applicative, Monad,
  MonadReader (BfsEnv t n), MonadState (BfsState n), MonadError ())

runBFSM :: BfsMonad t n a -> BfsEnv t n -> BfsState n -> Maybe (a, BfsState n)
runBFSM (BFSM m) env st = case runExcept (runStateT (runReaderT m env) st) of
  Left ()   -> Nothing
  Right res -> Just res

evalBFSM :: BfsMonad t n a -> BfsEnv t n -> BfsState n -> Maybe a
evalBFSM m env st = fst <$> runBFSM m env st

execBFSM :: BfsMonad t n a -> BfsEnv t n -> BfsState n -> Maybe (BfsState n)
execBFSM m env st = snd <$> runBFSM m env st

explore :: (Ord n, Foldable t) => BfsMonad t n ()
explore = do
  done <- join $ asks bfsGoal
  unless done $ do
    lastLayer <- gets bfsLastLayer
    if Set.size lastLayer == 0
      then throwError ()
      else exploreNextLayer >> explore

constructPath :: (Ord n) => Goal n -> BfsState n -> Maybe (Path n)
constructPath goal st = do
  node <- case goal of
    GTarget target -> return target
    GCond cond     -> find cond (bfsLastLayer st)
    GFull          -> error "Can't construct path when goal is full."
  constructPath' (bfsPreMap st) node []


constructPath' :: (Ord n) => Map n (Maybe n) -> n -> Path n -> Maybe (Path n)
constructPath' preMap node path = do
  pre <- Map.lookup node preMap
  let path' = node : path
  case pre of
    Nothing   -> return path'
    Just pre' -> constructPath' preMap pre' path'

exploreNextLayer :: (Ord n, Foldable t) => BfsMonad t n ()
exploreNextLayer = do
  lastLayer <- gets bfsLastLayer
  currentLayer <- mconcat <$> mapM exploreNeighbors (Set.toList lastLayer)
  modify (\s -> s{bfsLastLayer = currentLayer})
  modify (\s -> s{bfsNLayers = 1 + bfsNLayers s})

exploreNeighbors :: (Ord n, Foldable t) => n -> BfsMonad t n (Set n)
exploreNeighbors node = do
  adjacency <- asks bfsAdjacency
  preMap <- gets bfsPreMap
  let neighbors = adjacency node
  let neighbors' = filter (\x -> not $ Map.member x preMap) $ toList neighbors
  mapM_ (addPredecessor node) neighbors'
  return $ Set.fromList neighbors'

addPredecessor :: (Ord n) => n -> n -> BfsMonad t n ()
addPredecessor pre node = modify (\s -> s{bfsPreMap = Map.insert node (Just pre) (bfsPreMap s)})
