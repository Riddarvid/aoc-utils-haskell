{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AoCUtils.Dijkstra (
  PredMap,
  dijkstraFull,
  dijkstraTarget
) where

import           Data.Foldable (find)
import           Data.Heap     (Entry (Entry), Heap, viewMin)
import qualified Data.Heap     as Heap
import           Data.Map      (Map)
import qualified Data.Map      as Map
import           Data.Set      (Set)
import qualified Data.Set      as Set
import           Prelude       hiding (pred)

type PredMap a = Map a ([a], Int)

data DijkstraCost a = DC [a] Int

instance Eq (DijkstraCost a) where
  (==) :: DijkstraCost a -> DijkstraCost a -> Bool
  (DC _ p1) == (DC _ p2) = p1 == p2

instance Ord (DijkstraCost a) where
  compare :: DijkstraCost a -> DijkstraCost a -> Ordering
  compare (DC _ p1) (DC _ p2) = compare p1 p2

type DijkstraEntry a = Entry (DijkstraCost a) a

type DijkstraHeap a = Heap (DijkstraEntry a)

dijkstraFull :: Ord a => (a -> [(a, Int)]) -> a -> PredMap a
dijkstraFull neighborsOf start =
  dijkstraFull' neighborsOf (Heap.singleton (Entry (DC [] 0) start)) Set.empty Map.empty

dijkstraFull' :: forall a. Ord a => (a -> [(a, Int)]) ->
  DijkstraHeap a -> Set a -> PredMap a -> PredMap a
dijkstraFull' neighborsOf = go
  where
    go :: DijkstraHeap a -> Set a -> PredMap a -> PredMap a
    go frontier expanded predMap = case viewMin frontier of
      Nothing   -> predMap
      Just (dnode@(Entry (DC pred cost) node), frontier') -> go frontier'' expanded' predMap'
        where
          predMap' = Map.insert node (pred, cost) predMap
          expanded' = Set.insert node expanded
          neighbors = neighborNodesOf neighborsOf dnode
          neighbors' = filter (\(Entry _ n) -> not $ Set.member n expanded) neighbors
          frontier'' = foldr updateFrontier frontier' neighbors'

dijkstraTarget :: Ord a => (a -> [(a, Int)]) -> (a -> Bool) -> a -> PredMap a
dijkstraTarget neighborsOf isGoal start =
  dijkstraTarget' neighborsOf isGoal (Heap.singleton (Entry (DC [] 0) start)) Set.empty Map.empty

dijkstraTarget' :: forall a. Ord a => (a -> [(a, Int)]) -> (a -> Bool) ->
  DijkstraHeap a -> Set a -> PredMap a -> PredMap a
dijkstraTarget' neighborsOf isGoal = go
  where
    go :: DijkstraHeap a -> Set a -> PredMap a -> PredMap a
    go frontier expanded predMap = case viewMin frontier of
      Nothing   -> error "No path to goal"
      Just (dnode@(Entry (DC pred cost) node), frontier') -> if isGoal node
        then predMap'
        else go frontier'' expanded' predMap'
        where
          predMap' = Map.insert node (pred, cost) predMap
          expanded' = Set.insert node expanded
          neighbors = neighborNodesOf neighborsOf dnode
          neighbors' = filter (\(Entry _ n) -> not $ Set.member n expanded) neighbors
          frontier'' = foldr updateFrontier frontier' neighbors'

--------------- Common -----------------

updateFrontier :: Eq a => DijkstraEntry a -> DijkstraHeap a -> DijkstraHeap a
updateFrontier entry@(Entry (DC newPreds newCost) node) heap =
  case find (\(Entry _ node') -> node == node') heap of
    Nothing -> Heap.insert entry heap
    Just (Entry (DC _ oldCost) _) -> case compare newCost oldCost of
      LT -> Heap.map setCost heap
      EQ -> Heap.map appendCost heap
      GT -> heap
  where
    setCost (Entry oldCost node')
      | node == node' = Entry (DC newPreds newCost) node'
      | otherwise = Entry oldCost node'
    appendCost (Entry oldCost@(DC oldPreds _) node')
      | node == node' = Entry (DC (newPreds ++ oldPreds) newCost) node'
      | otherwise = Entry oldCost node'

neighborNodesOf :: (a -> [(a, Int)]) -> DijkstraEntry a -> [DijkstraEntry a]
neighborNodesOf neighborsOf (Entry (DC _ cost) node) =
  map (\(neighbor, moveCost) -> Entry (DC [node] (cost + moveCost)) neighbor) $
  neighborsOf node
