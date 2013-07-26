module Language.Automata.Finite.Deterministic
  ( DFA
  , fromList
  , toList
  , toGraph
  , member
  , elems
  , union
  , difference
  , intersection
  , concat
  , complement
  , kleene
  , isSubsetOf
  , relabel
  ) where

import Prelude hiding (lookup, concat)
import Data.Maybe (fromMaybe)
import Data.List (foldl')
import Data.Graph.Inductive (Gr, mkGraph)
import Data.Map (Map, fromListWith, singleton,
                 lookup, empty, unionWith)
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Arrow (first, second)
import Control.Monad.State (gets, modify, evalState)

data State a t
  = Stuck
  | State a Bool (Map t a)
  deriving (Eq, Show, Read, Ord)

data DFA a t
  = DFA { table :: Map a (State a t)
        , start :: State a t }
  deriving (Eq, Show, Read)

-- First argument is a list of tuples (from, token, to) which specifies
-- when the machine is at state `from` and reads `token`, the resulting
-- state is `to`. Because this machine is deterministic, ... TODO
--
-- The second argument is a list of accept states. If the machine halts
-- on one of these states, the input is "accepted" and eval will return
-- True.
--
-- The last argument is a single start state.
--
fromList :: (Ord a, Ord t) => [(a, t, a)] -> [a] -> a -> DFA a t
fromList edges accept k = DFA table' start'
  where
    table' = unionWith combine incoming outgoing
    start' = fromMaybe (State k (k `elem` accept) empty) (lookup k table')

    combine (State a x m) (State _ _ n) = State a x (m `M.union` n)
    incoming = M.fromList (map fromAccept accept)
    outgoing = fromListWith combine (map fromEdge edges)

    fromAccept s       = (s, State s True empty)
    fromEdge (a, t, b) = (a, State a (a `elem` accept) (singleton t b))

-- Produce a representation of the given DFA as a list of edges, accept
-- states, and the start state
--
toList :: DFA a t -> ([(a, t, a)], [a], a)
toList m = (edges =<< states, accept states, start')
  where
    State start' _ _     = start m
    states               = M.elems (table m)
    edges (State a _ ts) = map (brand a) (M.toList ts)
    brand a (t, b)       = (a, t, b)
    accept as            = [ a | State a x _ <- as, x ]

toGraph :: Ord a => DFA a t -> Gr a t
toGraph m = mkGraph states (edges =<< M.toList (table m))
  where
    states = zip [0..] (M.keys (table m))
    table' = M.fromList (map (\(n, a) -> (a, n)) states)
    node a = fromMaybe 0 (lookup a table')
    edges (a, State _ _ ts) = convert (node a) (M.toList ts)
    convert a = map (\(t, b) -> (a, node b, t))

-- Run the simulation, producing True if the machine accepted the input
-- or False otherwise.
--
member :: (Ord a, Ord t) => [t] -> DFA a t -> Bool
member ts m = accept (eval ts) 
  where
    eval = foldl' step (start m)

    accept Stuck         = False
    accept (State _ x _) = x

    -- step :: (Ord a, Ord t) => State a t -> t -> State a t
    step Stuck _          = Stuck
    step (State _ _ tx) t = case lookup t tx of
      Nothing -> Stuck
      Just s  -> fromMaybe Stuck (lookup s (table m))

-- Enumerate the valid strings accepted by the given DFA. The results
-- are produced from a breadth-first traversal of the state transition
-- graph.
--
-- Note: strings in the output may be repeated.
--
elems :: (Ord a, Ord t) => DFA a t -> [[t]]
elems m = walk (S.singleton ([], start m))
  where
    walk ss
      | stuck ss  = []
      | otherwise = accept ss ++ walk (S.fold step S.empty ss)

    stuck = S.null . S.filter op
      where
        op (_, Stuck) = False
        op _          = True

    accept = S.fold op []
      where
        op (x, State _ True _) xs = reverse (x:xs)
        op _                   xs = xs

    step (_, Stuck)         ss = ss
    step (xs, State _ _ ts) ss = M.foldWithKey op ss ts
      where
        op x n = S.insert (x:xs, fromMaybe Stuck (lookup n (table m)))

union :: DFA a t -> DFA a t -> DFA a t
union = undefined

difference :: DFA a t -> DFA a t -> DFA a t
difference = undefined

intersection :: DFA a t -> DFA a t -> DFA a t
intersection = undefined

concat :: DFA a t -> DFA a t -> DFA a t
concat = undefined

complement :: DFA a t -> DFA a t
complement = undefined

kleene :: DFA a t -> DFA a t
kleene = undefined

isSubsetOf :: DFA a t -> DFA a t -> DFA a t
isSubsetOf = undefined

-- Replace each distinct state label (of any type 'a') with a distinct label
-- of type 'b'. Type 'b' can be any for which minBound and succ are defined.
relabel :: (Ord a, Ord t, Ord b, Bounded b, Enum b) => DFA a t -> DFA b t
relabel m
  = let (ts', as', ss') = evalState rebuild (minBound, M.empty)
     in fromList ts' as' ss'
  where
    rebuild = do
      let (ts, as, ss) = toList m
      ss' <- index ss
      as' <- mapM index as
      ts' <- mapM edges ts
      return (ts', as', ss')

    fresh = do
      n <- gets fst
      modify (first succ)
      return n

    store s = do
      n <- fresh
      modify (second (M.insert s n))
      return n

    index s = do
      k <- gets snd
      maybe (store s) return (M.lookup s k)

    edges (a, t, b) = do
      a' <- index a
      b' <- index b
      return (a', t, b')
