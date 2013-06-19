module Language.Automata.Finite.Deterministic
  ( DFA
  , fromList
  , toList
  , eval
  , relabel
  ) where

import Prelude hiding (lookup)
import Data.Maybe (fromMaybe)
import Data.List (foldl')
import Data.Map (Map, fromListWith, union, singleton,
                 elems, lookup, empty, unionWith)
import qualified Data.Map as M

import Control.Arrow (first, second)
import Control.Monad.State (gets, modify, evalState)

data State a t
  = Stuck
  | State a Bool (Map t a)
  deriving (Eq, Show, Read)

data DFA s t
  = DFA { table :: Map s (State s t)
        , start :: State s t }
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

    combine (State a x m) (State _ _ n) = State a x (m `union` n)
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
    State start' _ _    = start m
    states              = elems (table m)
    edges (State a _ t) = map (brand a) (M.toList t)
    brand a (t, b)      = (a, t, b)
    accept as           = [ a | State a x _ <- as, x ]

step :: (Ord a, Ord t) => DFA a t -> State a t -> t -> State a t
step _ Stuck _          = Stuck
step m (State _ _ ts) t = case lookup t ts of
  Nothing -> Stuck
  Just s  -> fromMaybe Stuck (lookup s (table m))

-- Run the simulation, producing True if the machine accepted the input
-- or False otherwise.
--
eval :: (Ord a, Ord t) => DFA a t -> [t] -> Bool
eval m = accept . foldl' (step m) (start m)
  where
    accept Stuck         = False
    accept (State _ x _) = x

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
