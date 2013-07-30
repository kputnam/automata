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

data Mode
  = Accept
  | Reject
  deriving (Eq, Show, Read, Ord)

modeIf :: Bool -> Mode
modeIf True  = Accept
modeIf False = Reject

data State n t
  = Stuck
  | State n Mode (Map t n)
  deriving (Eq, Show, Read, Ord)

data DFA n t
  = DFA { table :: Map n (State n t)
        , start :: State n t }
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
fromList :: (Ord n, Ord t) => [(n, t, n)] -> [n] -> n -> DFA n t
fromList edges accept s = DFA table' start'
  where
    table' = unionWith combine incoming outgoing
    start' = fromMaybe (State s (modeIf $ s `elem` accept) empty) (lookup s table')

    combine (State n x tx) (State n' x' tx')
      | n == n' &&
        x == x'   = State n x (tx `M.union` tx')
      | otherwise = error "impossible"
    combine _ _   = error "impossible"

    incoming = M.fromList (map fromAccept accept)
    outgoing = fromListWith combine (map fromEdge edges)

    fromAccept n        = (n, State n Accept empty)
    fromEdge (n, t, tx) = (n, State n (modeIf $ n `elem` accept) (singleton t tx))

-- Produce a representation of the given DFA as a list of edges, accept
-- states, and the start state
--
toList :: DFA n t -> ([(n, t, n)], [n], n)
toList m = (edges =<< states, accept states, start')
  where
    State start' _ _     = start m
    states               = M.elems (table m)

    edges (State a _ tx) = map (brand a) (M.toList tx)
    edges _              = [] -- not possible

    brand n (t, n')      = (n, t, n')
    accept ss            = [ n | State n Accept _ <- ss ]

toGraph :: Ord a => DFA a t -> Gr a t
toGraph m = mkGraph states (edges =<< M.toList (table m))
  where
    states = zip [0..] (M.keys (table m))
    table' = M.fromList (map (\(k, n) -> (n, k)) states)
    node n = fromMaybe 0 (lookup n table')

    edges (n, State _ _ tx) = convert (node n) (M.toList tx)
    edges _                 = [] -- not possible

    convert n = map (\(t, n') -> (n, node n', t))

-- Run the simulation, producing True if the machine accepted the input
-- or False otherwise.
--
member :: (Ord n, Ord t) => [t] -> DFA n t -> Bool
member ts m = accept (eval ts) 
  where
    eval = foldl' step (start m)

    accept (State _ Accept _) = True
    accept _                  = False

    -- step :: (Ord a, Ord t) => State a t -> t -> State a t
    step Stuck _          = Stuck
    step (State _ _ tx) t = case lookup t tx of
      Nothing -> Stuck
      Just n  -> fromMaybe Stuck (lookup n (table m))

-- Enumerate the valid strings accepted by the given DFA. The results
-- are produced from a breadth-first traversal of the state transition
-- graph.
--
-- Note: strings in the output may be repeated.
--
elems :: (Ord n, Ord t) => DFA n t -> [[t]]
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
        op (t, State _ Accept _) ts = reverse (t:ts)
        op _                     ts = ts

    step (_, Stuck)         ss = ss
    step (ts, State _ _ tx) ss = M.foldWithKey op ss tx
      where
        op t n = S.insert (t:ts, fromMaybe Stuck (lookup n (table m)))

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
relabel :: (Ord n, Ord t, Ord m, Bounded m, Enum m) => DFA n t -> DFA m t
relabel m = relabelWithMin m minBound

relabelWithMin :: (Ord n, Ord t, Ord m, Enum m) => DFA n t -> m -> DFA m t
relabelWithMin m s
  = let (tx, as, ss) = evalState rebuild (s, M.empty)
     in fromList tx as ss
  where
    rebuild = do
      let (tx, as, ss) = toList m
      ss' <- indexOf ss
      as' <- mapM indexOf as
      tx' <- mapM edges tx
      return (tx', as', ss')

    fresh = do
      n <- gets fst
      modify (first succ)
      return n

    store n = do
      n' <- fresh
      modify (second (M.insert n n'))
      return n'

    indexOf n = do
      tx <- gets snd
      maybe (store n) return (M.lookup n tx)

    edges (a, t, b) = do
      a' <- indexOf a
      b' <- indexOf b
      return (a', t, b')
