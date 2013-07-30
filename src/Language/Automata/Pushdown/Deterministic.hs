module Language.Automata.Pushdown.Deterministic
  ( DPDA
  , fromList
  , fromDFA
  , fromNFA
  , toList
  , member
  , elems
  -- , union
  -- , intersect
  -- , concat
  -- , complement
  -- , kleene
  -- , reverse
  , simplify
  , relabel
  ) where

import Prelude hiding (lookup)
import Data.Maybe (fromMaybe)
import Data.Map (Map, empty, lookup, union, unionWith, singleton, fromListWith)
import Data.List (foldl')
import qualified Data.Map as M

import Language.Automata.Mode
import qualified Language.Automata.Finite.Deterministic    as D
import qualified Language.Automata.Finite.Nondeterministic as N

type Stack s
  = [s]

data State n t s
  = Stuck
  | State n Mode (Map (Maybe t, s) (n, Stack s))
  deriving (Eq, Show, Read, Ord)

data DPDA n t s
  = DPDA { table :: Map n (State n t s)
         , start :: State n t s
         , top   :: s }
  deriving (Eq, Show, Read)

fromList :: (Ord n, Ord s, Ord t)
         => [(n, Maybe t, s, n, Stack s)] -> [n] -> n -> s -> DPDA n t s
fromList edges accept k = DPDA table' start'
  where
    table' = unionWith combine incoming outgoing
    start' = fromMaybe (State k (modeIf $ k `elem` accept) empty) (lookup k table')

    combine (State n x tx) (State n' x' tx')
      | n == n' &&
        x == x'   = State n x (tx `union` tx')
      | otherwise = error "impossible"
    combine _ _   = error "impossible"

    incoming = M.fromList (map fromAccept accept)
    outgoing = fromListWith combine (map fromEdge edges)

    fromAccept n = (n, State n Accept empty)
    fromEdge (n, t, s, n', ss) = (n, State n (modeIf $ n `elem` accept) (singleton (t, s) (n', ss)))

fromDFA :: (Ord n, Ord t) => D.DFA n t -> DPDA n t Char
fromDFA m = fromList ts' as s '$'
  where
    (ts, as, s) = D.toList m
    ts' = map (\(n, t, n') -> (n, Just t, '$', n', "$")) ts

fromNFA :: (Ord n, Ord t) => N.NFA n t -> DPDA Int t Char
fromNFA = fromDFA . D.relabel . N.toDFA

toList :: DPDA n t s -> ([(n, Maybe t, s, n, Stack s)], [n], n, s)
toList m = (edges =<< states, accept states, start', top m)
  where
    State start' _ _ = start m
    states           = M.elems (table m)

    edges (State n _ tx) = map (brand n) (M.toList tx)
    edges _              = error "impossible"

    brand n ((t, s), (n', ss)) = (n, t, s, n', ss)
    accept ss                  = [ n | State n Accept _ <- ss ]

free :: (Ord n, Ord t, Ord s)
     => DPDA n t s -> State n t s -> Stack s -> (State n t s, Stack s)
free _ Stuck ss               = (Stuck, ss)
free _ s []                   = (s, [])
free m s@(State _ _ tx) (h:t) = case lookup (Nothing, h) tx of
    Just (n, ss) -> case lookup n (table m) of
      Just s'    -> (s', ss ++ t)
      Nothing    -> (s, h:t)
    Nothing      -> (s, h:t)

step :: (Ord n, Ord t, Ord s)
     => DPDA n t s -> State n t s -> Stack s -> t -> (State n t s, Stack s)
step _ Stuck ss _              = (Stuck, ss)
step _ s [] _                  = (s, [])
step m (State _ _ tx) (s:ss) t = case lookup (Just t, s) tx of
    Just (n, rs) -> case lookup n (table m) of
      Just s'    -> free m s' (rs ++ ss)
      Nothing    -> (Stuck, s:ss)
    Nothing      -> (Stuck, s:ss)

eval :: (Ord n, Ord t, Ord s) => DPDA n t s -> [t] -> (State n t s, Stack s)
eval m = foldl' (uncurry (step m)) (free m (start m) [top m])

member :: (Ord n, Ord t, Ord s) => [t] -> DPDA n t s -> Bool
member ts m = accept (fst (eval m ts))
  where
    accept (State _ Accept _) = True
    accept _                  = False

elems :: (Ord n, Ord t, Ord s) => DPDA n t s -> [t]
elems = undefined

simplify :: DPDA n t s -> DPDA n t s
simplify = elimUseless . elimUnit . elimFree
  where
    elimFree    = undefined
    elimUnit    = undefined
    elimUseless = undefined

relabel :: DPDA n t s -> DPDA n t s
relabel = undefined
