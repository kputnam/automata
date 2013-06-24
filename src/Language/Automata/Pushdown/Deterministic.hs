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
import qualified Language.Automata.Finite.Deterministic    as D
import qualified Language.Automata.Finite.Nondeterministic as N

data State a t s
  = Stuck
  | State a Bool (Map (Maybe t, s) (a, [s]))
  deriving (Eq, Show, Read, Ord)

data DPDA a t s
  = DPDA { table :: Map a (State a t s)
         , start :: State a t s
         , stack :: s }
  deriving (Eq, Show, Read)

fromList :: (Ord a, Ord s, Ord t)
         => [(a, Maybe t, s, a, [s])] -> [a] -> a -> s -> DPDA a t s
fromList edges accept k = DPDA table' start'
  where
    table' = unionWith combine incoming outgoing
    start' = fromMaybe (State k (k `elem` accept) empty) (lookup k table')

    combine (State a x m) (State _ _ n) = State a x (m `union` n)
    combine _ _ = error "impossible"

    incoming = M.fromList (map fromAccept accept)
    outgoing = fromListWith combine (map fromEdge edges)

    fromAccept a = (a, State a True empty)
    fromEdge (a, t, s, b, ss) = (a, State a (a `elem` accept) (singleton (t,s) (b,ss)))

fromDFA :: (Ord a, Ord t) => D.DFA a t -> DPDA a t Char
fromDFA m = fromList ts' as s '$'
  where
    (ts, as, s) = D.toList m
    ts' = map (\(a, t, b) -> (a, Just t, '$', b, "$")) ts

fromNFA :: (Ord a, Ord t) => N.NFA a t -> DPDA Int t Char
fromNFA = fromDFA . D.relabel . N.toDFA

toList :: DPDA a t s -> ([(a, Maybe t, s, a, [s])], [a], a, s)
toList m = (edges =<< states, accept states, start', stack m)
  where
    State start' _ _ = start m
    states           = M.elems (table m)

    edges (State a _ t) = map (brand a) (M.toList t)
    edges _             = error "impossible"

    brand a ((t, s), (b, ss)) = (a, t, s, b, ss)
    accept as                 = [ a | State a x _ <- as, x ]

free :: (Ord a, Ord t, Ord s)
     => DPDA a t s -> State a t s -> [s] -> (State a t s, [s])
free _ Stuck ss               = (Stuck, ss)
free _ s []                   = (s, [])
free m s@(State _ _ ts) (h:t) = case lookup (Nothing, h) ts of
    Just (a, ss) -> case lookup a (table m) of
      Just s'    -> (s', ss ++ t)
      Nothing    -> (s, h:t)
    Nothing      -> (s, h:t)

step :: (Ord a, Ord t, Ord s)
     => DPDA a t s -> State a t s -> [s] -> t -> (State a t s, [s])
step _ Stuck ss _              = (Stuck, ss)
step _ s [] _                  = (s, [])
step m (State _ _ ts) (s:ss) t = case lookup (Just t, s) ts of
    Just (a, rs) -> case lookup a (table m) of
      Just s'    -> free m s' (rs ++ ss)
      Nothing    -> (Stuck, s:ss)
    Nothing      -> (Stuck, s:ss)

eval :: (Ord a, Ord t, Ord s) => DPDA a t s -> [t] -> (State a t s, [s])
eval m = foldl' (uncurry (step m)) (free m (start m) [stack m])

member :: (Ord a, Ord t, Ord s) => [t] -> DPDA a t s -> Bool
member ts m = accept (fst (eval m ts))
  where
    accept Stuck         = False
    accept (State _ x _) = x

elems :: (Ord a, Ord t, Ord s) => DPDA a t s -> [t]
elems = undefined

simplify :: DPDA a t s -> DPDA a t s
simplify = elimUseless . elimUnit . elimFree
  where
    elimFree    = undefined
    elimUnit    = undefined
    elimUseless = undefined

relabel :: DPDA a t s -> DPDA a t s
relabel = undefined
