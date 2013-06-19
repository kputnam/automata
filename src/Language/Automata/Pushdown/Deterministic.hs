module Language.Automata.Pushdown.Deterministic where

import Prelude hiding (lookup)
import Data.Maybe (fromMaybe)
import Data.Map (Map, empty, lookup, union, unionWith,
                 singleton, fromListWith, elems)
import qualified Data.Map as M


data State a t s
  = Stuck
  | State a Bool (Map (Maybe t, s) (a, [s]))
  deriving (Eq, Show, Read, Ord)

data DPDA a t s
  = DPDA { table :: Map a (State a t s)
         , start :: State a t s
         , stack :: [s] }
  deriving (Eq, Show, Read)

fromList :: (Ord a, Ord s, Ord t) => [(a, Maybe t, s, a, [s])] -> [a] -> a -> [s] -> DPDA a t s
fromList edges accept k = DPDA table' start'
  where
    table' = unionWith combine incoming outgoing
    start' = fromMaybe (State k (k `elem` accept) empty) (lookup k table')

    combine (State a x m) (State _ _ n) = State a x (m `union` n)
    incoming = M.fromList (map fromAccept accept)
    outgoing = fromListWith combine (map fromEdge edges)

    fromAccept a = (a, State a True empty)
    fromEdge (a, t, s, b, ss) = (a, State a (a `elem` accept) (singleton (t,s) (b,ss)))

toList :: DPDA a t s -> ([(a, Maybe t, s, a, [s])], [a], a, [s])
toList m = (edges =<< states, accept states, start', stack m)
  where
    State start' _ _          = start m
    states                    = elems (table m)
    edges (State a _ t)       = map (brand a) (M.toList t)
    brand a ((t, s), (b, ss)) = (a, t, s, b, ss)
    accept as                 = [ a | State a x _ <- as, x ]
