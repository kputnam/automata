module Language.Automata.Finite.Deterministic
  ( DFA
  , fromList
  , toList
  , eval
  ) where

import Prelude hiding (lookup)
import Data.Maybe (fromMaybe)
import Data.List (foldl')
import Data.Map (Map, fromListWith, union, singleton,
                 elems, lookup, empty, unionWith)
import qualified Data.Map as M

data State s t
  = Stuck
  | State s Bool (Map t s)
  deriving (Eq, Show, Read)

data DFA s t
  = DFA { table :: Map s (State s t)
        , start :: State s t }
  deriving (Eq, Show, Read)

fromList :: (Ord s, Ord t) => [(s, t, s)] -> [s] -> s -> DFA s t
fromList edges accept k = DFA table' start'
  where
    table' = unionWith combine incoming outgoing
    start' = fromMaybe Stuck (lookup k table')
    combine (State s b m) (State _ _ n) = State s b (m `union` n)
    incoming = M.fromList (map fromAccept accept)
    outgoing = fromListWith combine (map fromEdges edges)
    fromAccept s        = (s, State s True empty)
    fromEdges (a, t, b) = (a, State a (a `elem` accept) (singleton t b))

toList :: DFA s t -> ([(s, t, s)], [s], s)
toList m = (edges =<< states, accept states, start')
  where
    State start' _ _    = start m
    states              = elems (table m)
    edges (State a _ t) = map (brand a) (M.toList t)
    brand a (t, b)      = (a, t, b)
    accept xs           = [ a | State a b _ <- xs, b ]

step :: (Ord s, Ord t) => DFA s t -> State s t -> t -> State s t
step _ Stuck _          = Stuck
step m (State _ _ ts) t = case lookup t ts of
  Nothing -> Stuck
  Just s  -> fromMaybe Stuck (lookup s (table m))

eval :: (Ord s, Ord t) => DFA s t -> [t] -> Bool
eval m = accept . foldl' (step m) (start m)
  where
    accept Stuck         = False
    accept (State _ x _) = x
