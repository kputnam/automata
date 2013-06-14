module Language.Automata.Finite.Deterministic
  ( build
  , step
  , eval
  ) where

import Prelude hiding (lookup)
import Data.Map (Map, fromList, lookup)
import Data.List (foldl')
import Data.Maybe (fromMaybe)

data State s t
  = Stuck
  | State s Bool (Map t s)
  deriving (Eq, Show, Read)

build :: (Ord s, Ord t) => [(s, [(t, s)])] -> [s] -> Map s (State s t)
build states accept = fromList (map state states)
  where
    state (s, ts) = (s, State s (s `elem` accept) (fromList ts))

step :: (Ord s, Ord t) => Map s (State s t) -> State s t -> t -> State s t
step _ Stuck _          = Stuck
step m (State _ _ ts) t = case lookup t ts of
  Nothing -> Stuck
  Just s  -> fromMaybe Stuck (lookup s m)

eval :: (Ord s, Ord t) => Map s (State s t) -> s -> [t] -> Bool
eval m s = accept . foldl' (step m) (fromMaybe Stuck (lookup s m))
  where
    accept Stuck         = False
    accept (State _ x _) = x
