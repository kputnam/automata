module Language.Automata.Finite.Nondeterministic
  ( NFA
  , Token(..)
  , build
  , eval
  , toDFA
  ) where

import Prelude hiding (lookup, read, null)
import Data.Maybe (fromMaybe)
import Data.Map (Map, lookup, fromListWith, unionWith)
import Data.Set (Set, empty, insert, fold, union, toList, null)
import Data.List (foldl')
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Language.Automata.Finite.Deterministic as D

data State s t
  = Stuck
  | State s Bool (Map (Token t) (Set s))
  deriving (Eq, Show, Read, Ord)

data Token t
  = Epsilon
  | Token t
  deriving (Eq, Show, Read, Ord)

newtype NFA s t
  = NFA { getM :: Map s (State s t) }
  deriving (Eq, Show, Read)

build :: (Ord s, Ord t) => [(s, Token t, s)] -> [s] -> NFA s t
build trans accept = NFA (unionWith combine incoming outgoing)
  where
    combine (State s b m) (State _ _ n) = State s b (unionWith union m n)
    incoming = M.fromList (map fromAccept accept)
    outgoing = fromListWith combine (map fromTrans trans)
    fromAccept s        = (s, State s True M.empty)
    fromTrans (a, t, b) = (a, State a (a `elem` accept) (M.singleton t (S.singleton b)))

read :: (Ord s, Ord t) => NFA s t -> State s t -> Token t -> Set (State s t)
read _ Stuck _          = empty
read m (State _ _ ts) t = case lookup t ts of
  Nothing -> empty
  Just ss -> fold f empty ss
  where f s r = maybe r (`insert` r) (lookup s (getM m))

free :: (Ord s, Ord t) => NFA s t -> State s t -> Set (State s t)
free m s = if null ss
             then ss
             else fold union ss (S.map (free m) ss)
  where ss = read m s Epsilon

step :: (Ord s, Ord t) => NFA s t -> State s t -> t -> Set (State s t)
step m s t = fold (union . free m) (read m s (Token t)) empty

eval :: (Ord s, Ord t) => NFA s t -> s -> [t] -> Bool
eval m i = any accept . toList . foldl' step' (start `insert` free m start)
  where
    start      = fromMaybe Stuck (lookup i (getM m))
    step' ss t = fold (\s -> union (step m s t)) empty ss
    accept Stuck         = False
    accept (State _ x _) = x

toDFA :: NFA s t -> D.DFA s t
toDFA = undefined
