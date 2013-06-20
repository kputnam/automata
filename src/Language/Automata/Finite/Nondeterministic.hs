module Language.Automata.Finite.Nondeterministic
  ( NFA
  , State(State)
  , fromList
  , fromDFA
  , toList
  , toDFA
  , member
  , elems
  , union
  , intersect
  , concat
  , complement
  , kleene
  , reverse
  , reverseDFA
  , minimizeDFA
  , relabel
  ) where

import Prelude hiding (lookup, read, concat, reverse)
import Data.Maybe (fromMaybe)
import Data.Map (Map, lookup, fromListWith, unionWith)
import Data.Set (Set, empty, insert, fold)
import Data.List (foldl')
import Control.Arrow (first, second)
import Control.Applicative ((<$>), (<*>))
import Control.Monad.State (gets, modify, evalState, ap)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Language.Automata.Finite.Deterministic as D

data State a t
  = State a Bool (Map (Maybe t) (Set a))
  deriving (Eq, Show, Read, Ord)

data NFA a t
  = NFA { table :: Map a (State a t)
        , start :: State a t }
  deriving (Eq, Show, Read)

-- First argument is a list of tuples (from, token, to) which specifies
-- when the machine is at state `from` and reads `token`, the resulting
-- state is `to`. Because this machine is non-deterministic, TODO...
--
-- The second argument is a list of accept states. If the machine halts
-- on one of these states, the input is "accepted" and eval will return
-- True.
--
-- The last argument is a single start state.
--
fromList :: (Ord a, Ord t) => [(a, Maybe t, a)] -> [a] -> a -> NFA a t
fromList edges accept k = NFA table' start'
  where
    table' = unionWith combine incoming outgoing
    start' = fromMaybe (State k (k `elem` accept) M.empty) (lookup k table')

    combine (State a x m) (State _ _ n) = State a x (unionWith S.union m n)
    incoming = M.fromList (map fromAccept accept)
    outgoing = fromListWith combine (map fromEdge edges)

    fromAccept a       = (a, State a True M.empty)
    fromEdge (a, t, b) = (a, State a (a `elem` accept) (M.singleton t (S.singleton b)))

fromDFA :: (Ord a, Ord t) => D.DFA a t -> NFA a t
fromDFA = undefined

-- Produce a representation of the given NFA as a list of edges, accept
-- states, and the start state
--
toList :: NFA a t -> ([(a, Maybe t, a)], [a], a)
toList m = (edges =<< states, accept states, start')
  where
    State start' _ _    = start m
    states              = M.elems (table m)
    edges (State a _ t) = brand a =<< M.toList t
    brand a (t, bs)     = map (\b -> (a, t, b)) (S.toList bs)
    accept as           = [ a | State a x _ <- as, x ]

-- Rabin–Scott powerset construction
toDFA :: (Ord a, Ord t) => NFA a t -> D.DFA (Set a) t
toDFA m = let (ts, as) = loop empty [flatten start']
           in D.fromList ts as (label start')
  where
    start'  = free m (start m)
    state a = fromMaybe (dummy a) (lookup a (table m))
    dummy a = State a False M.empty
    label   = S.map (\(State a _ _) -> a)

    -- loop :: Set (Set s) -> [State (Set s) t] -> ([(Set s, t, Set s)], [Set s])
    loop = loop' ([], [])
      where
        loop' acc _    []   = acc
        loop' (ts, as) done (s@(State a x _):ss)
          | a `S.member` done = loop' (ts, as) done ss
          | otherwise         = let ts' = tuple s
                                    ss' = map (\(_, _, b) -> flatten (S.map state b)) ts'
                                 in loop' (ts' ++ ts, [a | x] ++ as)
                                          (a `insert` done)
                                          (ss' ++ ss)

    -- tuple :: State (Set s) t -> [(Set s, t, Set s)]
    tuple (State a _ ts) = edges
      where
        edges = map (\(Just t, b) -> (a, t, join b)) (M.toList ts)
        join  = S.unions . S.toList

    -- flatten :: Set (State s t) -> State (Set s) t
    flatten ss = State (label freed) (accept freed) (edges freed)
      where
        freed     = fold (S.union . free m) empty ss
        accept    = S.fold (\(State _ x _) -> (|| x)) False
        edges     = S.fold (M.unionWith merge . tx) M.empty
        merge a b = S.singleton (S.unions (S.toList a ++ S.toList b))

        tx s@(State _ _ t) = M.fromList (fx s =<< M.toList t)
        fx _ (Nothing, _)  = []
        fx s (Just t, _)   = [(Just t, S.singleton (label (step m s t)))]

-- Produce a set of outcome states after reading the given token (which
-- maybe be Nothing) from the given state.
--
-- This follows paths of at most one edge from the current state: it does
-- not recursively follow free transitions.
--
read :: (Ord a, Ord t) => NFA a t -> State a t -> Maybe t -> Set (State a t)
read m (State _ _ ts) t = case lookup t ts of
  Nothing -> empty
  Just as -> fold f empty as
  where
    -- If a is a non-existent state, we treat it the same as a "stuck"
    -- state and do not add it to the set of outcome states. The empty
    -- set is then a representation of a single "stuck" state.
    f a r = maybe r (`insert` r) (lookup a (table m))

-- Produce a set of outcome states after following (and also not following)
-- all free transitions, recursively, from the given state.
--
free :: (Ord a, Ord t) => NFA a t -> State a t -> Set (State a t)
free m s = let qs = next s
            in s `insert` fold S.union qs (S.map loop qs)
  where
    next q = read m q Nothing
    loop q
      | s == q    = empty -- we back to where we started
      | otherwise = let qs = next q
                     in q `insert` fold S.union qs (S.map loop qs)

-- Produce a set of outcome states after reading the given token (which
-- may not be Nothing) from the given state.
--
-- This traverses all free transitions /after/ reading the input token.
--
step :: (Ord a, Ord t) => NFA a t -> State a t -> t -> Set (State a t)
step m s = fold (S.union . free m) empty . read m s . Just

-- Run the simulation, producing True if the machine accepted the input
-- or False otherwise.
--
member :: (Ord a, Ord t) => [t] -> NFA a t -> Bool
member ts m = accept (eval ts)
  where
    eval = foldl' step' (ap free start m)

    -- Input is accepted if any accept state is in the outcome set
    accept = S.fold (\(State _ x _) -> (|| x)) False

    -- Take a set of start states and produce a set of outcome states
    step' ss t = fold (\s -> S.union (step m s t)) empty ss

-- Run the simulation, generating all input strings of the language
--
elems :: (Ord a, Ord t) => NFA a t -> [t]
elems = undefined

union :: NFA a t -> NFA a t -> NFA a t
union = undefined

intersect :: NFA a t -> NFA a t -> NFA a t
intersect = undefined

concat :: NFA a t -> NFA a t -> NFA a t
concat = undefined

complement :: NFA a t -> NFA a t
complement = undefined

kleene :: NFA a t -> NFA a t
kleene = undefined

reverse :: NFA a t -> NFA a t
reverse = undefined

-- Construct an NFA which accepts the /reversed/ inputs that the original
-- DFA accepts, and rejects the /reversed/ inputs that the original DFA
-- rejects.
--
reverseDFA :: (Bounded a, Enum a, Ord a, Ord t) => D.DFA a t -> NFA a t
reverseDFA m = fromList (ss' ++ ts') as' s'
  where
    (ts, as, s) = D.toList m

    -- Start state label is the next "available" label
    s'   = maybe minBound succ (max' ts)
    max' = max <$> foldr (max . Just . (\(a, _, _) -> a)) Nothing
               <*> foldr (max . Just . (\(_, _, b) -> b)) Nothing

    ss' = map (\a -> (s', Nothing, a)) as
      -- Free transitions from each former accept state to new start

    ts' = map (\(a, t, b) -> (b, Just t, a)) ts
      -- Reverse edge direction

    as' = [s]
      -- New accept state is the former start state

-- Brzozowski's algorithm
--
minimizeDFA :: (Bounded s, Enum s, Ord s, Ord t) => D.DFA s t -> D.DFA Int t
minimizeDFA = f . g
  where
    -- Explicit type annotation required because `Int` can't be inferred
    -- from D.relabel . toDFA . reverseDFA . D.relabel . toDFA . reverseDFA
    f :: Ord t => D.DFA Int t -> D.DFA Int t
    f = D.relabel . toDFA . reverseDFA

    g :: (Bounded s, Enum s, Ord s, Ord t) => D.DFA s t -> D.DFA Int t
    g = D.relabel . toDFA . reverseDFA

-- Replace each distinct state label (of any type 'a') with a distinct label
-- of type 'b'. Type 'b' can be any for which minBound and succ are defined.
relabel :: (Ord a, Ord t, Ord b, Bounded b, Enum b) => NFA a t -> NFA b t
relabel m
  = let (ts, as, ss) = evalState rebuild (minBound, M.empty)
     in fromList ts as ss
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
      maybe (store s) return (lookup s k)

    edges (a, t, b) = do
      a' <- index a
      b' <- index b
      return (a', t, b')
