module Language.Automata.Finite.Nondeterministic
  ( NFA
  , fromList
  , fromDFA
  , toList
  , toDFA
  , member
  , elems
  , union
  , difference
  , intersection
  , concat
  , complement
  , kleene
  , isSubsetOf
  , reverse
  , reverseDFA
  , minimizeDFA
  , relabel
  ) where

import Prelude hiding (lookup, read, concat, reverse)
import Data.Maybe (fromMaybe)
import Data.Map (Map, lookup, fromListWith, unionWith)
import Data.Monoid
import Data.Set (Set, empty, insert)
import Data.List (foldl')
import Control.Arrow (first, second)
import Control.Applicative ((<$>), (<*>))
import Control.Monad.State (gets, modify, evalState, ap)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Language.Automata.Finite.Deterministic as D

data Mode
  = Accept
  | Reject
  deriving (Eq, Show, Read, Ord)

instance Monoid Mode where
  mempty           = Reject
  mappend Accept _ = Accept
  mappend _ Accept = Accept
  mappend _ _      = Reject

modeIf :: Bool -> Mode
modeIf True  = Accept
modeIf False = Reject

data State n t
  = State n Mode (Map (Maybe t) (Set n))
  deriving (Eq, Show, Read, Ord)

data NFA n t
  = NFA { table :: Map n (State n t)
        , start :: State n t }
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
fromList :: (Ord n, Ord t) => [(n, Maybe t, n)] -> [n] -> n -> NFA n t
fromList edges accept k = NFA table' start'
  where
    table' = unionWith combine incoming outgoing
    start' = fromMaybe (State k (modeIf $ k `elem` accept) mempty) (lookup k table')

    combine (State n x m) (State n' x' m')
      | n == n' &&
        x == x'   = State n x (unionWith S.union m m')
      | otherwise = error "impossible"
    combine _ _   = error "impossible"

    incoming = M.fromList (map fromAccept accept)
    outgoing = fromListWith combine (map fromEdge edges)

    fromAccept n        = (n, State n Accept mempty)
    fromEdge (n, t, n') = (n, State n (modeIf $ n `elem` accept) (M.singleton t (S.singleton n')))

fromDFA :: (Ord n, Ord t) => D.DFA n t -> NFA n t
fromDFA = undefined

-- Produce a representation of the given NFA as a list of edges, accept
-- states, and the start state
--
toList :: NFA n t -> ([(n, Maybe t, n)], [n], n)
toList m = (edges =<< states, accept states, start')
  where
    State start' _ _     = start m
    states               = M.elems (table m)
    edges (State n _ tx) = brand n =<< M.toList tx
    brand n (t, tx)      = map (\n' -> (n, t, n')) (S.toList tx)
    accept ss            = [ n | State n Accept _ <- ss ]

-- Rabin–Scott powerset construction
toDFA :: (Ord n, Ord t) => NFA n t -> D.DFA (Set n) t
toDFA m = let (ts, as) = loop empty [flatten start']
           in D.fromList ts as (label start')
  where
    start'  = free m (start m)
    state n = fromMaybe (dummy n) (lookup n (table m))
    dummy n = State n Reject mempty
    label   = S.map (\(State n _ _) -> n)

    -- loop :: Set (Set s) -> [State (Set s) t] -> ([(Set s, t, Set s)], [Set s])
    loop = loop' ([], [])
      where
        loop' acc _    []   = acc
        loop' (ts, as) done (s@(State n x _):ss)
          | n `S.member` done = loop' (ts, as) done ss
          | otherwise         = let ts' = tuple s
                                    ss' = map (\(_, _, n') -> flatten (S.map state n')) ts'
                                 in loop' (ts' ++ ts, [n | x == Accept] ++ as)
                                          (n `insert` done)
                                          (ss' ++ ss)

    -- tuple :: State (Set s) t -> [(Set s, t, Set s)]
    tuple (State n _ tx) = edges
      where
        edges = map (\(Just t, n') -> (n, t, join n')) (M.toList tx)
        join  = S.unions . S.toList

    -- flatten :: Set (State s t) -> State (Set s) t
    flatten ss = State (label freed) (mode freed) (edges freed)
      where
        freed     = S.fold (S.union . free m) empty ss
        mode      = S.fold (\(State _ x _) -> (<> x)) mempty
        edges     = S.fold (M.unionWith merge . tx) mempty
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
read :: (Ord n, Ord t) => NFA n t -> State n t -> Maybe t -> Set (State n t)
read m (State _ _ tx) t = case lookup t tx of
  Nothing -> empty
  Just as -> S.fold f empty as
  where
    -- If a is a non-existent state, we treat it the same as a "stuck"
    -- state and do not add it to the set of outcome states. The empty
    -- set is then a representation of a single "stuck" state.
    f n acc = maybe acc (`insert` acc) (lookup n (table m))

-- Produce a set of outcome states after following (and also not following)
-- all free transitions, recursively, from the given state.
--
free :: (Ord n, Ord t) => NFA n t -> State n t -> Set (State n t)
free m s = s `insert` loop empty s
  where
    loop acc q
      | q `S.member` acc = acc
      | otherwise        = let acc' = q `insert` acc
                               qs   = read m q Nothing
                            in S.fold S.union qs (S.map (loop acc') qs)

-- Produce a set of outcome states after reading the given token (which
-- may not be Nothing) from the given state.
--
-- This traverses all free transitions /after/ reading the input token.
--
step :: (Ord n, Ord t) => NFA n t -> State n t -> t -> Set (State n t)
step m s = S.fold (S.union . free m) empty . read m s . Just

-- Run the simulation, producing True if the machine accepted the input
-- or False otherwise.
--
member :: (Ord n, Ord t) => [t] -> NFA n t -> Bool
member ts m = mode (eval ts) == Accept
  where
    eval = foldl' step' (ap free start m)

    -- Input is accepted if any accept state is in the outcome set
    mode = S.fold (\(State _ x _) -> (<> x)) mempty

    -- Take a set of start states and produce a set of outcome states
    step' ss t = S.fold (\s -> S.union (step m s t)) empty ss

-- Run the simulation, generating all input strings of the language
--
elems :: (Ord n, Ord t) => NFA n t -> [t]
elems = undefined

union :: NFA n t -> NFA n t -> NFA n t
union = undefined

difference :: NFA n t -> NFA n t -> NFA n t
difference = undefined

intersection :: NFA n t -> NFA n t -> NFA n t
intersection = undefined

concat :: NFA n t -> NFA n t -> NFA n t
concat = undefined

complement :: NFA n t -> NFA n t
complement = undefined

kleene :: NFA n t -> NFA n t
kleene = undefined

isSubsetOf :: NFA n t -> NFA n t -> NFA n t
isSubsetOf = undefined

reverse :: NFA n t -> NFA n t
reverse = undefined

-- Construct an NFA which accepts the /reversed/ inputs that the original
-- DFA accepts, and rejects the /reversed/ inputs that the original DFA
-- rejects.
--
reverseDFA :: (Bounded n, Enum n, Ord n, Ord t) => D.DFA n t -> NFA n t
reverseDFA m = fromList (ss' ++ ts') as' s'
  where
    (ts, as, s) = D.toList m

    -- Start state label is the next "available" label
    s'   = maybe minBound succ (max' ts)
    max' = max <$> foldr (max . Just . (\(n, _, _) -> n)) Nothing
               <*> foldr (max . Just . (\(_, _, b) -> b)) Nothing

    ss' = map (\n' -> (s', Nothing, n')) as
      -- Free transitions from each former accept state to new start

    ts' = map (\(n, t, n') -> (n', Just t, n)) ts
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
relabel :: (Ord n, Ord t, Ord m, Bounded m, Enum m) => NFA n t -> NFA m t
relabel m = relabelWithMin m minBound

relabelWithMin :: (Ord n, Ord t, Ord m, Enum m) => NFA n t -> m -> NFA m t
relabelWithMin m s
  = let (ts, as, ss) = evalState rebuild (s, mempty)
     in fromList ts as ss
  where
    rebuild = do
      let (ts, as, ss) = toList m
      ss' <- indexOf ss
      as' <- mapM indexOf as
      ts' <- mapM edges ts
      return (ts', as', ss')

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
      maybe (store n) return (lookup n tx)

    edges (a, t, b) = do
      a' <- indexOf a
      b' <- indexOf b
      return (a', t, b')
