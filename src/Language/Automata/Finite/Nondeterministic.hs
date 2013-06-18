module Language.Automata.Finite.Nondeterministic
  ( NFA
  , Token(..)
  , State(State)
  , fromList
  , toList
  , eval
  , toDFA
  , invertDFA
  , minimizeDFA
  , renumberNFA
  ) where

import Prelude hiding (lookup, read)
import Data.Maybe (fromMaybe)
import Data.Map (Map, lookup, fromListWith, unionWith, elems)
import Data.Set (Set, empty, insert, fold, union, member)
import Data.List (foldl')
import Control.Arrow (first, second)
import Control.Applicative ((<$>), (<*>))
import Control.Monad.State (gets, modify, evalState)
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

data NFA s t
  = NFA { table :: Map s (State s t)
        , start :: State s t }
  deriving (Eq, Show, Read)

fromList :: (Ord s, Ord t) => [(s, Token t, s)] -> [s] -> s -> NFA s t
fromList edges accept k = NFA table' start'
  where
    table' = unionWith combine incoming outgoing
    start' = fromMaybe Stuck (lookup k table')
    combine (State s b m) (State _ _ n) = State s b (unionWith union m n)
    incoming = M.fromList (map fromAccept accept)
    outgoing = fromListWith combine (map fromEdges edges)
    fromAccept s        = (s, State s True M.empty)
    fromEdges (a, t, b) = (a, State a (a `elem` accept) (M.singleton t (S.singleton b)))

toList :: NFA s t -> ([(s, Token t, s)], [s], s)
toList m = (edges =<< states, accept states, start')
  where
    State start' _ _    = start m
    states              = elems (table m)
    edges (State a _ t) = brand a =<< M.toList t
    brand a (t, bs)     = map (\b -> (a, t, b)) (S.toList bs)
    accept xs           = [ a | State a b _ <- xs, b ]

read :: (Ord s, Ord t) => NFA s t -> State s t -> Token t -> Set (State s t)
read _ Stuck _          = empty
read m (State _ _ ts) t = case lookup t ts of
  Nothing -> empty
  Just ss -> fold f empty ss
  where f s r = maybe r (`insert` r) (lookup s (table m))

-- TODO: Detect loops by passing original start state to recursive calls
free :: (Ord s, Ord t) => NFA s t -> State s t -> Set (State s t)
free m s = s `insert` fold union ss (S.map (free m) ss)
  where ss = read m s Epsilon

step :: (Ord s, Ord t) => NFA s t -> State s t -> t -> Set (State s t)
step m s t = fold (union . free m) empty (read m s (Token t))

eval :: (Ord s, Ord t) => NFA s t -> [t] -> Bool
eval m = any accept . S.toList . foldl' step' (free m (start m))
  where
    step' ss t = fold (\s -> union (step m s t)) empty ss
    accept Stuck         = False
    accept (State _ x _) = x

renumberNFA :: (Ord s, Ord t) => NFA s t -> NFA Int t
renumberNFA m = let (ts', as', ss') = evalState rebuild (0, M.empty)
                in fromList ts' as' ss'
  where
    (ts, as, ss) = toList m

    rebuild = do
      ss' <- index ss
      as' <- mapM index as
      ts' <- mapM edges ts
      return (ts', as', ss')

    fresh = do
      n <- gets fst
      modify (first (+1))
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

invertDFA :: (Bounded s, Enum s, Ord s, Ord t) => D.DFA s t -> NFA s t
invertDFA m = fromList (ss' ++ ts') as' s'
  where
    (ts, as, s) = D.toList m
    ts'  = map (\(a, t, b) -> (b, Token t, a)) ts
    ss'  = map (\a -> (s', Epsilon, a)) as
    as'  = [s]
    s'   = maybe minBound succ (max' ts)
    max' = max <$> foldr (max . Just . (\(a, _, _) -> a)) Nothing
               <*> foldr (max . Just . (\(_, _, b) -> b)) Nothing

minimizeDFA :: (Bounded s, Enum s, Ord s, Ord t) => D.DFA s t -> D.DFA Int t
minimizeDFA = D.renumberDFA . toDFA . invertDFA .
              D.renumberDFA . toDFA . invertDFA

toDFA :: (Ord s, Ord t) => NFA s t -> D.DFA (Set s) t
toDFA m = let (ts, as) = loop empty [flatten start']
           in D.fromList ts as (label start')
  where
    start'  = free m (start m)
    state s = fromMaybe (dummy s) (lookup s (table m))
    dummy s = State s False M.empty
    label   = S.map (\(State s _ _) -> s)

    -- loop :: Set (Set s) -> [State (Set s) t] -> ([(Set s, t, Set s)], [Set s])
    loop = loop' ([], [])
      where
        loop' acc _    []   = acc
        loop' (ts, as) done (s@(State a _ _):xs)
          | a `member` done = loop' (ts, as) done xs
          | otherwise       = let (ts', as') = tuple s
                                  ts''  = ts' ++ ts
                                  as''  = as' ++ as
                                  done' = a `insert` done
                                  xs'   = map (\(_, _, b) -> flatten (S.map state b)) ts'
                                  xs''  = xs ++ xs'
                               in loop' (ts'', as'') done' xs''

    -- tuple :: State (Set s) t -> ([(Set s, t, Set s)], [Set s])
    tuple (State s b t) = (trans, accept)
      where
        trans  = map (\(Token t', s') -> (s, t', join s')) (M.toList t)
        accept = [ s | b ]
        join   = S.unions . S.toList

    -- State 1 T (a -> {1,2})
    -- State 2 F (a -> {2,3})
    -- ---------------------------------
    -- State {1,2} T (a -> {{1,2},{2,3}}
    --
    -- flatten :: Set (State s t) -> State (Set s) t
    flatten ss = State (label freed) (accept freed) (edges freed)
      where
        freed     = fold (union . free m) empty ss
        accept    = S.fold (\(State _ a _) -> (|| a)) False
        edges     = S.fold (M.unionWith merge . tx) M.empty
        merge a b = S.singleton (S.unions (S.toList a ++ S.toList b))
        tx s@(State _ _ t) = M.fromList (fx s =<< M.toList t)
        fx _ (Epsilon, _)  = []
        fx s (Token t, _)  = [(Token t, S.singleton (label (step m s t)))]
