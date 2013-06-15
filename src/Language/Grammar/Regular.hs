module Language.Grammar.Regular where

import qualified Language.Automata.Finite.Nondeterministic as N

import Control.Monad.State

data Regexp a
  = Empty
  | Literal a
  | Concat (Regexp a) (Regexp a)
  | Choose (Regexp a) (Regexp a)
  | Repeat (Regexp a)
  deriving (Eq, Show, Read)

-- parse  :: Parser Regexp Char
-- pretty :: Regexp Char -> String

inc :: (Enum s, MonadState s m) => m s
inc = do
  n <- get
  modify succ
  return n

toNFA :: (Enum s, Ord s, Ord a) => Regexp a -> s -> N.NFA s a
toNFA r = uncurry N.build . evalState (build r)
  where
    build Empty        = do
      n <- inc
      return ([], [n])

    build (Literal a)  = do
      n <- inc
      m <- inc
      return ([(n, N.Token a, m)], [m])

    build (Concat a b) = do
      (ta, aa) <- build a; sb <- get
      (tb, ab) <- build b
      let ts = map (\x -> (x, N.Epsilon, sb)) aa
      return (ta ++ tb ++ ts, ab)

    build (Choose a b) = do
      ss <- inc
      sa <- get; (ta, aa) <- build a
      sb <- get; (tb, ab) <- build b
      let ts = [(ss, N.Epsilon, sa), (ss, N.Epsilon, sb)]
      return (ta ++ tb ++ ts, aa ++ ab)

    build (Repeat a)   = do
      ss <- inc
      sa <- get; (ta, aa) <- build a
      let ts = map (\x -> (x, N.Epsilon, sa)) aa
          tt = (ss, N.Epsilon, sa)
      return (tt:ta ++ ts, ss:aa)
