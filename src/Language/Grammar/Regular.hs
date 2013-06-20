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

toNFA :: (Enum a, Ord a, Ord t) => Regexp t -> a -> N.NFA a t
toNFA r k
  = let (ts, as) = evalState (build r) k
     in N.fromList ts as k
  where
    -- inc :: (Enum a, MonadState a m) => m a
    inc = do
      n <- get
      modify succ
      return n

    -- build :: (Enum a, MonadState a m) => Regexp t -> m ([(a, Maybe t, a)], [a])
    build Empty        = do
      n <- inc
      return ([], [n])
    build (Literal a)  = do
      n <- inc
      m <- inc
      return ([(n, Just a, m)], [m])
    build (Concat a b) = do
      (ta, aa) <- build a; sb <- get
      (tb, ab) <- build b
      let ts = map (\x -> (x, Nothing, sb)) aa
      return (ta ++ tb ++ ts, ab)
    build (Choose a b) = do
      ss <- inc
      sa <- get; (ta, aa) <- build a
      sb <- get; (tb, ab) <- build b
      let ts = [(ss, Nothing, sa), (ss, Nothing, sb)]
      return (ta ++ tb ++ ts, aa ++ ab)
    build (Repeat a)   = do
      ss <- inc
      sa <- get; (ta, aa) <- build a
      let ts = map (\x -> (x, Nothing, sa)) aa
          tt = (ss, Nothing, sa)
      return (tt:ta ++ ts, ss:aa)
