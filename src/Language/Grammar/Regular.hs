{-# LANGUAGE FlexibleInstances #-}

module Language.Grammar.Regular where

import Data.Monoid ((<>))
import Control.Monad.State (evalState, get, modify)
import qualified Language.Automata.Finite.Nondeterministic as N

data Regexp t
  = Empty
  | Literal t
  | Concat (Regexp t) (Regexp t)
  | Choose (Regexp t) (Regexp t)
  | Repeat (Regexp t)
  deriving (Eq, Read)

-- parse  :: Parser Regexp Char

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
    build Empty = do
      n <- inc
      return ([], [n])
    build (Literal a) = do
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
    build (Repeat a) = do
      ss <- inc
      sa <- get; (ta, aa) <- build a
      let ts = map (\x -> (x, Nothing, sa)) aa
          tt = (ss, Nothing, sa)
      return (tt:ta ++ ts, ss:aa)

class Literal t where
  gap :: Regexp t -> String
  lit :: t -> String

instance Literal Int where
  gap _ = " "
  lit   = show

instance Literal Char where
  gap _ = ""
  lit c = [c]

instance Literal t => Show (Regexp t) where
  show = walk 0
    where
      walk :: Literal t => Int -> Regexp t -> String
      walk _ Empty  = ""
      walk _ (Literal t)
                    = lit t
      walk p e@(Concat a b)
        | p <= q    = walk q a <> gap e <> walk q b
        | otherwise = paren q e
        where q = 2
      walk p e@(Choose a b)
        | p <= q    = walk q a <> "|" <> walk q b
        | otherwise = paren q e
        where q = 2
      walk p e@(Repeat a)
        | p < q     = walk q a <> "*"
        | p == q    = "(" <> walk q a <> "*)"
        | otherwise = paren q e
        where q = 3

      paren :: Literal t => Int -> Regexp t -> String
      paren p e = "(" <> walk p e <> ")"
