{-# LANGUAGE OverloadedStrings #-}

module Language.Grammar.Regular
  ( Regexp(..)
  , toNFA
  , fromNFA
  , parse
  ) where

import Data.Text (pack)
import Data.List (foldl')
import Data.Char (chr)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Attoparsec.Text hiding (parse)
import Control.Applicative
import Control.Monad.State (evalState, get, modify)
import qualified Language.Automata.Finite.Nondeterministic as N

data Regexp t
  = Empty
  | Literal t
  | Concat (Regexp t) (Regexp t)
  | Choose (Regexp t) (Regexp t)
  | Repeat (Regexp t)
  deriving (Eq, Read)--, Show)

-- parse  :: Parser Regexp Char

todo :: a
todo = error "todo"

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

-- POSIX Enhanced Regular Expression syntax
parse :: String -> Either String (Regexp Char)
parse = parseOnly (expr <* endOfInput) . pack
  where
    expr   = branch >>= (\b -> (Choose b <$> (char '|' *> expr)) <|> pure b)
    branch = piece >>= (\p -> (Concat p <$> branch) <|> pure p)
    piece  = atom >>= (\a -> (char '*' *> pure (Repeat a))
                       <|> (char '+' *> pure (Concat a (Repeat a)))
                       <|> (char '?' *> pure (Choose a Empty))
                       <|> (bound    *> todo)
                       <|> pure a)
    bound :: Parser (Int, Maybe Int)
    bound = brace ((,) <$> decimal <*> optional decimal)
    atom  = paren expr
        <|> paren (pure Empty)
        <|> brack klass
        <|> char '.'  *> todo
        <|> char '^'  *> todo
        <|> char '$'  *> todo
        <|> char '\\' *> (Literal <$> satisfy (inClass "^.[$()|*+?{\\"))
        <|> char '\\' *> (esc =<< satisfy (inClass "dswDSW"))
        <|> string "\\x" *> todo -- \xFF, \x{F}, \x{FF}, ...\x{FFFFFFFF}
        <|> char '\\' *> (Literal <$> anyChar)
        <|> Literal <$> satisfy (notInClass ")|+?*")
    esc   = fromJust . flip lookup
              [('d', oneOf '0' ['1'..'9'])
              ,('s', oneOf ' ' "\t")
              ,('w', oneOf '_' (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']))
              ,('D', todo)
              ,('S', todo)
              ,('W', todo)]
    klass = string "[:alnum:]"  *> oneOf 'a' (['b'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])
        <|> string "[:alpha:]"  *> oneOf 'a' (['b'..'z'] ++ ['A'..'Z'])
        <|> string "[:blank:]"  *> oneOf ' ' "\t"
        <|> string "[:cntrl:]"  *> oneOf (chr 0x7f) [chr 0x00 .. chr 0x1f]
        <|> string "[:digit:]"  *> oneOf '0' ['1'..'9']
        <|> string "[:graph:]"  *> oneOf (chr 0x21) [chr 0x22 .. chr 0x7e]
        <|> string "[:lower:]"  *> oneOf 'a' ['b'..'z']
        <|> string "[:print:]"  *> oneOf (chr 0x20) [chr 0x20 .. chr 0x7e]
        <|> string "[:punct:]"  *> oneOf ']' "[!\"#$%&'()*+,./:;<=>?@\\^_`{|}~-"
        <|> string "[:upper:]"  *> oneOf 'A' ['B'..'Z']
        <|> string "[:xdigit:]" *> oneOf 'a' (['b'..'f'] ++ ['A'..'F'] ++ ['0'..'9'])
    oneOf = (pure .) . foldl' (flip (Choose . Literal)) . Literal

    paren = wrapped '(' ')'
    brace = wrapped '{' '}'
    brack = wrapped '[' ']'
    wrapped a b p = (char a *> p) <* char b

fromNFA :: (Enum a, Ord a, Ord t) => N.NFA a t -> Regexp t
fromNFA = todo

instance Literal t => Show (Regexp t) where
  show = walk 0
    where
      walk :: Literal t => Int -> Regexp t -> String
      walk _ Empty  = ""
      walk _ (Literal t)
                    = lit t
      walk p e@(Choose a b)
        | p <= q    = walk q a <> "|" <> walk q b
        | otherwise = paren q e
        where q = 1
      walk p e@(Concat a b)
        | p <= q    = walk q a <> gap e <> walk q b
        | otherwise = paren q e
        where q = 2
      walk p e@(Repeat a)
        | p < q     = walk q a <> "*"
        | p == q    = "(" <> walk q a <> "*)"
        | otherwise = paren q e
        where q = 3

      paren :: Literal t => Int -> Regexp t -> String
      paren p e = "(" <> walk p e <> ")"

class Literal t where
  gap :: Regexp t -> String
  lit :: t -> String

instance Literal Int where
  gap _ = " "
  lit   = show

instance Literal Char where
  gap _ = ""
  lit c = [c]
