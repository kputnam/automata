module Language.Automata.Finite.RegularExpression where

import qualified Language.Automata.Finite.Nondeterministic as N

data Regexp a
  = Empty
  | Literal a
  | Concat (Regexp a) (Regexp a)
  | Choose (Regexp a) (Regexp a)
  | Repeat (Regexp a)
  deriving (Eq, Show, Read)

-- parse  :: Parser Regexp Char
-- pretty :: Regexp Char -> String

toNFA :: Regexp a -> N.NFA Int a
toNFA = undefined
