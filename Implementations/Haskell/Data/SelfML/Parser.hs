module Data.SelfML.Parser (
  Tree(..),
  Forest,
  smlForest,
  smlTree,
  smlString,
  smlVerbatim,
  smlBracketed,
  smlBacktick,
  smlLineComment,
  smlComment,
  smlWhitespace,
  smlSpaces) where

import Control.Applicative
import Data.Char
import Text.Parsec hiding ((<|>),many,spaces,optional)

{- The data structure has to go here for now - yes it's a bit misleading. 
   But otherwise there'd be a recursive module dependency. -}

-- | Like Data.Tree.Tree, but provides a singleton case. In essence, a conc list.
data Tree a = Node a (Forest a) | Terminal a
            deriving (Eq, Read, Show)

type Forest a = [Tree a]

smlForest :: Parsec [Char] () a -> Parsec [Char] () (Forest a)
smlTree   :: Parsec [Char] () a -> Parsec [Char] () (Tree   a)
smlString :: Parsec [Char] () [Char]

smlForest     v = spaces (smlTree v `sepEndBy` smlWhitespace)
smlTree       v = Terminal <$> v <|>
                  parens (Node <$> spaces v <*> smlForest v)
smlString       = smlBacktick <|> smlBracketed <|> smlVerbatim <?> "a string"
smlVerbatim     = many1 $ satisfyNone [flip elem "[](){}#`", isSpace]
smlBracketed    = brackets $ concat <$> many (("["++) . (++"]") <$> try smlBracketed <|> many1 (noneOf "[]"))
smlBacktick     = backticks $ many (try (string "``" *> pure '`') <|> noneOf "`")
smlLineComment  = char '#' *> many (noneOf "\n") *> (char '\n' *> pure () <|> eof)
smlComment      = braceHash $ many (notFollowedBy (string "#}") *>
                                    (try smlComment <|>
                                     anyChar *> pure ())) *> pure ()

parens    p = char   '('  *> p <* char   ')'
brackets  p = char   '['  *> p <* char   ']'
braceHash p = string "{#" *> p <* string "#}"
backticks p = char   '`'  *> p <* char   '`'

smlSpaces   p = smlWhitespace *> p <* smlWhitespace
smlWhitespace = many (smlLineComment <|> smlComment <|> satisfy isSpace *> pure ()) *> pure ()

spaces = smlSpaces

satisfyNone fs = satisfy $ \ c -> not $ any ($ c) fs
