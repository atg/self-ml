module Data.SelfML.SelfPath.Parser (
  SelfPath(..),
  Query(..),
  parseSelfPath,
  spNode,
  spNodeI,
  spQuery,
  spNot,
  spEqual,
  spAtEq,
  spAtRange,
  spChild,
  spNamed,
  spWhitespace,
  spSpaces,
  spNum,
  spString,
  spVerbatim) where

import Control.Applicative
import Data.Char
import Data.List
import Data.Monoid
import Data.SelfML
import Data.SelfML.Parser
import Text.Parsec hiding (many, (<|>), spaces, optional)

parseSelfPath :: String -> Either ParseError SelfPath
parseSelfPath = parse (spSpaces spNode <* eof) ""

data Query = Named String
           | Equal (Tree String)
           | Child
           | At    Integer
           | AtEq  Integer (Tree String)
           | Range Integer Integer
           | Not   Query
           deriving (Eq, Read, Show)

data SelfPath = Recursive  Query    SelfPath
              | Direct     Query    SelfPath
              | Lookahead  SelfPath SelfPath
              | NLookahead SelfPath SelfPath
              | MEither    SelfPath SelfPath
              | Anything
              deriving (Eq, Read, Show)

instance Monoid SelfPath where
  mempty                      = Anything
  Recursive  q  sp `mappend` b = Recursive   q               (sp `mappend` b)
  Direct     q  sp `mappend` b = Direct      q               (sp `mappend` b)
  Lookahead  sa sb `mappend` b = Lookahead   sa              (sb `mappend` b)
  NLookahead sa sb `mappend` b = NLookahead  sa              (sb `mappend` b)
  MEither    sa sb `mappend` b = MEither    (sa `mappend` b) (sb `mappend` b)
  Anything         `mappend` b = b

spNode :: Parsec [Char] () SelfPath

spNode = try (MEither <$> spNodeI <*> (spSpaces (char ',') *> spNode)) <|> spNodeI

spNodeI = try (lah       <$> many (char '!') <*> parens (spNode)   <*> sn)
      <|>      Direct    <$> (char '>' *> spWhitespace *> spQuery) <*> sn
      <|>                     char '$' *> spWhitespace *> spNode
      <|>      Recursive <$> spQuery                               <*> sn
  where sn = try (spWhitespace *> spNodeI) <|> pure Anything
        lah excl ie sp =
          (if even (length excl) then Lookahead else NLookahead) ie sp

spQuery = spNot
      <|> spEqual
      <|> try spAtEq
      <|> spAtRange
      <|> spChild
      <|> spNamed

spNot        = Not   <$> (char '!' *> spQuery)
spEqual      = Equal <$> (char '=' *> smlTree spString)
spAtEq       = AtEq  <$> (char '#' *> spNum) <*> (char '=' *> smlTree spString)
spAtRange    = fd    <$> (char '#' *> spNum) <*> optional (char '-' *> spNum)
  where fd n = maybe (At n) (Range n)
spChild      = char '*' *> pure Child
spNamed      = Named <$> spString

spWhitespace = many (smlComment <|> satisfy isSpace *> pure ())
spSpaces p   = spWhitespace *> p <* spWhitespace

spNum      = read <$> many1 (oneOf ['0'..'9'])
spString   = smlBacktick <|> smlBracketed <|> spVerbatim <?> "a SelfML string"
spVerbatim = many1 $ satisfyNone [flip elem "[](){}#`,=!*<>$", isSpace]

parens      p  = char   '('  *> p <* char   ')'
satisfyNone fs = satisfy $ \ c -> not $ any ($ c) fs