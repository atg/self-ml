module Data.SelfML (
  -- * The SelfML document model
  Tree(Node, Terminal),
  Forest,
  isTerminal,
  isNode,
  children,
  withChildren,
  value,
  withValue,
  -- * Reading and printing SelfML trees
  readSML,
  showSML,
  showSMLString,
  prettyPrintSML) where

import Control.Applicative
import Data.Char (isSpace)
import Data.List
import Data.SelfML.Parser
import Text.Parsec (ParseError, parse, eof)

-- | Reads SelfML out of a string.
readSML :: String -> Either ParseError (Forest String)
readSML = parse (smlForest smlString <* eof) ""

-- | Serializes into a SelfML string.
showSML :: Forest String -> String
showSML [Node x []]  = "(" ++ showSMLString x ++ ")"
showSML [Node x ts]  = concat [ '(' : showSMLString x
                              , ' ' : showSML ts
                              , ")" ]
showSML [Terminal s] = showSMLString s
showSML xs           = intercalate " " $ map (showSML . singleton) xs

-- | Serializes into a SelfML string with indentation to make it more human-readable.
prettyPrintSML :: Forest String -> String
prettyPrintSML [Node x []]  = "(" ++ showSMLString x ++ ")"
prettyPrintSML [Node x ts]  = concat [ '(' : showSMLString x
                                     , concatMap (\(Terminal s) -> ' ' : showSMLString s) as
                                     , if null bs
                                          then ""
                                          else '\n' : (indent 4 $ intercalate "\n"
                                                                $ map (prettyPrintSML . singleton) bs)
                                     , ")" ]
  where (as,bs) = span isTerminal ts
prettyPrintSML [Terminal s] = showSMLString s
prettyPrintSML xs           = intercalate "\n\n" $ map (prettyPrintSML . singleton) xs

-- | Formats a string in SelfML syntax.
showSMLString :: String -> String
showSMLString s
  | null s = "[]"
  | any ((||) <$> (`elem` "[](){}#`") <*> isSpace) s =
      case parse (smlBracketed <* eof) "" ("[" ++ s ++ "]") of
        Left  _ -> "`" ++ concatMap (\c -> case c of { '`' -> "``"; _ -> [c] }) s ++ "`"
        Right _ -> "[" ++ s ++ "]"
  | otherwise = s

instance Functor Tree where
  fmap f (Node x ts)  = Node (f x) (map (fmap f) ts)
  fmap f (Terminal x) = Terminal (f x)

-- | 'True' if the node is 'Terminal'.
isTerminal (Terminal _) = True
isTerminal _            = False

-- | 'True' if the node is 'Node'.
isNode                  = not . isTerminal

-- | Returns the value (left side) of a node.
value :: Tree a -> a
value (Node x _)   = x
value (Terminal x) = x

-- | Returns the children of a node. If the node is 'Terminal', then it returns '[]'.
children :: Tree a -> Forest a
children (Node _ ts) = ts
children _           = []

-- | Modifies the children of a node. If the node is 'Terminal', nothing is changed.
withChildren :: (Forest a -> Forest a) -> Tree a -> Tree a
withChildren f (Node x ts)  = Node x (f ts)
withChildren f (Terminal x) = Terminal x

-- | Modifies the value (left side) of a node.
withValue :: (a -> a) -> Tree a -> Tree a
withValue f (Node x ts)  = Node (f x) ts
withValue f (Terminal x) = Terminal (f x)

singleton x = [x]

indent n = (replicate n ' ' ++) . concatMap (\c -> case c of
                                                '\n' -> '\n' : replicate n ' '
                                                _    -> [c])