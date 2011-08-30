module Data.SelfML.SelfPath (
  SelfPath(..),
  Query(..),
  (<//>),
  (<$/>),
  transformSML,
  parseSelfPath,
  filterSelfPath,
  mapSelfPath,
  deepQuery,
  shallowQuery,
  deepMapQuery,
  shallowMapQuery) where

import Control.Applicative
import Data.List
import Data.SelfML
import Data.SelfML.SelfPath.Parser
import Text.Parsec (ParseError)

-- | Filters through a SelfML forest using a SelfPath query string.
-- 
-- Example:
-- 
-- @
--     'readSML' "(greetings (hello alexgordon) (hello locks))" '>>=' ('<//>' "hello > #1")
-- @
-- 
-- Evaluates to:
-- 
-- @
--     'Right' ['Terminal' "alexgordon",'Terminal' "locks"]
-- @
(<//>) :: Forest String                     -- ^ Forest to match against
       -> String                            -- ^ SelfPath to compile
       -> Either ParseError (Forest String) -- ^ Result of the filter operation
f <//> sp = case parseSelfPath sp of
              Left err       -> Left err
              Right Anything -> Right f
              Right sp'      -> Right (filterSelfPath sp' (Node "" f))

-- | Maps over a SelfML tree using a map function and a SelfPath query string.
(<$/>) :: (Tree String -> Tree String)      -- ^ Map function
       -> String                            -- ^ SelfPath to compile
       -> Forest String                     -- ^ Forest to match & map over
       -> Either ParseError (Forest String) -- ^ Result of the map operation
f <$/> sp = \ fo -> cs . ($ Node "" fo) . ($ f) . mapSelfPath <$> parseSelfPath sp
  where cs (Node "" ts) = ts
        cs n@_          = [n]

-- | Runs a SelfPath query on some serialized SelfML text, mapping
--   matches through the function, and returns the pretty-printed
--   result.
transformSML :: (Tree String -> Tree String) -- ^ Map function
             -> String                       -- ^ SelfPath to compile
             -> String                       -- ^ Serialized SelfML text to transform
             -> Either ParseError String     -- ^ Pretty-printed transformation if successful
transformSML f sp i = prettyPrintSML <$> (readSML i >>= f <$/> sp)

-- | Runs a SelfPath query and returns a list of all matches.
filterSelfPath :: SelfPath -> Tree String -> Forest String
filterSelfPath (Recursive  q  sp) t = deepQuery    q t >>= filterSelfPath sp
filterSelfPath (Direct     q  sp) t = shallowQuery q t >>= filterSelfPath sp
filterSelfPath (Lookahead  ie sp) t = if null (filterSelfPath ie t)
                                         then []
                                         else filterSelfPath sp t
filterSelfPath (NLookahead ie sp) t = if null (filterSelfPath ie t)
                                         then filterSelfPath sp t
                                         else []
filterSelfPath (MEither    sa sb) t = filterSelfPath sa t ++ filterSelfPath sb t
filterSelfPath Anything           t = [t]

-- | Runs a SelfPath query and maps all matches through the function.
mapSelfPath :: SelfPath -> (Tree String -> Tree String) -> Tree String -> Tree String
mapSelfPath (Recursive  q  sp) f t = deepMapQuery    q (mapSelfPath sp f) t
mapSelfPath (Direct     q  sp) f t = shallowMapQuery q (mapSelfPath sp f) t
mapSelfPath (Lookahead  ie sp) f t = if null (filterSelfPath ie t)
                                        then t
                                        else mapSelfPath sp f t
mapSelfPath (NLookahead ie sp) f t = if null (filterSelfPath ie t)
                                        then mapSelfPath sp f t
                                        else t
mapSelfPath (MEither    sa sb) f t = mapSelfPath sb f (mapSelfPath sa f t)
mapSelfPath Anything           f t = f t

-- | Recursively matches a query through a tree.
deepQuery :: Query -> Tree String -> Forest String
deepQuery q n = shallowQuery q n ++ (children n >>= deepQuery q)

-- | Matches a query on the top level of a tree.
shallowQuery :: Query -> Tree String -> Forest String
shallowQuery      (Named s)    (Node _ ts)  = filter f ts
  where f (Node x _) = x == s
        f _          = False
shallowQuery (Not (Named s))   (Node _ ts)  = filter f ts
  where f (Node x _) = x /= s
        f _          = False
shallowQuery      (Equal t)    (Node _ ts)  = filter (== t)    ts
shallowQuery (Not (Equal t))   (Node _ ts)  = filter (/= t)    ts
shallowQuery      (At    i)    n            = filterWithIndex (const . (== i))                (nodeToList n)
shallowQuery (Not (At    i))   n            = filterWithIndex (const . (/= i))                (nodeToList n)
shallowQuery      (AtEq  i e)  n            = filterWithIndex (\ i' e' -> i' == i && e' == e) (nodeToList n)
shallowQuery (Not (AtEq  i e)) n            = filterWithIndex (\ i' e' -> i' == i && e' /= e) (nodeToList n)
shallowQuery      (Range a b)  n            = filterWithIndex (const . andF [(>= a),(<= b)])  (nodeToList n)
shallowQuery (Not (Range a b)) n            = filterWithIndex (const . andF [(<  a),(>  b)])  (nodeToList n)
shallowQuery      Child        (Node _ ts)  = ts
shallowQuery (Not Child)       _            = []
shallowQuery (Not (Not q))     t            = shallowQuery q t
shallowQuery _         _                    = []

-- | Recursively maps a query through a tree.
deepMapQuery :: Query -> (Tree String -> Tree String) -> Tree String -> Tree String
deepMapQuery q f = withChildren (map $ deepMapQuery q f) . shallowMapQuery q f

-- | Maps a query through the top level of a tree.
shallowMapQuery :: Query -> (Tree String -> Tree String) -> Tree String -> Tree String
shallowMapQuery      (Named s)    f t = withChildren (mapIf (andF [isNode, (== s).value]) f) t
shallowMapQuery (Not (Named s))   f t = withChildren (mapIf (andF [isNode, (/= s).value]) f) t
shallowMapQuery      (Equal e)    f t = withChildren (mapIf (== e) f) t
shallowMapQuery (Not (Equal e))   f t = withChildren (mapIf (/= e) f) t
shallowMapQuery      (At    0)    f t = withValue    (value.f.Terminal) t
shallowMapQuery (Not (At    0))   f t = withChildren (map f) t
shallowMapQuery      (At    i)    f t = withChildren (mapIf' (== i-1) f) t
shallowMapQuery (Not (At    i))   f t = withChildren (mapIf' (/= i-1) f) t
shallowMapQuery      (AtEq  0 e)  f t = withValue    (value . applyIf (== e) f . Terminal) t
shallowMapQuery (Not (AtEq  0 e)) f t = withValue    (value . applyIf (/= e) f . Terminal) t
shallowMapQuery      (AtEq  i e)  f t = withChildren (mapIf' (== i-1) (applyIf (== e) f)) t
shallowMapQuery (Not (AtEq  i e)) f t = withChildren (mapIf' (== i-1) (applyIf (/= e) f)) t
shallowMapQuery      (Range a b)  f t = withChildren (mapIf' (andF [(>= a),(<= b)]) f . (Terminal (value t) :)) t
shallowMapQuery (Not (Range a b)) f t = withChildren (mapIf' (orF  [(<  a),(>  b)]) f . (Terminal (value t) :)) t
shallowMapQuery      Child        f t = withChildren (map f) t
shallowMapQuery (Not Child)       f t = t
shallowMapQuery (Not (Not q))     f t = shallowMapQuery q f t

-- | Applies the function to the argument if and only if the predicate
--   matches the argument, otherwise simply returns the argument.
applyIf p f x | p x       = f x
              | otherwise = x

-- | Applies the function to the second argument if and only if the
--   predicate matches the first argument, otherwise simply returns
--   the second argument.
applyIf' p f a b | p a       = f b
                 | otherwise = b

mapIf  p f = map          (applyIf  p f)
mapIf' p f = mapWithIndex (applyIf' p f)

-- | Like 'map', but accumulates the index while mapping and applies
--   that to the function as well.
mapWithIndex f xs = snd (mapAccumL (\ i x -> (i + 1, f i x)) 0 xs)

-- | Like 'mapWithIndex', but for 'filter'.
filterWithIndex p = reverse . snd . foldl (\ (i,l) x -> (i + 1, if p i x then x : l else l)) (0,[])

-- | Returns 'True' if the argument matches all of the predicates in the list.
andF fs = and . flip map fs . flip ($)

-- | Returns 'True' if the argument matches any of the predicates in the list.
orF  fs = or  . flip map fs . flip ($)

-- | Returns the value of the node and any children. Treats an empty
--   'Node' and a 'Terminal' the same way, so it's not lossless.
nodeToList n = Terminal (value n) : children n