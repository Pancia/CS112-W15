module Hw3 where

import Data.List

data BST k v = Empty | Node k v (BST k v) (BST k v)
             deriving (Eq)

val :: BST k v -> Maybe v
val Empty = Nothing
val (Node _ v _ _) = Just v

size :: BST k v -> Int
size Empty = 0
size (Node _ _ l r) = 1 + size l + size r

ins :: (Ord k) => k -> v -> BST k v -> BST k v
ins k v Empty = Node k v Empty Empty
ins k v (Node k' v' l r)
        | k == k' = Node k v l r
        | k < k'  = Node k' v' (ins k v l) r
        | k > k'  = Node k' v' l           (ins k v r)

instance (Show v) => Show (BST k v) where
        show Empty = ""
        show (Node k v l r) = "(" ++ show l ++ show v ++ show r ++ ")"

data JSON = JStr String
          | JNum Double
          | JArr [JSON]
          | JObj [(String, JSON)]

instance Show JSON where
        show = showJSON

-- JArr [JStr "12", JNum 23.0] => ["12",23.0]
-- JArr [JStr "12", JNum 23.0, JObj [("foo",JNum 23),("bar",JNum 42)]]
-- => ["12",23.0,{"foo":23.0,"bar":42.0}]
showJSON :: JSON -> String
showJSON (JStr s) = show s
showJSON (JNum d) = show d
showJSON (JArr a) = show a
showJSON (JObj o) = "{" ++ intercalate "," objs ++ "}"
        where objs = map showJOBJ o
              showJOBJ (s, j) = show s ++ ":" ++ show j

class Json a where
        toJson :: a -> JSON
        fromJson :: JSON -> a

instance Json Double where
        toJson d = (JNum d)
        fromJson (JNum d) = d

instance (Json a) => Json [a] where
        toJson a = (JArr (map toJson a))
        fromJson (JArr a) = map fromJson a
