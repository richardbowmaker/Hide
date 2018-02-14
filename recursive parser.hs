-- Web example https://stackoverflow.com/questions/16182833/parsec-using-between-to-parse-parens
module Main where

import Text.Parsec
import Text.Parsec.String
import Control.Applicative ((<$>))

main = undefined 

data Tree = Leaf String | Node [Tree]


parseTop :: Parser Tree
parseTop = Node <$> between (char '(') (char ')') (many parseTree)

parseTree :: Parser Tree
parseTree = node <|> leaf
  where
    node = Node <$> between (char '(') (char ')') (many parseTree)
    leaf = Leaf <$> many1 (noneOf "()")


treeToString :: Tree -> String
treeToString (Leaf s) = s
treeToString (Node ts) = "(" ++ (concat (map treeToString ts)) ++ ")"

nodes :: Tree -> [Tree]
nodes (Leaf _) = []
nodes t@(Node ts) = t : concatMap nodes ts

instance Show Tree where
  showsPrec d (Leaf x) = showString x
  showsPrec d (Node xs) = showString "(" . showList xs . showString ")"
    where
      showList [] = id
      showList (x:xs) = shows x . showList xs


parseGroups :: Parser [String]
parseGroups = map show . nodes <$> parseTree

