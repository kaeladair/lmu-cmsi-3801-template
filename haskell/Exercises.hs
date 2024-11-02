module Exercises
    ( change,
      firstThenApply,
      powers,
      meaningfulLineCount,
      Shape(..),
      volume,
      surfaceArea,
      BST(Empty),
      size,
      contains,
      insert,
      inorder
    ) where

import qualified Data.Map as Map
import Data.Text (pack, unpack, replace)
import Data.List(isPrefixOf, find)
import Data.Char(isSpace)

change :: Integer -> Either String (Map.Map Integer Integer)
change amount
    | amount < 0 = Left "amount cannot be negative"
    | otherwise = Right $ changeHelper [25, 10, 5, 1] amount Map.empty
        where
          changeHelper [] remaining counts = counts
          changeHelper (d:ds) remaining counts =
            changeHelper ds newRemaining newCounts
              where
                (count, newRemaining) = remaining `divMod` d
                newCounts = Map.insert d count counts

firstThenApply :: [a] -> (a -> Bool) -> (a -> b) -> Maybe b
firstThenApply xs p f = f <$> find p xs

powers :: Integral a => a -> [a]
powers base = iterate (base *) 1

meaningfulLineCount :: FilePath -> IO Int
meaningfulLineCount filename = do
    contents <- readFile filename
    return $ length $ filter isNotCommentOrEmpty $ lines contents
    where
        isNotCommentOrEmpty line = 
            let trimmed = dropWhile isSpace line
            in not (null trimmed) && head trimmed /= '#'

data Shape = Sphere Double | Box Double Double Double
    deriving (Show, Eq)

volume :: Shape -> Double
volume (Sphere r) = (4/3) * pi * r^3
volume (Box w l d) = w * l * d

surfaceArea :: Shape -> Double
surfaceArea (Sphere r) = 4 * pi * r^2
surfaceArea (Box w l d) = 2 * (w*l + w*d + l*d)

data BST a = Empty | Node a (BST a) (BST a)
    deriving (Eq)

instance (Show a, Eq a) => Show (BST a) where
    show Empty = "()"
    show (Node v Empty Empty) = "(" ++ show v ++ ")"
    show (Node v l r) = "(" ++ leftStr ++ show v ++ rightStr ++ ")"
        where
            leftStr = if l == Empty then "" else show l
            rightStr = if r == Empty then "" else show r

size :: BST a -> Int
size Empty = 0
size (Node _ l r) = 1 + size l + size r

contains :: Ord a => a -> BST a -> Bool
contains _ Empty = False
contains x (Node v l r)
    | x == v = True
    | x < v = contains x l
    | otherwise = contains x r

insert :: Ord a => a -> BST a -> BST a
insert x Empty = Node x Empty Empty
insert x t@(Node v l r)
    | x == v = t
    | x < v = Node v (insert x l) r
    | otherwise = Node v l (insert x r)

inorder :: BST a -> [a]
inorder Empty = []
inorder (Node v l r) = inorder l ++ [v] ++ inorder r
