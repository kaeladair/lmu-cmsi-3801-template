module Exercises
    ( change,
      firstThenApply,
      powers,
      meaningfulLineCount,
      Shape(..),
      volume,
      surfaceArea,
      -- put the proper exports here
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

-- Write your shape data type here
data Shape = Sphere Double | Box Double Double Double
    deriving (Show, Eq)

volume :: Shape -> Double
volume (Sphere r) = (4/3) * pi * r^3
volume (Box w l d) = w * l * d

surfaceArea :: Shape -> Double
surfaceArea (Sphere r) = 4 * pi * r^2
surfaceArea (Box w l d) = 2 * (w*l + w*d + l*d)

-- Write your binary search tree algebraic type here
