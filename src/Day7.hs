{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <&>" #-}
module Day7 (day7) where

import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import Data.Set (Set, fromList, intersection, map, member, union)

data Tree = Dir String (Set Tree) (Maybe Int) | File String Int
  deriving (Eq, Ord, Show)

compSize :: Tree -> Tree
compSize (Dir str st Nothing) = Dir str new_set (Just (sum $ Data.Set.map size new_set))
  where
    new_set = Data.Set.map compSize st
compSize (Dir str st (Just a)) = Dir str st (Just a)
compSize (File str a) = File str a

tree2list :: Tree -> [Int]
tree2list (Dir str st (Just a))= a : concatMap tree2list st
tree2list (File str a) = []

size :: Tree -> Int
size (Dir _ _ (Just a)) = a
size (Dir _ _ Nothing) = 0
size (File _ a) = a

parseInput :: [String] -> Maybe Tree -> ([String], Tree)
parseInput [] (Just tr) = ([], tr)
parseInput [] Nothing = ([], Dir "notree" (fromList []) Nothing)
parseInput (x : xs) (Just tr)
  | "$ cd .." == x = (xs, tr)
  | "dir" `isPrefixOf` x = parseInput xs (Just tr)
  | "$ ls" `isPrefixOf` x = parseInput xs (Just tr)
  | "$ cd" `isPrefixOf` x = parseInput rest (Just (Dir str1 (tr1 `union` fromList [new_tree]) Nothing))
  | otherwise = parseInput xs (Just new_tree2)
  where
    new_tree2 = Dir str1 (tr1 `union` fromList [File str_part (read sz_part)]) Nothing
    (rest, new_tree) = parseInput xs (Just (Dir str_part (fromList []) Nothing))
    Dir str1 tr1 _ = tr
    spltStr = splitOn " " x
    sz_part = head spltStr
    str_part = last spltStr
parseInput (x : xs) Nothing
  | "$ cd" `isPrefixOf` x = parseInput xs (Just (Dir str_part (fromList []) Nothing))
  | otherwise = error "Something is wrong"
  where
    spltStr = splitOn " " x
    str_part = last spltStr

day7 :: IO ()
day7 = do
  input_stream <- readFile "./task_7.txt" >>= return . lines
  let (_, test) = parseInput input_stream Nothing
  -- print $ tree2list . compSize $ test
  print $ sum . filter (<= 100000) $ (tree2list . compSize $ test)
