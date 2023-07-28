{-# LANGUAGE ScopedTypeVariables #-}

module Main (splitList, main) where

import Control.Lens
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Stack (Stack, stackNew, stackPeek, stackPop, stackPush)

splitList :: forall a. (Eq a) => a -> [a] -> Maybe ([a], [a])
splitList = splitListHelper []
  where
    splitListHelper :: [a] -> a -> [a] -> Maybe ([a], [a])
    splitListHelper accum splitSymb (h : rest) =
      if h == splitSymb
        then Just (accum, rest)
        else splitListHelper (accum ++ [h]) splitSymb rest
    splitListHelper _ _ [] = Nothing

parseInput :: String -> ([String], [[Int]])
parseInput input_text = (init $ map parseInputState states, map parseInputActions actions)
  where
    (states, actions) = case (splitList "" . lines) input_text of
      Just a -> a
      Nothing -> error "Was unable to parse actions description"

parseInputActions :: String -> [Int]
parseInputActions action_desc = map (read . filter (\x -> x >= '0' && x <= '9')) $ concatMap (splitOn "to") (splitOn "from" action_desc)

parseInputState :: String -> String
parseInputState action_desc = takeNthElems 3 preproc_list
  where
    preproc_list = tail action_desc

takeNthElems :: Int -> [a] -> [a]
takeNthElems n (x : xs) = x : takeNthElems n (drop n xs)
takeNthElems _ [] = []

createState :: [String] -> [Stack Char]
createState state = foldl (addStateHelper []) [stackNew | _ <- [1 .. (length (state !! 1))]] true_state
  where
    addStateHelper :: [Stack Char] -> [Stack Char] -> String -> [Stack Char]
    addStateHelper accum (st : stt) (ch : rest) = addStateHelper (accum ++ [new_stack]) stt rest
      where
        new_stack = case ch of
          ' ' -> st
          _ -> stackPush st ch
    addStateHelper accum [] [] = accum
    addStateHelper _ _ _ = error "Everything is wrong"
    true_state = reverse state

doAction :: [Stack Char] -> [Int] -> [Stack Char]
doAction stcks [0, _, _] = stcks
doAction stcks [a, b, c] = doAction ((stcks & ix bb .~ new_state) & ix cc .~ stackPush (stcks !! cc) chr) [a - 1, b, c]
  where
    (new_state, chr) = fromJust (stackPop (stcks !! bb))
    bb = b - 1
    cc = c - 1
doAction _ _ = error ":(((("

doAction2 :: [Stack Char] -> [Int] -> [Stack Char]
doAction2 stcks [a, b, c] = tail $ doAction new_stack_list [a, 1, c + 1]
  where
    new_stack_list = doAction (stackNew : stcks) [a, b + 1, 1]
doAction2 _ _ = error ":(((("

main :: IO ()
main = do
  file_str <- readFile "./task_5.txt"
  (state_inp, actions) <- pure $ parseInput file_str
  print $ map (fromJust . stackPeek) (foldl doAction (createState state_inp) actions)
  print $ map (fromJust . stackPeek) (foldl doAction2 (createState state_inp) actions)
