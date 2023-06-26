{-# LANGUAGE ScopedTypeVariables #-}

module Main (splitList, main) where

import Control.Applicative
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Stack (Stack, stackNew, stackPeek, stackPop, stackPush)

splitList :: forall a. (Eq a) => a -> [a] -> Maybe ([a], [a])
splitList = splitListHelper []
  where
    splitListHelper :: [a] -> a -> [a] -> Maybe ([a], [a])
    splitListHelper accum bb (h : rest) =
      if h == bb
        then Just (accum, rest)
        else splitListHelper (accum ++ [h]) bb rest
    splitListHelper _ _ [] = Nothing

findNumCrates :: String -> Int
findNumCrates = flip div 3 . length

parseInput :: String -> ([String], [[Int]])
parseInput input_text = (init $ map parseInputState states, map parseInputActions actions)
  where
    (states, actions) = case (splitList "" . lines) input_text of
      Just a -> a
      Nothing -> error "Was unable to parse actions description"

parseInputActions :: String -> [Int]
parseInputActions action_desc = map (read . filter (\x -> x >= '1' && x <= '9')) $ concatMap (splitOn "to") (splitOn "from" action_desc)

parseInputState :: String -> String
parseInputState action_desc = takeNthElems 3 preproc_list
  where
    preproc_list = tail action_desc

takeNthElems :: Int -> [a] -> [a]
takeNthElems n (x : xs) = x : takeNthElems n (drop n xs)
takeNthElems _ [] = []

createNStack :: Int -> [Stack Char]
createNStack n = [stackNew | _ <- [1 .. n]]

createState :: [String] -> [Stack Char]
createState state = foldl (addStateHelper []) (createNStack (length state)) state
  where
    addStateHelper :: [Stack Char] -> [Stack Char] -> String -> [Stack Char]
    addStateHelper accum (st : stt) (ch : rest) = addStateHelper (accum ++ [new_stack]) stt rest
      where
        new_stack = case ch of
          ' ' -> st
          _ -> stackPush st ch
    addStateHelper accum [] [] = accum
    addStateHelper _ _ _ = error "Everything is wrong"

doAction :: [Stack Char] -> [Int] -> [Stack Char]
doAction stcks [a, b, c] = stcks
    where (new_state, chr) =
        case stackPop (stcks !! a) of
            Just (new_state, chr) -> (new_state, chr)
            Nothing -> (stcks !! a, ' ')
doAction _ _ = error ":(((("

main :: IO ()
main = do
  file_str <- readFile "./task_5.txt"
  (state, actions) <- pure $ parseInput file_str
  print "Yay"
