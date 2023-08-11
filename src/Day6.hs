{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use if" #-}
module Day6 (day6) where

parseInputString :: String -> Int
parseInputString = parseInputStringHelper 0 0 ""
  where
    parseInputStringHelper :: Int -> Int -> String -> String -> Int
    parseInputStringHelper 4 acc _ _ = acc
    parseInputStringHelper chk acc check_list (c : str) = case c `elem` check_list of
      True -> parseInputStringHelper (length new_check_list) (acc + 1) new_check_list str
      False -> parseInputStringHelper (chk + 1) (acc + 1) (check_list ++ [c]) str
      where
        new_check_list = tail . dropWhile (/= c) $ check_list ++ [c]

day6 :: IO ()
day6 = do
  input_stream <- readFile "./task_6.txt"
  print $ parseInputString input_stream
