import Day1 (day1)
import Day10 (day10)
import Day11 (day11)
import Day12 (day12)
import Day2 (day2)
import Day3 (day3)
import Day4 (day4)
import Day5 (day5)
import Day6 (day6)
import Day7 (day7)
import Day8 (day8)
import Day9 (day9)

chooseDay :: Int -> IO ()
chooseDay day_num = case day_num of
    1 -> day1
    2 -> day2
    3 -> day3
    4 -> day4
    5 -> day5
    6 -> day6
    7 -> day7
    8 -> day8
    9 -> day9
    10 -> day10
    11 -> day11
    12 -> day12
    _ -> putStrLn "Day with this number is unimplemented"

main :: IO ()
main = do
    putStrLn "Input day number to solve: " >> getLine >>= chooseDay . read
