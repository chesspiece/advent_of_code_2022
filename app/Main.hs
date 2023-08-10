import Day1 (day1)
import Day2 (day2)
import Day3 (day3)
import Day4 (day4)
import Day5 (day5)

chooseDay :: Int -> IO ()
chooseDay day_num = case day_num of
  1 -> day1
  2 -> day2
  3 -> day3
  4 -> day4
  5 -> day5
  _ -> error "Day with such number is unimplemented yet"

main :: IO ()
main = do
  putStrLn "Input day number to solve"
  day_number <- getLine 
  chooseDay $ read day_number
