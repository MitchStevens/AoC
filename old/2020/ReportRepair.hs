module ReportRepair where
import Advent (readInput)
import Control.Monad (guard)

expenseReport :: [Int] -> Int
expenseReport nums = head $ do
    a <- nums
    b <- nums
    guard (a + b == 2020)
    return (a * b)


expenseReport' :: [Int] -> Int
expenseReport' nums = head $ do
    a <- nums
    b <- nums
    c <- nums
    guard (a + b + c == 2020)
    return (a * b * c)


day1 :: IO ()
day1 = do
    nums <- fmap read <$> readInput 2020 1
    print $ expenseReport nums
    print $ expenseReport' nums
