module P19 where
main = print result

isLeapYear year = year `mod` 4 == 0 && (year `mod` 100 /= 0 || year `mod` 400 == 0)
daysInMonths year = [31, if isLeapYear year then 29 else 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
daysInYear year = sum $ daysInMonths year

daysSinceFirstOfYear :: (Int, Int, Int) -> Int
daysSinceFirstOfYear (year, month, day) = (day-1) + (sum (take (month-1) (daysInMonths year)))

daysSinceFirstOf1900 :: (Int, Int, Int) -> Int
daysSinceFirstOf1900 (1900, 1, 1) = 0
daysSinceFirstOf1900 (year, 1, 1) = (daysInYear $ year-1) + daysSinceFirstOf1900 (year-1, 1, 1)
daysSinceFirstOf1900 date@(year, _, _) = daysSinceFirstOfYear date + daysSinceFirstOf1900 (year, 1, 1)

-- 0 == Sunday
-- 1 == Monday == 1900/1/1
weekday :: (Int, Int, Int) -> Int
weekday date = (1 + (daysSinceFirstOf1900 date)) `mod` 7

result = length [(year, month, 1) | year <- [1901..2000], month <- [1..12], weekday (year, month, 1) == 0]
