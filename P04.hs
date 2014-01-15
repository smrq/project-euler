module P04 where
main = print result

isPalindrome x = reverse str == str where str = show x
result = maximum [a*b | a <- [100..999], b <- [100..999], isPalindrome (a*b)]
