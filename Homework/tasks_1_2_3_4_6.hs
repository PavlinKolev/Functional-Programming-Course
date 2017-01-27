-- Task 1 =====================================================================
hailstone :: Int -> [Int]
hailstone 1 = [1]
hailstone n
  | even n = n : hailstone (n `div` 2)
  | otherwise = n : hailstone (3*n + 1)



-- Task 2 =====================================================================
count :: Int
count =  length(filter f l)
  where l = [x | x <- [15,17..99] , not (x `elem` primes)]
        f x = length(filter (condition x) primes) == 0
        primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]

condition :: Float -> Float -> Bool
condition x prime = if prime >= x then False else (sqr == fromInteger (round sqr))
  where sqr = sqrt ((x - prime)/2)



-- Task 3 =====================================================================
divisors :: Int -> [(Int, Int)]
divisors n
  | n < 2 = []
  |otherwise = divisor n 2 []

divisor :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
divisor n x []
  |n `mod` x == 0 = divisor (n `div` x) x [(x, 1)]
  |otherwise = divisor n (x + 1) []
divisor n x l@((a, b):xs)
  | n == 1 = l
  | n `mod` x /= 0 = divisor n (x + 1) l
  | otherwise = divisor (n `div` x) x new_list
  where new_list = if (a==x) then ((a, b + 1):xs) else ((x, 1):(a,b):xs)



-- Task 4 =====================================================================
intercalate :: String -> [String] -> String
intercalate _ [] = ""
intercalate _ [a] = a
intercalate x (a:b:xs) = a ++ x ++ intercalate x (b:xs)



-- Task 6 =====================================================================
longestWord :: [String] -> String
longestWord [] = ""
longestWord l = foldl1 (f l) new_list
  where new_list = [first_word_concat] ++ (tail l)
        first_word_concat = longest_concat_with (head l) (tail l)

remove_str :: String -> [String] -> [String]
remove_str _ [] = []
remove_str x (y:xs)
  | x == y = xs
  | otherwise = [y] ++ remove_str x xs

f :: [String] -> String -> String -> String
f l x y = if (length x) >= (length a) then x else a
  where a = longest_concat_with y (remove_str y l)

longest_concat_with :: String -> [String] -> String
longest_concat_with x [] = x
longest_concat_with x (y:xs)
  | (can_concat x y) && (can_concat y x) = if (length x_y_concats) > (length tail_concats) then x_y_concats else tail_concats
  | can_concat x y = if (length a) > (length tail_concats) then a else tail_concats
  | can_concat y x = if (length b) > (length tail_concats) then b else tail_concats
  | otherwise = tail_concats
  where a = longest_concat_with (concatenate x y) xs
        b = longest_concat_with (concatenate y x) xs
        x_y_concats = if (length a > length b) then a else b
        tail_concats = longest_concat_with x xs


can_concat :: String -> String -> Bool
can_concat _ "" = False
can_concat "" _ = False
can_concat x y = (last x) == (head y)

concatenate :: String -> String -> String
concatenate x "" = x
concatenate "" x = x
concatenate x y
  | (last x) == (head y) = x ++ (tail y)
  | otherwise = if (length x) >= (length y) then x else y


main :: IO()
main = do
    putStrLn "Task 1 ========================================================="
    putStr "hailstone 1 -> "
    print (hailstone 1)
    putStr "hailstone 4 -> "
    print (hailstone 4)
    putStr "hailstone 10 -> "
    print (hailstone 10)

    putStrLn ""
    putStrLn "Task 2 ========================================================="
    putStr "Count is "
    print count

    putStrLn ""
    putStrLn "Task 3 ========================================================="
    putStr "divisors 120 -> "
    print (divisors 120)
    putStr "divisors 127 -> "
    print (divisors 127)

    putStrLn ""
    putStrLn "Task 4 ========================================================="
    putStr "intercalate \", ha, \" [\"this\" \"sounds\", \"funny\"] -> "
    print (intercalate ", ha, " ["this", "sounds", "funny"])

    putStrLn ""
    putStrLn "Task 6 ========================================================="
    putStr "longestWord [\"strings\", \"safe\", \"get\", \"setting\", \"elon\"] -> "
    print (longestWord ["strings", "safe", "get", "setting", "elon"])
