

factMy :: Integer -> Integer
factMy 0 = 1
factMy n = n * factMy (n - 1)

fibMy :: Integer -> Integer
fibMy 0 = 0
fibMy 1 = 1
fibMy n = fibMy (n - 1) + fibMy (n - 2)

fibMy' :: Integer -> Integer -> Integer -> Integer
fibMy' 0 _ prevprev = prevprev
fibMy' 1 prev _ = prev
fibMy' n prev prevprev = fibMy' (n - 1) (prev + prevprev) prev

-- Реализация функций sumMy и productMy
sumMy :: (Num a) => [a] -> a
sumMy = foldr (+) 0
-- sumMy [1, 2, 3, 4, 5]


productMy :: (Num a) => [a] -> a
productMy = foldr (*) 1

-- Реализация функций maxMy и minMy
maxMy :: (Ord a) => [a] -> a
maxMy [] = error "maxMy: empty list"
maxMy xs = foldr1 max xs

minMy :: (Ord a) => [a] -> a
minMy [] = error "minMy: empty list"
minMy xs = foldr1 min xs

-- Реализация функций maximumMy и minimumMy
maximumMy :: (Ord a) => [a] -> a
maximumMy [] = error "maximumMy: empty list"
maximumMy xs = foldl1 max xs

minimumMy :: (Ord a) => [a] -> a
minimumMy [] = error "minimumMy: empty list"
minimumMy xs = foldl1 min xs

-- Реализация функций evenMy и oddMy
evenMy :: Integral a => a -> Bool
evenMy x = x `mod` 2 == 0

oddMy :: Integral a => a -> Bool
oddMy x = x `mod` 2 /= 0

-- Реализация функций gcdMy и lcmMy
gcdMy :: Integral a => a -> a -> a
gcdMy a b
  | b == 0 = abs a
  | otherwise = gcdMy b (a `mod` b)

lcmMy :: Integral a => a -> a -> a
lcmMy a b = abs (a * b) `div` gcdMy a b

-- Реализация функции andMy
andMy :: [Bool] -> Bool
andMy [] = True
andMy (x:xs) = x && andMy xs

-- Реализация функции orMy
orMy :: [Bool] -> Bool
orMy [] = False
orMy (x:xs) = x || orMy xs

-- andMy [True, True, True]
-- andMy [True, False, True]
-- orMy [True, False, False]
-- orMy [False, False, False]

headMy :: [a] -> a
headMy (x:_) = x

tailMy :: [a] -> [a]
tailMy (_:xs) = xs

lastMy :: [a] -> a
lastMy [x] = x
lastMy (_:xs) = lastMy xs

initMy :: [a] -> [a]
initMy [_] = []
initMy (x:xs) = x : initMy xs

lengthMy :: [a] -> Int
lengthMy [] = 0
lengthMy (_:xs) = 1 + lengthMy xs


(!!!) :: [a] -> Int -> a
(!!!) (x:xs) 0 = x
(!!!) (_:xs) n = xs !!! (n - 1)

(+++) :: [a] -> [a] -> [a]
[] +++ ys = ys
(x:xs) +++ ys = x : (xs +++ ys)

concatMy :: [[a]] -> [a]
concatMy [] = []
concatMy (xs:xss) = xs +++ concatMy xss


takeMy :: Int -> [a] -> [a]
takeMy _ [] = []
takeMy n (x:xs)
  | n <= 0    = []
  | otherwise = x : takeMy (n - 1) xs


dropMy :: Int -> [a] -> [a]
dropMy _ [] = []
dropMy n xs
  | n <= 0    = xs
  | otherwise = dropMy (n - 1) (tailMy xs)


reverseMy :: [a] -> [a]
reverseMy [] = []
reverseMy (x:xs) = reverseMy xs +++ [x]

elemMy :: (Eq a) => a -> [a] -> Bool
elemMy _ [] = False
elemMy y (x:xs)
  | y == x    = True
  | otherwise = elemMy y xs

replicateMy :: Int -> a -> [a]
replicateMy n x
  | n <= 0    = []
  | otherwise = x : replicateMy (n - 1) x

-- Примеры вызовов наших реализованных функций
-- [1, 2, 3, 4, 5]
-- headMy [1, 2, 3, 4, 5]
-- tailMy [1, 2, 3, 4, 5]

-- lastMy [1, 2, 3, 4, 5]
-- initMy [1, 2, 3, 4, 5]
-- lengthMy [1, 2, 3, 4, 5]
-- [1, 2, 3, 4, 5] !!! [6, 7]
-- [1, 2, 3, 4, 5] +++ [6, 7]
-- concatMy [[1, 2], [3, 4], [5]]
-- takeMy 3 [1, 2, 3, 4, 5]  [-1,2,3]
-- dropMy 2 [1, 2, 3, 4, 5]   -- [3,4,5]
-- reverseMy [1, 2, 3, 4, 5]       -- [5,4,3,2,1]
-- elemMy 3 [1, 2, 3, 4, 5]            -- True
-- replicateMy 3 7


lookupMy :: Eq a => a -> b -> [(a, b)] -> b
lookupMy _ defaultValue [] = defaultValue
lookupMy key defaultValue ((k, v):rest)
    | key == k   = v
    | otherwise  = lookupMy key defaultValue rest

-- lookupMy "banana" "unknown" [("apple", "red"), ("banana", "yellow"), ("grape", "purple")]
-- "yellow"


substrMy :: [a] -> Int -> Int -> [a]
substrMy lst start end = take (end - start + 1) (drop start lst)

-- [1, 2, 3, 4, 5, 6, 7, 8, 9]
-- substrMy myList 2 5

strReplaceMy :: Eq a => [a] -> [a] -> [a] -> [a]
strReplaceMy _ _ [] = []  -- базовый случай: если третий список пуст, возвращаем пустой список
strReplaceMy from to input@(x:xs)
    | take (length from) input == from = to ++ strReplaceMy from to (drop (length from) input)
    | otherwise = x : strReplaceMy from to xs

main :: IO ()
main = do
  let inputList = "hello world hello"
      fromList = "hello"
      toList = "hi"

  putStrLn $ strReplaceMy fromList toList inputList

  putStrLn "func elemIndices 2 is "
  let myList = [1, 2, 3, 4, 2, 5, 2, 6]
  print $ elemIndices 2 myList

  putStrLn "func strPosMy "

  let inputList = "ababcab"
      toFindList = "ab"
  print $ strPosMy toFindList inputList

  putStrLn "func strRotateMy example "
  let myList = [1, 2, 3, 4, 5, 6]
  print $ strRotateMy myList 2

  let myString = "abcdefgh"
  putStrLn $ unevenHandWritingMy myString



elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices _ [] = []  -- базовый случай: если список пуст, возвращаем пустой список индексов
elemIndices x lst = findIndices 0 lst
  where
    findIndices _ [] = []  -- базовый случай: если список пуст, возвращаем пустой список индексов
    findIndices currentIndex (y:ys)
      | x == y = currentIndex : restOfIndices  -- если текущий элемент равен искомому, добавляем индекс к результату
      | otherwise = restOfIndices  -- в противном случае продолжаем поиск
      where
        restOfIndices = findIndices (currentIndex + 1) ys  -- рекурсивный вызов для оставшейся части списка



strPosMy :: Eq a => [a] -> [a] -> [Int]
strPosMy _ [] = []  -- базовый случай: если второй список пуст, возвращаем пустой список индексов
strPosMy toFind input@(x:xs)
    | take (length toFind) input == toFind = length (takeWhile (/= x) input) : restOfIndices
    | otherwise = restOfIndices
    where
        restOfIndices = strPosMy toFind xs

strRotateMy :: [a] -> Int -> [a]
strRotateMy lst 0 = lst  -- базовый случай: если количество поворотов равно 0, возвращаем исходный список
strRotateMy lst n = strRotateMy (lastElement : init lst) (n - 1)
  where
    lastElement = last lst

unevenHandWritingMy :: String -> String
unevenHandWritingMy = mapIndexed toggleCase
  where
    mapIndexed _ [] = []  -- базовый случай: если строка пуста, возвращаем пустую строку
    mapIndexed f xs = zipWith f [0..] xs  -- применяем функцию f к каждому элементу, передавая индекс
    
    toggleCase i c
      | (i + 1) `mod` 3 == 0 = if isLowerChar c then toUpperChar c else toLowerChar c  -- меняем регистр каждой третьей буквы
      | otherwise = c

    isLowerChar c = c >= 'a' && c <= 'z'  -- проверка, является ли символ строчной буквой
    toUpperChar c = toEnum (fromEnum c - fromEnum 'a' + fromEnum 'A')  -- преобразование в прописную букву
    toLowerChar c = toEnum (fromEnum c - fromEnum 'A' + fromEnum 'a')  -- преобразование в строчную букву

