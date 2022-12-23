-- function (inputs,outputs)
f = [(0,1),(1,2),(2,3),(3,4),(4,5),(1,6)]
-- domain
x = [0..4]
-- codomain
y = [1..6]




-- Outputs booleans [isFunction, isInjection, isSurjection, isBijection]




-- Returns a list of bools [Function, Injection, Surjection, Bijection]
fuType :: (Eq a, Eq b) => [(a,b)] -> [a] -> [b] -> [Bool]
fuType f x y
    | isFn f x y == False = [False, False, False, False]
    | otherwise = True : isInjection f x y : isSurjection f x y : isBijection f x y : []




{-	1st part checks all of [a] is in first part of [(a,b)] and that there is no a in [(a,b)] which is not in [a]
		i.e. subsets of each other and equal to one another
	2nd part first removes any exact repeat pairs from [(a,b)], then gets first element of pair and checks they are all different
	3rd part checks all b in [(a,b)] is a subset of [b]
-}
isFn :: (Eq a, Eq b) => [(a,b)] -> [a] -> [b] -> Bool
isFn fs xs ys = setEquals (allFst fs) xs && allDifferent (allFst (removeRepeatVals fs)) && isSubset (allSnd fs) ys

-- Checks that 2 lists are exactly equal sets (regardless of order of elements)
setEquals :: (Eq a) => [a] -> [a] -> Bool
setEquals xs ys = isSubset xs ys && isSubset ys xs

-- returns list of 1st elements of each pair in a list
allFst :: [(a,b)] -> [a]
allFst [] = []
allFst (x:xs) = fst x : allFst xs

-- Checks all elements of a list are different
allDifferent :: (Eq a) => [a] -> Bool
allDifferent [] = True
allDifferent (x:xs)
    | elem x xs = False
    | otherwise = allDifferent xs

-- Removes repeating elements from a list
removeRepeatVals :: (Eq a) => [a] -> [a]
removeRepeatVals [] = []
removeRepeatVals (x:xs)
    | elem x xs = removeRepeatVals xs
    | otherwise = x : removeRepeatVals xs

-- Checks 1st list is a subset of 2nd list
isSubset :: (Eq a) => [a] -> [a] -> Bool
isSubset [] _ = True
isSubset (x:xs) ys
    | elem x ys = isSubset xs ys
    | otherwise = False

-- returns list of 2nd elements of each pair in a list
allSnd :: [(a,b)] -> [b]
allSnd [] = []
allSnd (x:xs) = snd x : allSnd xs

-- Takes a list of pairs (function), a list of type a (domain) and a list of type b (codomain)
-- Checks if it is a function, and then checks if it is an Injection 
-- ie all elements of codomain are mapped to no more than once
isInjection :: (Eq a, Eq b) => [(a,b)] -> [a] -> [b] -> Bool
isInjection fs xs ys = isFn fs xs ys && listLengthsless2 (map (mapTo fs) ys)

-- Takes a list of lists, returns false if any list has length > 1
listLengthsless2 :: [[a]] -> Bool
listLengthsless2 [] = True
listLengthsless2 (x:ys)
    | length x > 1 = False
    | otherwise = listLengthsless2 ys

-- Takes a list of pairs and a value b,
-- Returns list of values a where b matches value
mapTo :: (Eq a,Eq b) => [(a,b)] -> b -> [a]
mapTo [] y  = []
mapTo (x:xs) y
    | snd x == y = fst x : mapTo xs y
    | otherwise = mapTo xs y

-- Takes a list of pairs (function), a list of type a (domain) and a list of type b (codomain)
-- Checks if it is a function, and then checks if it is an Surjection 
-- ie all elements of codomain are mapped to at least once
isSurjection :: (Eq a, Eq b) => [(a,b)] -> [a] -> [b] -> Bool
isSurjection fs xs ys = isFn fs xs ys && setEquals (allSnd fs) ys

-- Takes a list of pairs (function), a list of type a (domain) and a list of type b (codomain)
-- Checks if function is a  bijection
-- ie it is a function, an injection and a surjection
isBijection :: (Eq a, Eq b) => [(a,b)] -> [a] -> [b] -> Bool
isBijection fs xs ys = isInjection fs xs ys && isSurjection fs xs ys




eitherSide :: Int -> [Int]
eitherSide n
    | n <= 99 = listEitherSide' n
    | n <= 9999 = listEitherSide' n
    | n <= 99999 = listEitherSide' n
    | n <= 999999 = listEitherSide' n
    | otherwise = listEitherSide' n

-- Returns a list where first element is the smallest prime strictly larger than the input
-- and the second element is the largest prime strictly smaller than the input
listEitherSide' :: Int -> [Int]
listEitherSide' n = (getLargerPrime n) : (getSmallerPrime n) : []

-- Returns the next largest prime which is strictly smaller than the input
getSmallerPrime :: Int -> Int
getSmallerPrime n
    | isPrime (n-1) [2..(n-1)] = (n-1)
    | otherwise = getSmallerPrime (n-1)

-- Returns the next smallest prime which is strictly larger than the input
getLargerPrime :: Int -> Int
getLargerPrime n
    | isPrime (n+1) [2..(n-1)] = (n+1)
    | otherwise = getLargerPrime (n+1)

-- Checks if input is a prime or not
-- Takes a number to check if prime and a list of numbers to check if composites
isPrime :: Int -> [Int] -> Bool
isPrime _ [] = True
isPrime n (x:xs)
    | rem n x == 0 = False
    | otherwise = isPrime n (removeMults x (reduceListByFactor n x xs))

-- Removes all elements which are factors of Int provided from list
removeMults :: Int -> [Int] -> [Int]
removeMults _ [] = []
removeMults n (x:xs)
    | rem x n == 0 = removeMults n xs
    | otherwise = x : removeMults n xs

-- Removes elements from list which are larger than n / non-composite
reduceListByFactor :: Int -> Int -> [Int] -> [Int]
reduceListByFactor _ _ [] = []
reduceListByFactor n y (x:xs)
    | x <= div (n+1) y = x : reduceListByFactor n y xs
    | otherwise = []