module Tutorial4 where

import Data.Char
import Data.List
import Test.QuickCheck
import Data.Ratio


-- 1. doubles
-- a.

--helper function
double :: Int -> Int
double x = x * 2

doublesComp :: [Int] -> [Int]
doublesComp xs = [double x | x <- xs]

-- b.
doublesRec :: [Int] -> [Int]
doublesRec [] = []
doublesRec (x:xs) = double x : doublesRec xs

-- c.
doublesHO :: [Int] -> [Int]
doublesHO xs = map double xs

-- d.
prop_doubles :: [Int] -> Bool
prop_doubles xs = (doublesComp xs == doublesRec xs) && (doublesRec xs == doublesHO xs)


-- 2. aboves
-- a.
abovesComp :: Int -> [Int] -> [Int]
abovesComp n xs =  [x | x <- xs, x > n]

-- b.
abovesRec :: Int -> [Int] -> [Int]
abovesRec n [] =  []
abovesRec n (x:xs) | x > n = x : abovesRec n xs
                   | otherwise = abovesRec n xs

-- c.
abovesHO :: Int -> [Int] -> [Int]
abovesHO n xs =  filter (>n) xs

-- d.
prop_aboves :: Int -> [Int] -> Bool
prop_aboves n xs = (abovesComp n xs == abovesRec n xs) && (abovesRec n xs == abovesHO n xs)


-- 3. parity
-- a.
xor :: Bool -> Bool -> Bool
xor a b = ( a && not b ) || (not a && b)

-- b.
parityRec :: [Bool] -> Bool
parityRec [] = True
parityRec (x:xs) = xor x (parityRec xs)

-- c.
parityHO :: [Bool] -> Bool
parityHO xs = foldr xor True xs

-- d.
prop_parity :: [Bool] -> Bool
prop_parity xs =  parityRec xs == parityHO xs

-- 4. allcaps
-- a.
allcapsComp :: String -> Bool
allcapsComp xs = and [isUpper c | c <- xs, isAlpha c]

-- b.
allcapsRec :: String -> Bool
allcapsRec [] = True
allcapsRec (x:xs) | isAlpha x && isUpper x = allcapsRec xs
                  | isAlpha x && not (isUpper x) = False
                  | otherwise = allcapsRec xs


-- c.
allcapsHO :: String -> Bool
allcapsHO xs =  foldr (&&) True ( map isUpper (filter isAlpha xs))

-- d.
prop_allcaps :: String -> Bool
prop_allcaps xs =  (&&) (allcapsComp xs == allcapsRec xs ) (allcapsRec xs == allcapsHO xs)


-- ** Optional material
-- Matrix manipulation

type Matrix = [[Rational]]

-- 5
-- a.
uniform :: [Int] -> Bool
uniform [] = True
uniform (x:xs) = all (==x) xs

-- b.
valid :: Matrix -> Bool
valid [] = False -- no rows
valid rows = uniform (map length rows) && not (null (head rows))


-- 6.

---------------------------------------------

matrix1 :: Matrix
matrix1 = [[1,2,3], -- 2x3 matrix
           [4,5,6]]

matrix2 :: Matrix
matrix2 = [[6,5,4], -- 2x3 matrix
           [3,2,1]]

matrix3 :: Matrix
matrix3 = [[1,2,3], -- error matrix
           [4,5]]

matrix4 :: Matrix
matrix4 = [[10,10] -- 3x2 matrix
          ,[9,9], 
           [8,8]]

matrixA :: Matrix
matrixA = [[1,2,3],
           [4,5,6],
           [7,8,9]]

matrixB :: Matrix
matrixB = [[9,8,7],
           [6,5,4],
           [3,2,1]]

addition1 :: Matrix
addition1 = plusM matrix1 matrix2
addition2 :: Matrix
addition2 = plusM matrix1 matrix3

aB :: Matrix
aB = timesM matrixA matrixB
bA :: Matrix
bA = timesM matrixB matrixA

---------------------------------------------

width :: Matrix -> Int
width m | valid m = length (head m)
        | otherwise = 0

height :: Matrix -> Int
height m | valid m = length m
         | otherwise = 0

plusM :: Matrix -> Matrix -> Matrix
plusM m1 m2 | validated && sameHeight && sameWidth = zipWith (zipWith (+)) m1 m2
            | otherwise = error "Matrices are not of the same dimensions"
        where
            sameHeight = height m1 == height m2
            sameWidth = width m1 == width m2
            validated = valid m1 && valid m2


-- 7.
timesM :: Matrix -> Matrix -> Matrix
timesM m1 m2 | validated && validMP = [[sum (zipWith (*) row col) | col <- transpose m2] | row <- m1]
             | otherwise = error "For the multiplication AB to be valid the numbers of columns in Matrix A(m) must match the number of rows in Matrix B(m)"
        where
            validMP = width m1 == height m2 
            validated = valid m1 && valid m2

-- ** Challenge

-- 8.
-- Mapping functions
mapMatrix :: (a -> b) -> [[a]] -> [[b]]
mapMatrix f = undefined

zipMatrix :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipMatrix f = undefined

-- All ways of deleting a single element from a list
removes :: [a] -> [[a]]
removes = undefined

-- Produce a matrix of minors from a given matrix
minors :: Matrix -> [[Matrix]]
minors m = undefined

-- A matrix where element a_ij = (-1)^(i + j)
signMatrix :: Int -> Int -> Matrix
signMatrix w h = undefined

determinant :: Matrix -> Rational
determinant = undefined


cofactors :: Matrix -> Matrix
cofactors m = undefined

scaleMatrix :: Rational -> Matrix -> Matrix
scaleMatrix k = undefined

inverse :: Matrix -> Matrix
inverse m = undefined

-- Tests
identity :: Int -> Matrix
identity n = undefined

prop_inverse1 :: Rational -> Property
prop_inverse1 a = undefined

prop_inverse2 :: Rational -> Rational -> Rational
                -> Rational -> Property
prop_inverse2 a b c d = undefined

type Triple a = (a,a,a)

prop_inverse3 :: Triple Rational ->
                 Triple Rational ->
                 Triple Rational ->
                 Property
prop_inverse3 r1 r2 r3 = undefined
