{-# OPTIONS_GHC -fwarn-tabs #-}

module HaskellIntro where

import Set

-- Load this file into GHCi (say, with `ghci HaskellIntro.hs`) and type
-- `isThisWorking` at the prompt. GHCi will tell you whether it's working!

isThisWorking :: String
isThisWorking = "Yes"

--
-- Problem 1
--

lastDigit :: Integer -> Integer
lastDigit n = mod n 10

dropLastDigit :: Integer -> Integer
dropLastDigit n | n < 10  = 0
                | otherwise = div n 10

toDigits :: Integer -> [Integer]
toDigits n =  reverse (toDigitsHelper n)

toDigitsHelper :: Integer -> [Integer]
toDigitsHelper n | n <= 0    = []
	         | otherwise = (mod n 10) : (toDigitsHelper (div n 10))


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l = reverse (doubleHelper 0 (reverse l))

doubleHelper :: Int -> [Integer] -> [Integer]
doubleHelper n l       | n >= (length l)   = []
                       | (mod n 2) /= 0    = ((l !! n) * 2) : (doubleHelper (n + 1) l)
		       | otherwise         = (l !! n) : (doubleHelper (n + 1) l)

sumDigits :: [Integer] -> Integer
sumDigits l | l == [] 		= 0
            | otherwise 	= (sumInner (toDigits (head l))) + (sumDigits (tail l))

sumInner :: [Integer] -> Integer
sumInner l  | l == []           = 0
            | otherwise         = (head l) + (sumInner (tail l))


validate :: Integer -> Bool
validate n =  ((mod (sumDigits (doubleEveryOther (toDigits n))) 10) == 0)

--
-- Problem 2
--

pow :: (a -> a) -> Int -> a -> a
pow f n x | n == 0          = x
          | otherwise       = f (pow f (n - 1) x)



g :: Integer -> Integer
g n | n == 0         = 0
    | otherwise      = n - (pow g 2 (n - 1))



h :: Integer -> Integer
h n  | n == 0       = 0
     | otherwise    = n - (pow h 3 (n - 1))



d :: Int -> Integer -> Integer
d i n  | n == 0         = 0
       | otherwise      = n - pow (d i ) i (n - 1)

--
-- Problem 3
--
powerSet :: Ord a => Set a -> Set a
powerSet s       | isEmpty s         = empty
                 | otherwise         = (union (powerSet (snd (split s))) (mapSet  (union (singleton (fst (split s)))) (powerSet (snd (split s)))))
