module Impl where

import Lib

-- Remember that [a] is syntactic sugar for List a.
-- data [a] = [] | a : [a]
-- which is like
-- data List a = Nil | Cons a (List a)

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

sum' :: [Integer] -> Integer
sum' [] = 0
sum' (x:xs) = x + sum xs

reverse' :: [Integer] -> [Integer]
reverse' [] = []
reverse' (x:xs) = reverse xs ++ [x]

foldr' :: (e -> acc -> acc) -> acc -> [e] -> acc
foldr' _ n [] = n
foldr' f n (x:xs) = f x (foldr' f n xs)

foldl' :: (acc -> e -> acc) -> acc -> [e] -> acc
foldl' _ n [] = n
foldl' f n (x:xs) = foldl' f (f n x) xs
