module Data.Path where

import Prelude

import Data.Array (null)
import Data.Array.Partial (head, tail)
import Partial.Unsafe (unsafePartial)

length :: forall a. Array a -> Int
length arr = 
    if null arr 
        then 0
        else 1 +  length (unsafePartial tail arr)


even :: Int -> Boolean
even a = 
    if mod a 2 == 0
        then true
        else false

even' :: Int -> Boolean
even' a 
    | mod a 2 == 0 = true
    | otherwise     = false


counteven :: Array Int -> Int
counteven [] = 0
counteven arr =
            if even $unsafePartial head arr 
            then counteven (unsafePartial tail arr) + 1
            else counteven (unsafePartial tail arr)

counteven' :: Array Int -> Int
counteven' [] = 0
counteven' arr 
    | even $unsafePartial head arr = counteven' (unsafePartial tail arr) + 1
    | otherwise                    = counteven' (unsafePartial tail arr)


fact :: Int -> Int
fact 0 = 1
fact n = n * fact(n - 1)

fib :: Int -> Int 
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
