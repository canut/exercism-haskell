module Hamming (distance) where

import Control.Monad

distance :: String -> String -> Maybe Int
distance [] [] = Just 0
distance xs ys
    | length xs /= length ys = Nothing
    | head xs == head ys     = distance (tail xs) (tail ys)
    | otherwise              = fmap sum $ sequence [Just 1, distance (tail xs) (tail ys)]