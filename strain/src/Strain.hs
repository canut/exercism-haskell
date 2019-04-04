module Strain (keep, discard) where

discard :: (a -> Bool) -> [a] -> [a]
discard p xs
    | null xs     = xs
    | p (head xs) = discard p (tail xs)
    | otherwise   = (head xs):discard p (tail xs) 

keep :: (a -> Bool) -> [a] -> [a]
keep p xs
    | null  xs    = xs
    | p (head xs) = head xs:keep p (tail xs)
    | otherwise   = keep p (tail xs)
