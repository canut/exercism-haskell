module Acronym (abbreviate) where

import Data.Char

abbreviate :: String -> String
abbreviate s = filter isUpper (unwords $ map capitalize_or_reduce (words $ map (\c -> if c == '-' then ' ' else c) s))
                    where capitalize_or_reduce (x:xs)
                            | all isUpper (x:xs) = [x]
                            | otherwise          = (toUpper x) : xs