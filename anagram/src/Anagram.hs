module Anagram (anagramsFor) where

import Data.Char
import Data.List

toLowerString :: String -> String
toLowerString = map toLower

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = [word | word <- xss,
                             length word == length xs,
                             toLowerString word /= toLowerString xs,
                             sort (toLowerString word) == sort (toLowerString xs)
                             ]