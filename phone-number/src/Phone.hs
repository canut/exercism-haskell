module Phone (number) where

import Data.Char

number :: String -> Maybe String
number xs = let nums = filter isDigit xs
            in check $ drop_country nums
                where drop_country n = case n of ('1':nn) -> nn
                                                 n -> n
                      check n
                        | length n /= 10 = Nothing
                        | n!!0 == '0' || n!!0 == '1'  = Nothing
                        | n!!3 == '0' || n!!3 == '1'  = Nothing
                        | otherwise                   = Just n 