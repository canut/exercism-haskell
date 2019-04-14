module SecretHandshake (handshake) where

handshake :: Int -> [String]
handshake n
    | n `div` 16 == 1   = reverse $ handshake (n-16)
    | n `mod` 16 == 8  = "jump" : handshake (n-8)
    | n `mod` 8 == 4   = "close your eyes" : handshake (n-4)
    | n `mod` 4 == 2   = "double blink" : handshake (n-2)
    | n `mod` 2 == 1   = "wink" : handshake (n-1)
    | n == 0           = []
    