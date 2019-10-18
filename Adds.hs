module Adds
( join
, sizeliteral
, sizeidentificador
) where 

import Data.Char

join [] = []
join (x:xs) = x ++ (join xs)

sizeliteral :: String -> Int
sizeliteral [] = 0
sizeliteral (a:as) = if (isDigit a) || (a == '.')
    then 1 + sizeliteral as
    else 0

sizeidentificador :: String -> Int
sizeidentificador [] = 0
sizeidentificador (a:as) = if (isAlphaNum a)
    then 1 + sizeidentificador as
    else 0

