module Analex
( analex
) where

import Token
import Adds

analex xs = if take 5 xs == "begin" && drop ((length xs)-3) xs == "end"
    then [("MARCADOR", "begin")] ++ analextst (take ((length (drop 5 xs))-3) (drop 5 xs)) ++ [("MARCADOR", "end")]  
    else [("ERROR", "ERROR")]

analextst [] = []
analextst (x:xs) 
    | (x == '*' && (null xs)) = [((idtoken x xs), "*")] ++ analextst xs
    | (x == '*' && (head xs) == '*') = [((idtoken x xs), "**")] ++ analextst (drop 1 xs)
    | (x == '*' && (head xs) /= '*') = [((idtoken x xs), "*")] ++ analextst xs
    | elem x literal  = [((idtoken x xs), [x]++(take (sizeliteral xs) xs))] ++ analextst (drop (sizeliteral xs) xs) 
    | elem x identificador = [((idtoken x xs), [x]++(take (sizeidentificador xs) xs))] ++ analextst (drop (sizeidentificador xs) xs)
    | otherwise = [((idtoken x xs), [x])] ++ analextst xs
    where literal = ['0'..'9']
          identificador = ['a'..'z']++['A'..'Z']