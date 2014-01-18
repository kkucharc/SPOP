module Pliki where

import Model
import Control.Exception
import System.IO
import System.IO.Error
import Data.Char

-- funkcja zapisujaca do pliku
zapisz ::  String -> LZad -> IO()
zapisz nazwaPliku (LZ l) = do
    if (LZ l) == empty then putStrLn "Brak zadan na liscie" 
        else do 
            writeFile nazwaPliku (concat (map (\x -> pobierzZadanie x ++ "\n" ) l))
            putStrLn ("Zapisano do pliku " ++ nazwaPliku)

-- funkcja parsujaca linie
czysc :: [String] -> [String]
czysc (x:xs) = do
    line <- map (\y -> trim y) (czyscPrzecinki x ',')
    y <- czyscDate line
    y:(czysc xs)

czysc2 :: String -> [String]
czysc2 x = do
    line <- map (\y -> trim y) (czyscPrzecinki x ',')
    czyscDate line

-- funkcja eliminujaca delimitery
czyscPrzecinki :: String -> Char -> [String]
czyscPrzecinki [] delim = [""]
czyscPrzecinki (c:cs) delim
       | c == delim = "" : rest
       | otherwise = (c : head rest) : tail rest
          where
             rest = czyscPrzecinki cs delim

-- funkcja przetwarzajaca format (xx-yy-zzzz) na String
czyscDate :: String -> [String]
czyscDate [] = [""]
czyscDate (c:cs)
       | c == '(' || c == ')' = rest 
       | c == '-' = "" : rest
       | otherwise = (c : head rest) : tail rest
          where
             rest = czyscDate cs

-- trymer bialych znakow
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace
