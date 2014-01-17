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

-- funkcja wczytujaca plik
wczytaj :: FilePath -> LZad -> IO LZad
wczytaj nazwaPliku (LZ l) = do
    file <- readFile nazwaPliku
    let line = tworzZadaniePlik (czytaj (lines file))
    return (insert line (LZ l))

-- funkcja parsujaca linie
--czytaj :: [String] -> (String,String,String,String,String,String)
--czytaj :: [String] -> Zadanie
czytaj :: [String] -> [String]
czytaj (x:xs) = do 
    line <- ( map (\y -> trim y) (czyscPrzecinki x ','))
    --tworzZadaniePlik (czyscDate line)
    czyscDate line
    
    

czyscPrzecinki :: String -> Char -> [String]
czyscPrzecinki [] delim = [""]
czyscPrzecinki (c:cs) delim
       | c == delim = "" : rest
       | otherwise = (c : head rest) : tail rest
          where
             rest = czyscPrzecinki cs delim

czyscDate :: String -> [String]
czyscDate [] = [""]
czyscDate (c:cs)
       | c == '(' || c == ')' = rest 
       | c == '-' = "" : rest
       | otherwise = (c : head rest) : tail rest
          where
             rest = czyscDate cs

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

