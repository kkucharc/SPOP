module Pliki where

import Model
import Control.Exception
import System.IO
import System.IO.Error

zapisz ::  String -> LZad -> IO()
zapisz nazwaPliku (LZ l) = do
    if (LZ l) == empty then putStrLn "Brak zadan na liscie" 
        else do 
            writeFile nazwaPliku (concat (map (\x -> pobierzZadanie x ++ "\n" ) l))
            putStrLn ("Zapisano do pliku " ++ nazwaPliku)

wczytaj nazwaPliku (LZ l) = do
    catch (do 
            hFile <- openFile nazwaPliku ReadMode
            fileStr <- hGetContents hFile
            putStrLn (take 100 fileStr)
            hClose hFile
        ) errorHandler where
            errorHandler e =
                if isDoesNotExistError e then putStrLn ("Nie istnieje " ++ nazwaPliku)
                    else return ()