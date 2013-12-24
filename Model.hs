module Model where

-- import Char
import Data.Char
import Data.Time hiding (Day)

-- Definicja danych:
type Nazwa = String
data Dzien = Pon | Wt | Sr | Czw | Pt | Sob | Nie deriving (Eq, Show, Enum, Read)
data Miesiac = Sty | Lut | Mar | Kwi | Maj | Cze | Lip | Sie | Wrz | Paz | Lis | Gru deriving (Eq, Show, Enum, Read)
type Rok = Int
-- data zadania
data DataZadania = DataZadania Dzien Miesiac Rok deriving (Eq, Show, Read)
type Godzina = TimeOfDay
-- powtarzalność zadania:
data Powtarzalnosc = Jednorazowe | Co_dzien | Co_tydzien | Co_miesiac | Co_rok deriving (Eq, Show, Read)
-- zadanie:
data Zadanie = Zadanie Nazwa DataZadania Godzina Powtarzalnosc deriving (Eq, Show, Read)
-- klasa listy zadań
class ListaZadan lz where
    empty :: lz -- zwraca pusta liste
    insert :: Zadanie -> lz -> lz -- dodaje zadanie do listy
    delete :: String -> lz -> lz -- ususwa element z listy
    deleteAll :: lz -> lz
data LZad = LZ [Zadanie] deriving Show
-- instancja listy zadań
instance ListaZadan LZad where
    empty = LZ []
    insert z (LZ lz) = LZ (lz ++ [z])
    delete nazwa (LZ lz) = LZ newLzap where
        newLzap = concat (map (\x -> if pobierzNazwe x == nazwa then [] else [x]) lz)
    deleteAll (LZ lz) = empty
-- ******************
-- Funkcje pomocnicze
-- ******************
-- funkcja pobierająca nazwę zadania
pobierzNazwe :: Zadanie -> String
pobierzNazwe (Zadanie nazwa dataZadania godzina powtarzalnosc) = nazwa
-- sprawdza czy dany string składa się tylko z liczb/liter
czyString :: String -> Bool
czyString [] = False
czyString [x] | isAlphaNum x = True
              | otherwise = False
czyString (x:xs) | isAlphaNum x = czyString xs
                 | otherwise = False
-- sprawdza czy poprawny dzien
czyDzien :: String -> Bool
czyDzien [] = False
czyDzien xs | xs == "Pon" || xs == "Wt" || xs == "Sr" || xs == "Czw" || xs == "Pt" || xs == "Sob" || xs == "Nie" = True
            | otherwise = False
-- sprawdza czy poprawny miesiac
czyMiesiac :: String -> Bool
czyMiesiac [] = False
czyMiesiac xs | xs == "Sty" || xs == "Lut" || xs == "Mar" || xs == "Kwi" || xs == "Maj" || xs == "Cze" || xs == "Lip" || xs == "Sie" || xs == "Wrz" || xs == "Paz" || xs == "Lis" || xs == "Gru" = True
              | otherwise = False
-- sprawdza czy liczba
czyLiczba [] = False
czyLiczba [x] | isDigit x = True
              | otherwise = False
czyLiczba (x:xs) | isDigit x = czyLiczba xs
                 | otherwise = False
-- sprawdza czy poprawna godzina (hh:mm)
czyGodzina :: String -> Bool
czyGodzina [] = False
czyGodzina (a:b:c:d:e:f) | a >= '0' && a <= '1' && (isDigit b) && c == ':' && d >= '0' && d <= '5' && (isDigit e) && f ==[] = True
                         | a == '2' && b >= '0' && b <= '3' && c == ':' && d >= '0' && d <= '5' && (isDigit e) && f == [] = True
						 | otherwise = False
czyGodzina xs | (length xs) >= 6 || (length xs) <= 4 = False
-- sprawdza czy poprawny okres występowania zdarzenia
czyPowtarzalnosc :: String -> Bool
czyPowtarzalnosc [] = False
czyPowtarzalnosc xs | xs == "jednorazowe" || xs == "co dzien" || xs == "co tydzien" || xs == "co miesiac" || xs == "co rok" = True
                    | otherwise = False
-- zamiana String w Dzien
string2Dzien :: String -> Dzien
string2Dzien [] = error "Pusty string"
string2Dzien xs = case xs of
    "Pon" -> Pon
    "Wt" -> Wt
    "Sr" -> Sr
    "Czw" -> Czw
    "Pt" -> Pt
    "Sob" -> Sob
    "Nie" -> Nie
    otherwise -> error "Niepoprawny string"
-- zamiana String w Miesiac
string2Miesiac :: String -> Miesiac
string2Miesiac [] = error "Pusty string"
string2Miesiac xs = case xs of
    "Sty" -> Sty
    "Lut" -> Lut
    "Mar" -> Mar
    "Kwi" -> Kwi
    "Maj" -> Maj
    "Cze" -> Cze
    "Lip" -> Lip
    "Sie" -> Sie
    "Wrz" -> Wrz
    "Paz" -> Paz
    "Lis" -> Lis
    "Gru" -> Gru
    otherwise -> error "Niepoprawny string"
-- zamiana String w Powtarzalnosc
string2Powtarzalnosc :: String -> Powtarzalnosc
string2Powtarzalnosc [] = error "Pusty string"
string2Powtarzalnosc xs | xs == "jednorazowe" = Jednorazowe
                  | xs == "co dzien" = Co_dzien
                  | xs == "co tydzien" = Co_tydzien
                  | xs == "co miesiac" = Co_miesiac
                  | xs == "co rok" = Co_rok
                  | otherwise = error "Niepoprawny string"
-- zamiana String w Int
string2int :: String -> Int
string2int [] = 0
string2int (x:xs) = if(checkMinus x) then -1 * change2int(xs)
                    else change2int(x:xs)

checkMinus :: Char -> Bool
checkMinus a = if(a=='-') then True
               else False
change2int [] = 0
change2int (x:xs) = if(isDigit x) then result
                    else change2int(xs)
					where result = dig * exp + change2int(xs)
					      len = length(xs)
					      exp = 10^len
					      dig = digitToInt x
