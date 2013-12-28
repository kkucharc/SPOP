module Model where

-- biblioteki
import Data.Char
import Data.Time hiding (Day)

-- Definicja danych:
type Nazwa = String
type Dzien = Int
type Miesiac = Int
type Rok = Integer
type Godzina = TimeOfDay
-- data zadania
data DataZadania = DZ (Dzien,Miesiac,Rok) Godzina deriving (Eq, Show, Read)
-- powtarzalno�� zadania:
data Powtarzalnosc = Jednorazowe | Co_dzien | Co_tydzien | Co_miesiac | Co_rok deriving (Eq, Show, Read)
-- zadanie:
data Zadanie = Zadanie Nazwa DataZadania Powtarzalnosc deriving (Eq, Show, Read)
-- klasa listy zada�
class ListaZadan lz where
    empty :: lz -- zwraca pusta liste
    insert :: Zadanie -> lz -> lz -- dodaje zadanie do listy
    insertAll :: [Zadanie] -> lz -> lz -- dodaje liste zadan
    delete :: String -> lz -> lz -- ususwa element z listy
    deleteAll :: lz -> lz  -- usuwa wszystkie elementy z listy
    showAll :: lz -> String -- wyswietla wszystkie elementy na liscie
    findByName :: String -> lz -> [Zadanie] -- znajduje wszystkie zadania o podanej nazwie
data LZad = LZ [Zadanie] deriving (Show, Eq)
-- instancja listy zada�
instance ListaZadan LZad where
    empty = LZ []
    insert z (LZ lz) = LZ (lz ++ [z])
    insertAll [] (LZ lz) = LZ lz
    insertAll (z) (LZ lz) = LZ newLz where
        newLz = lz ++ z
    delete nazwa (LZ lz) = LZ newLz where
        newLz = concat (map (\x -> if pobierzNazwe x == nazwa then [] else [x]) lz)
    deleteAll (LZ lz) = empty
    showAll (LZ lz) = "*********\n" ++ wynik ++ "\n*********\n" where
        wynik = if (LZ lz) == empty then "Brak zadan na liscie"
            else "Zadania: " ++ el
        el = concat (map (\x -> "\n- " ++ pobierzZadanie x) lz)
    findByName nazwa (LZ lz) = filter (\x -> pobierzNazwe x == nazwa) lz
-- ******************
-- Funkcje pomocnicze
-- ******************
-- funkcja ustawiajaca aktualny dzien oraz godzine na 00:00:00
aktualnaData :: (Integer,Int,Int) -> DataZadania
aktualnaData (rok,miesiac,dzien) = DZ (dzien,miesiac,rok) (read("00:00:00")::TimeOfDay)
-- funkcja aktualizujaca aktualna date
aktualizujDate :: DataZadania -> (String,String,String,String) -> DataZadania
aktualizujDate (DZ (dzien,miesiac,rok) godzina) (nDzien,nMiesiac,nRok,nGodz) = DZ ((string2int nDzien),(string2int nMiesiac),(toInteger (string2int nRok))) (read(nGodz ++ ":00")::TimeOfDay) 
-- wyswietla aktualna date
wyswietlDate :: DataZadania -> String
wyswietlDate (DZ (dzien,miesiac,rok) godzina) = "(" ++ (show dzien) ++ "-" ++ (show miesiac) ++ "-" ++ (show rok) ++ "), " ++ (show godzina)
-- funkcja tworzaca zadanie
tworzZadanie :: (String,String,String,String,String,String) -> Zadanie
tworzZadanie (nazwa,dzien,miesiac,rok,godzina,powtarzalnosc) = Zadanie nazwa (DZ ((string2int dzien),(string2int miesiac),(toInteger (string2int rok))) (read(godzina ++ ":00")::TimeOfDay)) (string2Powtarzalnosc powtarzalnosc)
-- funkcja pobieraj�ca nazw� zadania
pobierzNazwe :: Zadanie -> String
pobierzNazwe (Zadanie nazwa dataZadania powtarzalnosc) = nazwa
-- funckja pobieraj�ca zadanie do wyswietlania
pobierzZadanie :: Zadanie -> String
pobierzZadanie (Zadanie nazwa (DZ (dzien,miesiac,rok) godzina) powtarzalnosc) = nazwa ++ ", (" ++ (show dzien) ++ "-" ++ (show miesiac) ++ "-" ++ (show rok) ++ "), " ++ (show godzina) ++ ", " ++ (powtarzalnosc2string powtarzalnosc) 
-- sprawdza czy dany string sk�ada si� tylko z liczb/liter
czyString :: String -> Bool
czyString [] = False
czyString [x] | isAlphaNum x = True
    | otherwise = False
czyString (x:xs) | isAlphaNum x = czyString xs
    | otherwise = False
-- sprawdza czy dzien
czyDzien :: String -> Bool
czyDzien [] = False
czyDzien [x] | x > '0' && x <= '9' = True
    | otherwise = False
czyDzien (x:y:ys) | x == '3' && y >= '0' && y <= '1' && ys == [] = True
    | x > '0' && x < '3' && y >= '0' && y <= '9' && ys == [] = True
    | otherwise = False
-- sprawcza czy miesiac
czyMiesiac :: String -> Bool
czyMiesiac [] = False
czyMiesiac [x] | x > '0' && x <= '9' = True
    | otherwise = False
czyMiesiac (x:y:ys) | x == '1' && y >= '0' && y < '3' && ys == [] = True
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
-- sprawdza czy poprawny okres wyst�powania zdarzenia
czyPowtarzalnosc :: String -> Bool
czyPowtarzalnosc [] = False
czyPowtarzalnosc xs | xs == "jednorazowe" || xs == "co dzien" || xs == "co tydzien" || xs == "co miesiac" || xs == "co rok" = True
    | otherwise = False
-- zamiana String w Powtarzalnosc
string2Powtarzalnosc :: String -> Powtarzalnosc
string2Powtarzalnosc [] = error "Pusty string"
string2Powtarzalnosc xs | xs == "jednorazowe" = Jednorazowe
    | xs == "co dzien" = Co_dzien
    | xs == "co tydzien" = Co_tydzien
    | xs == "co miesiac" = Co_miesiac
    | xs == "co rok" = Co_rok
    | otherwise = error "Niepoprawny string"
-- zamiana Powtarzalnosc w String
powtarzalnosc2string :: Powtarzalnosc -> String
powtarzalnosc2string p = case p of
    Jednorazowe -> "jednorazowe"
    Co_dzien -> "co dzien"
    Co_tydzien -> "co tydzien"
    Co_miesiac -> "co miesiac"
    Co_rok -> "co rok"
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