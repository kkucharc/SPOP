module Przypomnienia where
{-
Projekt z przedmiotu SPOP - Przypomnienia 
Katarzyna Kucharczyk
Pawe³ Matuszewski
-}
import Model
import Data.Time hiding (Day)
-- ****
-- main
-- ****
main :: IO ()
main = do
    -- ustawienie defaultowej daty:
    c <- getCurrentTime
    -- obie listy na poczatku puste oraz aktualny dzien
    -- ustawiany jest aktualny dzien,miesiac,rok oraz ustawiana jest godzina na 00:00:00
    menu(empty, empty,DZ (aktualnaData (toGregorian $ utctDay c)) (read("00:00:00")::TimeOfDay)) 
-- ***********
-- menu g³ówne
-- przyjmuje dwa parametry (liste zaplanowanych i liste zrealizowanych)
-- ***********
menu (LZ lzap, LZ lzre,aktualnyDzien)= do
    putStrLn "*****Program - Przypomnienia*****"
    putStr "Akutalna data: "
    print (wyswietlDate aktualnyDzien)
    putStrLn "***Mozliwe operacje:"
    putStrLn "*1. Zarzadzanie lista zaplanowanych zadan"
    putStrLn "*2. Zarzadzanie lista zrealizowanych zadan"
    putStrLn "*3. Zapis/odczyt zaplanowanego/zrealizowanego zadania"
    putStrLn "*4. Zmien aktualna date/godzine"
    putStrLn "*5. Zakoncz"
    putStrLn "*********************************"
    option <- getLine
    case option of
        "1" -> zarzadzanieZaplanowanymi(LZ lzap, LZ lzre,aktualnyDzien)
        "2" -> zarzadzanieZrealizowanymi(LZ lzap, LZ lzre,aktualnyDzien)
        "3" -> menuZapisOdczyt(LZ lzap, LZ lzre,aktualnyDzien)
        "4" -> do
            putStrLn "Podaj dzien (1-31):"
            dzien <- getLine
            putStrLn "Podaj miesiac (1-12):"
            miesiac <- getLine
            putStrLn "Podaj rok:"
            rok <- getLine
            putStrLn "Podaj godzine (hh:mm):"
            godzina <- getLine
            if (czyDzien dzien == False) || (czyMiesiac miesiac == False) || (czyLiczba rok == False) || (czyGodzina godzina == False) then do
                putStrLn "Blednie wprowadzone dane\nSprobuj ponownie"
                menu (LZ lzap, LZ lzre,aktualnyDzien)
            else do
                menu (LZ lzap, LZ lzre,(DZ ((string2int dzien),(string2int miesiac),(toInteger (string2int rok))) (read(godzina ++ ":00")::TimeOfDay)))
        "5" -> return()
        otherwise -> do
            putStrLn "Zla opcja!"
            menu (LZ lzap, LZ lzre,aktualnyDzien)
-- ************************************
-- menu zarz¹dzania zaplanowanych zadañ
-- ************************************
zarzadzanieZaplanowanymi(LZ lzap, LZ lzre,aktualnyDzien) = do
    putStrLn ""
    putStrLn "Zarzadzanie zaplanowanych zadan"
    putStrLn "Dostepne opcje:"
    putStrLn "1. Utworz nowe zadanie do wykonania"
    putStrLn "2. Wyswietl wszystkie zaplanowane zadania"
    putStrLn "3. Wyswietl zadania do zrealizowania w dniu dzisiejszym (w tym zalegle)"
    putStrLn "4. Wybierz zadanie jako zrealizowane (po nazwie)"
    putStrLn "5. Usun zadanie lub wszystkie zadania"
    putStrLn "6. Powrot"
    option <- getLine
    case option of
        "1" -> menuDodawania(LZ lzap, LZ lzre,aktualnyDzien)
        "2" -> do 
            putStrLn (showAll (LZ lzap))
            zarzadzanieZaplanowanymi(LZ lzap, LZ lzre,aktualnyDzien)
        -- TODO: na razie opcja 4 dodaje element do listy zrealizowanych
        -- Nale¿y dodaæ usuwanie/zmianê daty w zale¿noœci od powtarzalnoœci zadania
        "4" -> do
            putStrLn "Podaj nazwe"
            nazwa <- getLine
            if czyString nazwa == False then do
                putStrLn "Bledny ciag znakow\nSprobuj ponownie."
                zarzadzanieZaplanowanymi(LZ lzap, LZ lzre,aktualnyDzien)
            else do
                zarzadzanieZaplanowanymi(LZ lzap, insertAll (findByName nazwa (LZ lzap)) (LZ lzre),aktualnyDzien)
        "5" -> menuUsuwania(LZ lzap, LZ lzre, 0,aktualnyDzien)
        "6" -> menu(LZ lzap, LZ lzre,aktualnyDzien)
        otherwise -> do
            putStrLn "Zla opcja!"
            zarzadzanieZaplanowanymi(LZ lzap, LZ lzre,aktualnyDzien)
-- **********************
-- menu dodawania zadania
-- **********************
menuDodawania(LZ lzap, LZ lzre,aktualnyDzien) = do
    putStrLn ""
    putStrLn "Wprowadz nazwe zadania"
    nazwa <- getLine
    putStrLn "Wprowadz date zadania"
    putStrLn "Dzien: (1-31)"
    dzien <- getLine
    putStrLn "Miesiac: (1-12)"
    miesiac <- getLine
    putStrLn "Rok:"
    rok <- getLine
    putStrLn "Wprowadz godzine (hh:mm)"
    godzina <- getLine
    putStrLn "Wprowadz powtarzalnosc zdarzenia (jednorazowe/co dzien/co tydzien/co miesiac/co rok"
    okres <- getLine
    if (czyString nazwa == False) || (czyDzien dzien == False) || (czyMiesiac miesiac == False) || (czyLiczba rok == False) || (czyGodzina godzina == False)|| (czyPowtarzalnosc okres == False) then do
        putStrLn "Wprowadzono bledne dane!"
        putStrLn "Powrot do zarzadzania zaplanowanymi zadaniami"
        zarzadzanieZaplanowanymi(LZ lzap, LZ lzre,aktualnyDzien)
	else do
        putStrLn "Stworzono obiekt"
        print (Zadanie nazwa (DZ ((string2int dzien),(string2int miesiac),(toInteger (string2int rok))) (read(godzina ++ ":00")::TimeOfDay)) (string2Powtarzalnosc okres))
        zarzadzanieZaplanowanymi((insert (Zadanie nazwa (DZ ((string2int dzien),(string2int miesiac),(toInteger (string2int rok))) (read(godzina ++ ":00")::TimeOfDay)) (string2Powtarzalnosc okres)) (LZ lzap)), LZ lzre,aktualnyDzien)
-- *********************
-- menu usuwania zadañ
-- (lista zaplanowanych, lista zrealizowanych,tryb (0/1))
-- tryb = 0 - usuwanie zaplanowanych
-- tryb = 1 - usuwanie zrealizowanych
-- *********************
menuUsuwania(LZ lzap, LZ lzre,tryb,aktualnyDzien) = do
    putStrLn ""
    if tryb == 0 then putStrLn "Usuwanie zadan zaplanowanych"
    else putStrLn "Usuwanie zadan zrealizowanych"
    putStrLn "1. Usun zadanie o podanej nazwie"
    putStrLn "2. Usun wszystkie zadania z listy"
    putStrLn "3. Powrot do menu zarzadzania zadaniami"
    option <- getLine
    case option of
        "1" -> do
            putStrLn "Podaj nazwe"
            nazwa <- getLine
            if czyString nazwa == False then do
                putStrLn "Bledny ciag znakow\nSprobuj ponownie."
                menuUsuwania(LZ lzap, LZ lzre,tryb,aktualnyDzien)
            else do
                if tryb == 0 then menuUsuwania(delete nazwa (LZ lzap), LZ lzre,tryb,aktualnyDzien)
                else menuUsuwania(LZ lzap, delete nazwa (LZ lzre),tryb,aktualnyDzien)
        "2" -> do
            if tryb == 0 then zarzadzanieZaplanowanymi(deleteAll (LZ lzap),LZ lzre,aktualnyDzien)
            else zarzadzanieZrealizowanymi(LZ lzap, deleteAll (LZ lzre),aktualnyDzien)
        "3" -> do
            if tryb == 0 then zarzadzanieZaplanowanymi(LZ lzap, LZ lzre,aktualnyDzien)
            else zarzadzanieZrealizowanymi(LZ lzap, LZ lzre,aktualnyDzien)
        otherwise -> do
            putStrLn "Zla opcja!"
            menuUsuwania(LZ lzap, LZ lzre,tryb,aktualnyDzien)
-- *************************************
-- menu zarz¹dzania zrealizowanych zadañ
-- *************************************
zarzadzanieZrealizowanymi(LZ lzap, LZ lzre,aktualnyDzien) = do
    putStrLn ""
    putStrLn "Zarzadzanie zrealizowanych zadan"
    putStrLn "Dostepne opcje:"
    putStrLn "1. Wyswietl zrealizowane zadania"
    putStrLn "2. Usun zrealizowane zadanie lub wszystkie zadania"
    putStrLn "3. Powrot"
    option <- getLine
    case option of
        "1" -> do
            putStrLn (showAll (LZ lzre))
            zarzadzanieZrealizowanymi(LZ lzap, LZ lzre,aktualnyDzien)
        "2" -> menuUsuwania(LZ lzap, LZ lzre,1,aktualnyDzien)
        "3" -> menu(LZ lzap, LZ lzre,aktualnyDzien)
        otherwise -> do
            putStrLn "Zla opcja!"
            zarzadzanieZrealizowanymi(LZ lzap, LZ lzre,aktualnyDzien)
-- *******************
-- menu odczytu/zapisu
-- *******************
menuZapisOdczyt(LZ lzap, LZ lzre,aktualnyDzien) = do
    putStrLn ""
    putStrLn "Menu odczytu/zapisu zaplanowanych/zrealizowanych zadan"
    putStrLn "Dostepne opcje:"
    putStrLn "1. Zapisanie zaplanowanych zadan do pliku"
    putStrLn "2. Odczytanie zaplanowanych zadan z pliku"
    putStrLn "3. Zapisanie zrealizowanych zadan do pliku"
    putStrLn "4. Odczytanie zrealizowanych zadan z pliku"
    putStrLn "5. Powrot"
    option <- getLine
    case option of
        "5" -> menu(LZ lzap, LZ lzre,aktualnyDzien)
        otherwise -> do
            putStrLn "Zla opcja!"
            menuZapisOdczyt(LZ lzap, LZ lzre,aktualnyDzien)