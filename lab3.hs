module Lab3 where

import           Data.Char
import           Data.List

----------------------------------------------
-- ex. 1
----------------------------------------------
-- a)
verifLR :: [Int] -> Bool
verifLR [] = True
verifLR (h:t) =
  let l = length (h : t)
   in even l

verifL (h:t) = even (length (h : t))

-- b)
takefinal :: [Int] -> Int -> [Int]
takefinal [] n = []
takefinal (h:t) n
  | length (h : t) < n = h : t
  | otherwise = drop (length (h : t) - n) (h : t)

-- prototipul functiei accepta orice tip de liste (nu doar de Int-uri sau Char-uri)
takeFinalUniv :: [a] -> Int -> [a]
takeFinalUniv [] n = []
takeFinalUniv (h:t) n
  | length (h : t) < n = h : t
  | otherwise = drop (length (h : t) - n) (h : t)

-- c) considerand indexarea listei de la 1
remove :: [a] -> Int -> [a]
remove [] n    = []
remove (h:t) n = take (n - 1) (h : t) ++ drop n (h : t)

----------------------------------------------
-- ex. 2
----------------------------------------------
myreplicate :: Int -> a -> [a]
myreplicate n v = l' n v [] -- ne folosim de o functie auxiliara l' pe care o definim mai jos
  where
    l' 0 v [] = [] -- caz de baza, previne loop infinit
    l' n v [] = l' (n - 1) v [v] -- construim lista daca aceasta este vida
    l' n v (h:t) -- cazul recursiv adauga variabila v pana cand lungimea listei este n
      | length t == n = h : t -- obtinem lungimea dorita - n
      | otherwise = v : l' (n - 1) v (h : t) -- apelul recursiv

-- fara recursie folosind alte functii din haskell
myreplicate1 :: Int -> a -> [a]
myreplicate1 n v = take n (repeat v)

-- folosind functia predefinita replicate, care face exact ce vrem noi
myreplicate2 :: Int -> a -> [a]
myreplicate2 n v = replicate n v

sumImp :: [Int] -> Int
sumImp [] = 0 -- lista vida
sumImp (h:t)
  | odd h = h + t' -- headul este h
  | otherwise = t'
  where
    t' = sumImp t

totalLen :: [String] -> Int
totalLen [] = 0 -- cazul de baza: lista vida
totalLen (h:t)
  -- echivalent h !! 0 == 'A', !! este operatorul de indexare
  | head h == 'A' = length h + totalLen t -- primul caracter din string este A
  | otherwise = totalLen t -- continua recursia prin lista de stringuri

----------------------------------------------
-- ex. 3
----------------------------------------------
nrVocale :: [String] -> Int
nrVocale [] = 0 -- lista vida de stringuri, deci nu avem vocale
nrVocale (h:t) -- cazul general, obtinem primul string si continuam recursia cu restul listei
  -- daca stringul este palindrom, adunam numarul de vocale din el la numarul de vocale din restul listei
  | estePalindrom h = nrVocaleCuvant h + nrVocale t
  -- daca stringul nu este palindrom, continuam recursia cu restul listei
  | otherwise = nrVocale t

-- functie care verifica daca un string este palindrom
estePalindrom :: [Char] -> Bool -- reminder: [Char] este acelasi lucru cu String
estePalindrom s = s == reverse s

-- functie care numara vocalele dintr-un string
nrVocaleCuvant :: [Char] -> Int
nrVocaleCuvant [] = 0 -- cazul de baza, stringul este gol
nrVocaleCuvant (h:t) -- cazul general, obtinem primul caracter si continuam recursia cu restul stringului
  | esteVocala h = 1 + nrVocaleCuvant t -- h este un char aici si vocala, deci adaugam 1 la numarul de vocale
  | otherwise = nrVocaleCuvant t

-- functie care verifica daca un caracter este vocala
esteVocala :: Char -> Bool
esteVocala c = c `elem` "aeiouAEIOU" -- verifica daca caracterul c se afla in lista de caractere "aeiouAEIOU"

----------------------------------------------
-- ex. 4
----------------------------------------------
addElemAfterEachEven :: Int -> [Int] -> [Int]
addElemAfterEachEven n [] = [] -- lista vida, deci nu avem elemente de adaugat
addElemAfterEachEven n (h:t) -- cazul general, obtinem primul element si continuam recursia cu restul listei
  | even h = h : n : addElemAfterEachEven n t -- daca elementul este par, il adaugam in lista, [h, n, rezultat recursiv]
  | otherwise = h : addElemAfterEachEven n t -- daca elementul nu este par, continuam recursia cu restul listei, dar il pastram pe h

----------------------------------------------
-- ex. 5
----------------------------------------------
divizori :: Int -> [Int]
-- pentru x de la 1 la n, daca n se imparte la x, atunci x este divizor al lui n
divizori n = [x | x <- [1 .. n], n `mod` x == 0]

----------------------------------------------
-- ex. 6
----------------------------------------------
listadiv :: [Int] -> [[Int]]
-- x va fi fiecare element din lista
-- partea din stanga a lui | este o expresie, deci putem folosi functii sau operatori
listadiv l = [divizori x | x <- l]

----------------------------------------------
-- ex. 7
----------------------------------------------
inIntervalRec :: Int -> Int -> [Int] -> [Int]
inIntervalRec _ _ [] = [] -- lista vida, deci nu avem elemente in interval
inIntervalRec li ls (h:t) -- cazul general, obtinem primul element si continuam recursia cu restul listei
  | li <= h && h <= ls = h : inIntervalRec li ls t
  | otherwise = inIntervalRec li ls t

inIntervalComp :: Int -> Int -> [Int] -> [Int]
inIntervalComp _ _ []  = [] -- lista vida, deci nu avem elemente in interval
inIntervalComp li ls l = [x | x <- l, li <= x && x <= ls] -- observatia filtrul din definirea listei

----------------------------------------------
-- ex. 8
----------------------------------------------
pozitiveRec :: [Int] -> Int
pozitiveRec [] = 0
pozitiveRec (h:t)
  | h > 0 = 1 + pozitiveRec t
  | otherwise = pozitiveRec t

-- am avut nevoie de length pentru a numara elementele din lista rezultata, descrierea de liste ne va genera o lista
-- de intregi ([Int]), dar noi vrem sa numaram elementele din acea lista, deci ne folosim de descrierea de liste pentru
-- a filtra elementele si apoi folosim length pentru a numara elementele din lista rezultata
pozitiveComp :: [Int] -> Int
pozitiveComp ls = length [x | x <- ls, x > 0]

----------------------------------------------
-- ex. 9
----------------------------------------------
pozitiiImpareRec :: [Int] -> [Int]
pozitiiImpareRec = pozImpRec 0

-- functie auxiliara care primeste un index si o lista de intregi si returneaza pozitiile impare din lista
pozImpRec :: Int -> [Int] -> [Int]
pozImpRec _ [] = []
pozImpRec i (h:t)
  | odd h = i : pozImpRec (i + 1) t
  | otherwise = pozImpRec (i + 1) t

pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp [] = []
pozitiiImpareComp ls = [i | (i, x) <- zip [0 .. length ls - 1] ls, odd x]

-- nu exte nevoie sa scriem length ls - 1, intrucat functia zip se opreste la lista mai scurta
-- pozitiiImpareComp ls = [i | (i, x) <- zip [0 .. ] ls, odd x]
----------------------------------------------
-- ex. 10
----------------------------------------------
multDigitsRec :: [Char] -> Int
multDigitsRec [] = 1 -- pentru siruri de caracatere "" este echivalent cu []
multDigitsRec (h:t)
  | isDigit h = digitToInt h * multDigitsRec t
  | otherwise = multDigitsRec t

multDigitsComp :: [Char] -> Int
multDigitsComp ls = product [digitToInt x | x <- ls, isDigit x]
