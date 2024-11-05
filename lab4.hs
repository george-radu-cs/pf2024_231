module Lab4 where

----------------------------------------------
-- ex. 2
----------------------------------------------
factori :: Int -> [Int]
-- trecem prin toate numerele de la 1 la n (cerinta nr pozitive) si verificam daca sunt divizori
factori n = [x | x <- [1 .. n], n `mod` x == 0]

----------------------------------------------
-- ex. 3
----------------------------------------------
-- un nr prim care are doar doi divizori pozitivi este prim
prim :: Int -> Bool
-- obtine toti divizorii unui numar ca o lista de intregi, obtine lungimea aceste liste si verifica daca este egala cu 2
prim n = length (factori n) == 2

----------------------------------------------
-- ex. 4
----------------------------------------------
numerePrime :: Int -> [Int]
-- in filtrele descrierii listei, verificam daca numarul este prim
-- puteti observa ca putem folosi orice tip de expresie care intoarce un Bool pentru a selecta elementele
numerePrime n = [x | x <- [2 .. n], prim x]

----------------------------------------------
-- ex. 5
----------------------------------------------
myzip3 :: [Int] -> [Int] -> [Int] -> [(Int, Int, Int)]
-- o solutie mai putin eficienta, deoarece folosim zip de 3 ori, dar este mai usor de inteles,
-- construim 3 liste de tupluri, in care primul element este indexul, si al doilea element este valoarea din lista
-- apoi filtram elementele (i1, x), (i2, y), (i3, z) care au acelasi index i = i1 = i2 = i3
myzip3 ls1 ls2 ls3 =
  [ (x, y, z)
  | (i1, x) <- zip [1 .. length ls1] ls1
  , (i2, y) <- zip [1 .. length ls2] ls2
  , (i3, z) <- zip [1 .. length ls3] ls3
  , i1 == i2 && i2 == i3
  ]

-- putem folosi zip intre rezultatul unui zip si lista ramasa, adica facem zip intre liste de tipul [Int] si
-- [(Int, Int)], obtinand o lista de tipul [(Int, (Int, Int))]
myzip3' :: [Int] -> [Int] -> [Int] -> [(Int, Int, Int)]
myzip3' lx ly lz = [(x, y, z) | (x, (y, z)) <- zip lx (zip ly lz)]

-- similar cu myzip3', dar folosim primul zip pe primele 2 liste
myzip3'' :: [Int] -> [Int] -> [Int] -> [(Int, Int, Int)]
myzip3'' lx ly lz = [(x, y, z) | ((x, y), z) <- zip (zip lx ly) lz]

----------------------------------------------
-- ex. 6
----------------------------------------------
firstEl :: [(a, b)] -> [a]
firstEl [] = [] -- lista vida, nu avem elemente
-- definim noi o functie lambda care primeste un tuplu si returneaza primul element din tuplu
firstEl xs = map (\(a, b) -> a) xs

-- sau putem folosim functia fst pentru a lua primul element din tuplurile/listele din xs
-- similar exista si functia snd care ia al doilea element din tupluri
firstEl' xs = map fst xs

----------------------------------------------
-- ex. 7
----------------------------------------------
sumList :: [[Int]] -> [Int]
sumList [] = [] -- primim o lista vida
-- aplicam pentru fiecare element din lista xs, functia sum, care calculeaza suma elementelor din lista
-- sum :: [Int] -> Int
sumList xs = map sum xs

----------------------------------------------
-- ex. 8
----------------------------------------------
prel2 :: [Int] -> [Int]
prel2 [] = []
prel2 xs =
  map
    -- fiecare element din lista va fi inlocuit cu un singur element, deci putem folosi functia map si definim
    -- o functie lambda care primeste un element si returneaza un element, in cazul nostru, daca elementul este par
    -- il impartim la 2, altfel il inmultim cu 2
    (\x ->
       if even x
         then div x 2
         else 2 * x)
    xs

----------------------------------------------
-- ex. 9
----------------------------------------------
ex9 :: Char -> [String] -> [String]
-- folosim functia filter, care primeste o functie pe care o va aplica pe fiecare element din lista xs
-- functia `elem` primeste un element de tip A si o lista de elemente de tip A si verifica daca elementul este in lista
-- pentru cazul nostru, putem sa fixam primul parametru al functiei elem, si sa obtinem o functie care verifica daca
-- un caracter este in lista de caractere, am fixat caracterul c dinamic (c primit ca parametru, nu fixat de noi, ex. 'a')
ex9 c xs = filter (c `elem`) xs

----------------------------------------------
-- ex. 10
----------------------------------------------
squareOddsFromList :: [Int] -> [Int]
squareOddsFromList [] = []
-- filtram mai intai lista initiala pentru a obtine o noua lista doar cu numerele impare, pe care apoi le ridicam la
-- patrat. Din nou puteti observa fixarea parametrului operatorului de ridicare la putere, 2, astfel obtinem o functie
-- x^2, cu x parametru, pe care o folosim in map
-- echivalent fara operatorul de aplicare ($), dar folosind paranteze
-- squareOddsFromList xs = map (^ 2) (filter odd xs)
squareOddsFromList xs = map (^ 2) $ filter odd xs

----------------------------------------------
-- ex. 11
----------------------------------------------
squarePosOdd :: [Int] -> [Int]
squarePosOdd [] = []
-- zip [1 ..] xs
-- zipul creeaza o lista de tupluri unde primul element este indexul iar al doilea este nr din lista
-- [2, 4, 5] => [(1, 2), (2, 4), (3, 5)]
--
-- filter (odd . fst) <lista-rezultat>
-- primul argument este o functie compusa care selecteaza un tuplu daca prima valoare din tuplu este un nr impar
-- (odd . fst) (1, 2) => odd 1 => True
-- dupa filtrare, lista va contine doar tuplurile cu index impar
--
-- map ((^ 2) . snd) <lista-rezultat>
-- map - primul arg este o functie compusa care ridica la patrat ultimul element din tuplu
squarePosOdd xs = map ((^ 2) . snd) $ filter (odd . fst) $ zip [1 ..] xs

----------------------------------------------
-- ex. 12
----------------------------------------------
numaiVocale :: [String] -> [String]
numaiVocale [] = []
-- pentru fiecare string din lista de stringuri xs, aplicam filter (elem "aeiouAEIOU") pe string (string-ul este o lista
-- de caractere), deci obtinem un string care contine doar vocalele (din nou, pentru fiecare string din lista xs)
numaiVocale xs = map (filter (`elem` "aeiouAEIOU")) xs

----------------------------------------------
-- ex. 13
----------------------------------------------
mymap :: (a -> b) -> [a] -> [b]
mymap _ []     = [] -- daca lista este vida, nu avem elemente pe care sa aplicam functia
mymap f (x:xs) = f x : mymap f xs -- aplicam functia f pe primul element si continuam recursia cu restul listei

myfilter :: (a -> Bool) -> [a] -> [a]
-- _ este un wildcard, putem sa il folosim cand nu ne intereseaza valoarea, dar vrem sa pastram parametrul
myfilter _ [] = [] -- daca lista este vida, nu avem elemente pe care sa aplicam filtrul
myfilter f (x:xs)
  | f x = x : myfilter f xs -- daca f x este True, adaugam x in lista rezultat
  | otherwise = myfilter f xs -- altfel mergem mai departe cu recursia
