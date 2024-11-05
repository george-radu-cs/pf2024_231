module Lab2 where

----------------------------------------------
-- ex. 4
----------------------------------------------
maxim :: Integer -> Integer -> Integer
maxim x y =
  if (x > y)
    then x
    else y

-- o varianta in care folosim rezultatul lui maxim de y si z, si il pasam
-- ca paremtru lui maxim. Observati folosirea de paranteze pentru a grupa
-- argumentele functiei maxim.
maxim3' x y z = maxim x (maxim y z)

-- folosind un domeniu de vizibiliate local
maxim3'' x y z =
  let u = maxim x y
   in maxim u z

-- Exemplu pentru maxim3 fara folosirea functiei maxim dintre 2 intregi
maxim3 x y z =
  if (x > y && x > z)
    then x
    else if (y > x && y > z)
           then y
           else z

-- definirea unei functii care calculeaza maximul a 4 numere, ne putem folosi
-- de functia maxim3, similar cum am definit si maxim3 folosind functia maxim
maxim4 x y z v =
  let u = maxim3 x y z
   in maxim u v

-- testam daca functia maxim4 calculeaza corect maximul a 4 numere
testMaxim4 x y z t =
  let u = maxim4 x y z t -- obtinem rezultatul functiei
    -- verificam daca rezultatul este corect, >= intrucat maximul poate aparea
    -- de mai multe ori in lista de argumente
   in (u >= x && u >= y && u >= z && u >= t)

----------------------------------------------
-- ex. 6
----------------------------------------------
-- a) o functtie cu doi parametri care calculează suma pătratelor lor;
sumaPatrate :: Integer -> Integer -> Integer
sumaPatrate x y = x * x + y * y

-- b) o functie cu un parametru ce întoarce stringul “par” dacă parametrul este
-- par s, i “impar” altfel;
paritate :: Integer -> [Char]
-- paritate :: Integer -> String -- varianta echivalenta [Char] == String
paritate x =
  if even x -- (x `mod` 2 == 0)
    then "par"
    else "impar"

-- c) o functie care calculează factorialul unui număr;
factorial :: Integer -> Integer
factorial 0 = 1 -- cazul de baza
factorial x = x * factorial (x - 1)

-- prin pattern matching se verifica cazurile si se opreste la primul care
-- respecta conditia, in cazul nostru x == 0 pentru cazul de baza
-- d) o functie care verifică dacă primul parametru este mai mare decât dublul
-- celui de-al doilea parametru;
isXGreaterThan2Y :: Integer -> Integer -> Bool
isXGreaterThan2Y x y = x > 2 * y

-- e) o functie care calculează elementul maxim al unei liste.
maximLista :: [Integer] -> Integer
maximLista [] = error "Lista vida" -- daca lista mea nu are elemente arunc o eroare
maximLista [x] = x -- cazul de baza, lista cu un singur element, maximul este acel element
-- cazul general, calculez maximul dintre primul element si maximul listei fara primul element
-- formularea (x:xs) inseamna ca x este primul element din lista, iar xs este restul listei
-- [1, 2, 3] => x = 1, xs = [2, 3]
-- puteti sa va ganditi la x:xs ca la o descompunere a listei in cap si coada
maximLista (x:xs) = maxim x (maximLista xs)

----------------------------------------------
-- ex. 7
----------------------------------------------
poly :: Double -> Double -> Double -> Double -> Double
poly a b c x = a * x ** 2 + b * x + c

----------------------------------------------
-- ex. 8
----------------------------------------------
-- folosind if
eeny :: Integer -> String
eeny n =
  if even n
    then "eeny"
    else "meeny"

-- folosing guards (se folosesc pipe-uri | pentru a separa cazurile, care se rezolva in ordine, de sus in jos, pana la
-- primul care se potriveste)
eeny2 :: Integer -> String
eeny2 n
  | even n = "eeny"
  | otherwise = "meeny"

----------------------------------------------
-- ex. 9
----------------------------------------------
-- folosim ca prima conditie mod 15, deoarece 15 este cel mai restrictiv
-- daca un numar este divizibil cu 15, atunci este divizibil si cu 3 si cu 5
-- conditiile se verifica in ordine, de sus in jos, pana la prima care se potriveste
-- astfel prevenim ca un numar sa intre pe un caz gresit, putei incerca sa schimbati ordinea pentru cazul cu guards
-- folosind if
fizzbuzzIf :: Integer -> String
fizzbuzzIf n =
  if n `mod` 15 == 0
    then "FizzBuzz"
    else if n `mod` 3 == 0
           then "Fizz"
           else if n `mod` 5 == 0
                  then "Buzz"
                  else ""

-- folosing guards
fizzbuzzGuards :: Integer -> String
fizzbuzzGuards n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 3 == 0 = "Fizz"
  | n `mod` 5 == 0 = "Buzz"
  | otherwise = ""

----------------------------------------------
-- ex. 10
----------------------------------------------
-- alte exemple de recursivitate, de observat ca la definirea folosind cazuri putem folosi orice expresie intoarce un
-- Bool pentru a selecta acel caz
-- tribonacciCazuri n
--   | n == 1 = 1
--   | n == 2 = 1
----- este echivalent cu
-- tribonacciCazuri n
--   | n <= 2 = 1
----- care este scris mai concis
-- cazuri
tribonacciCazuri :: Integer -> Integer
tribonacciCazuri n
  | n <= 2 = 1
  | n == 3 = 2
  | otherwise =
    tribonacciCazuri (n - 1)
      + tribonacciCazuri (n - 2)
      + tribonacciCazuri (n - 3)

-- ecuational
tribonacciEcuational :: Integer -> Integer
tribonacciEcuational 1 = 1
tribonacciEcuational 2 = 1
tribonacciEcuational 3 = 2
tribonacciEcuational n =
  tribonacciEcuational (n - 1)
    + tribonacciEcuational (n - 2)
    + tribonacciEcuational (n - 3)

----------------------------------------------
-- ex. 11
----------------------------------------------
binomial :: Integer -> Integer -> Integer
binomial n 0 = 1
binomial 0 k = 0
binomial n k = binomial (n - 1) k + binomial (n - 1) (k - 1)
