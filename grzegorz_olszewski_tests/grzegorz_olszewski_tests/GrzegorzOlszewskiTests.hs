-- Wymagamy, by moduł zawierał tylko bezpieczne funkcje
{-# LANGUAGE Safe #-}
-- Definiujemy moduł zawierający testy.
-- Należy zmienić nazwę modułu na {Imie}{Nazwisko}Tests gdzie za {Imie}
-- i {Nazwisko} należy podstawić odpowiednio swoje imię i nazwisko
-- zaczynające się wielką literą oraz bez znaków diakrytycznych.
module GrzegorzOlszewskiTests(tests) where

-- Importujemy moduł zawierający typy danych potrzebne w zadaniu
import DataTypes

-- Lista testów do zadania
-- Należy uzupełnić jej definicję swoimi testami
tests :: [Test]
tests =
  [ Test "add5" (SrcString "fun add5(n:int):int = n + 5 input n in add5(n)") (Eval [42] (Value 47))
  , Test "fib" (SrcFile "fib.pp5") (Eval [4] (Value 3))
  , Test "return5" (SrcString "fun return5():int = 5 in return5()") (Eval [] (Value 5))
  , Test "secondAndThirdPower" (SrcFile "secondAndThirdPower.pp5") (Eval [3] (Value 27))
  , Test "addPairs" (SrcFile "addPairs.pp5") (Eval [1,2,3,4] (Value 4))
  , Test "simpleMatch1" (SrcString "input x in match [x]:int list with [] -> 0 | x1::x2 -> 1") (Eval [3] (Value 1))
  , Test "simpleMatch2" (SrcString "input x in match x::1::2::3::[]:int list with [] -> 0 | x1::x2 -> 1") (Eval [3] (Value 1))
  , Test "simpleMatch3" (SrcString "match []:int list with [] -> 0 | x1::x2 -> 1") (Eval [] (Value 0))
  , Test "sum" (SrcFile "sum.pp5") (Eval [1,2,3,4,5] (Value 15))
  , Test "tail" (SrcFile "tail.pp5")  (Eval [1,2,3,4] (Value 2))
  , Test "is_prime" (SrcFile "is_prime.pp5") (Eval [5] (Value 1))
  , Test "factorial" (SrcFile "factorial.pp5") (Eval [4] (Value 24))
  , Test "to5power_mul3"(SrcFile"to5power_mul3.pp5") (Eval [2] (Value 96)) 
  ]
