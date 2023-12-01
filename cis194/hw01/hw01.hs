import System.Win32 (xBUTTON1)
-- ex01
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n<0=[-1]
  | n<10=[n]
  | otherwise=(n `mod` 10):toDigitsRev(n `div` 10)
toDigits :: Integer -> [Integer]
toDigits n=reverse (toDigitsRev n)

-- ex02
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther ls=snd (doubleEveryOther' ls)
  where doubleEveryOther':: [Integer] -> (Bool,[Integer])
        doubleEveryOther' [x]=(False,[x])
        doubleEveryOther' (x:xs)= if not (fst tmp) then (True,(2*x):snd tmp) else (False,x:snd tmp)
          where tmp=doubleEveryOther' xs

-- ex03
sumDigits :: [Integer] -> Integer
sumDigits [x]=sum(toDigits x)
sumDigits (x:xs) = sum(toDigits x) + sumDigits xs

-- ex04 
validate :: Integer -> Bool
validate n=(sumDigits (doubleEveryOther(toDigits n)) `mod` 10)==0

-- ex05
type Peg = String
type Move = (Peg,Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c=[(a,b)]
hanoi n a b c=hanoi (n-1) a c b++[(a,b)]++hanoi (n-1) c b a

-- ex06 (Optional)
minTuple:: (Integer , Integer)->(Integer , Integer)->(Integer , Integer)
minTuple a b
  | fst a < fst b = a
  | fst a == fst b =if snd a<snd b then a else b
  | otherwise = b
minTupleList :: [(Integer , Integer)] -> (Integer,Integer)
minTupleList [x]=x
minTupleList (x:xs)=minTuple x (minTupleList xs)
optHanoi :: Integer -> (Integer,Integer)
optHanoi 1= (1,0)
optHanoi n= minTupleList [((2* fst (optHanoi i))+(2^(n-i))-1,i)| i <- [1..(n-1)] ]
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 1 a b c d=[(a,b)]
hanoi4 n a b c d=hanoi4 j a c b d++hanoi (n-j) a b d++hanoi4 j c b a d
  where j=snd (optHanoi n)