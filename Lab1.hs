-- Dimitris Katsanos A.M 2998
import Data.List
-----------------------------------------------------------------------------------------

-- ASKHSH 1

grade :: Int->Int->Int                           

grade a b
	| a < 0 || a > 100 = -1
	| b < 0 || b > 20 = -1
	| c > 47 && a <= 47 = 47
	| c > 47 && a > 47 && c < 50 = 50
	| otherwise = c
	where c = div (8*a) 10 + b

-----------------------------------------------------------------------------------------
     
-- ASKHSH 2

-- voithitiki pou kanei anadromi 
help :: Int -> Int -> Int

help 0 0 = 0
help x y = help divx divy + if modx == mody then 1 else 0
    where (divx, modx) = (div x 10, mod x 10)
          (divy, mody) = (div y 10, mod y 10)

-- vasiki
digits :: Int -> Int -> Int

digits x y
	| help x y == 8 = 1000000
	| help x y == 7 = 100000
	| help x y == 6 = 8000
	| help x y == 5 = 300
	| help x y == 4 = 20
	| help x y == 3 = 5
	| help x y == 2 = 1
	| otherwise = 0

-----------------------------------------------------------------------------------------
     
-- ASKHSH 3

search :: Integer->Integer->Integer->Integer     

search a k m = (search1 a k m 1150) !! 0 -- an to kano 10.000 tha einai poli argo


search1 :: Integer->Integer->Integer->Integer -> [Integer]     

search1 a k m n = [ n | n <- [1..n], (n + a)^k<m^n ] -- den thelei sort

-----------------------------------------------------------------------------------------
     
-- ASKHSH 4                                   

sum2017 :: Integer -> Integer ->Integer 
sum2017 m n 	
	| m == n = (2*m)^n
	| m>n  = -1		 -- protos    teleuteos
	| otherwise = mpampis (2*m) ((2*m) + (n-m )) n


mpampis :: Integer -> Integer ->Integer -> Integer 
mpampis m n n1
	| m>n   
         =0
	| otherwise 
  	   = (m )^n1 + mpampis (m + 1 ) n n1
-------------------------------------------------------------------------------------

--askisi 3 ennalaktiki lisi
{-search :: Integer->Integer->Integer->Integer     

search a k m 
	| a < 0 && k < 0 && m<2 = -1
	| otherwise = kostas a k m 1  -- poli megalo n

kostas  :: Integer->Integer->Integer->Integer->Integer

kostas a k m n
	-- | n == 0 = c 
	| n == floor (logBase k (m^n - a) ) = n -- logBase 2 1 = 0.0
	| otherwise = kostas a k m n-1 
-}
