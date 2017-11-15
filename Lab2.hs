-- Dimitris Katsanos A.M. 2998
import Data.List
-----------------------------------------------------------------------------------------

-- ASKHSH 1
update :: Int->[Int]->[Int]

update n [] = [n]
update n s = (kopseIdia n s ) ++ [n]                                 

kopseIdia :: Int->[Int]-> [Int]     
kopseIdia n list = [ k | k <- list , k /=n ] 
 
-----------------------------------------------------------------------------------------

-- ASKHSH 2                              
fromTo :: Int->Int->[u]->[u]   
 
fromTo i j s 
	| (i < j) && i < 0 = take (j ) (drop (i-1) s) 
	| (i < j) && j < 0 = []
	| otherwise = take (j+1 - i ) (drop (i-1) s) 
-----------------------------------------------------------------------------------------
     
-- ASKHSH 3                                

hosum :: (Int -> Int) -> (Int -> Int)
hosum f = \x -> help (f) (x)

help :: (Int -> Int) -> Int-> Int
help f n
	| n>0 = sum [f x | x <- [-n..n]]
	| otherwise = sum [f x | x <- [n..(-n)]]

-----------------------------------------------------------------------------------------
     
-- ASKHSH 4                   

apply :: Ord u => [v->u]->[v]->[u]             

apply p s = rmdups (sort ([  z k | k <- s , z <- p]))

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs)   | x `elem` xs   = rmdups xs
                | otherwise     = x : rmdups xs



