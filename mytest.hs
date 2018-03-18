import Data.List
import HardConstraints

testme = 5
baselist = "abcd"
--addMe  ::  Int->Int->Int
addMe x y = x + y

--nodeAdv [] = print("guess we're done")
--nodeAdv (x:xs) = 

cutout  ::  Eq a=>a->[a]->[a]
cutout n [] = []
cutout n (x:xs)
  | x==n     = xs
  | x/=n     = x:cutout n xs
  
  
fmList = []
tntList = []
fpaList = []

--processTree :: (Integer,[Char],[Char],Integer)->(Integer,[Char])
-- (0,[],"abc",0) = (47,"cba")

treeGrow  ::  [(Integer,[Char],[Char],Integer)] -> [(Integer,[Char],[Char],Integer)]
treeGrow [] = []
treeGrow (x:xs) = leafMake x++treeGrow xs


--code here takes a node and returns a list of that node's children if applicable
--STRUCTURE node(depth,currentassignemtn,remaining tasks,penalty value) 
--CURRENT PROBLEM: im not sure how to preserve the remaining tasks properly.
leafMake :: (Integer,[Char],[Char],Integer) -> [(Integer,[Char],[Char],Integer)]
leafMake (4,y,[],z) = [(4,y,[],z)]
leafMake (x,y,[],z) = []
leafMake (x,y,z:zs,w) = if checkHC == true
                          then treeGrow[(x+1,y++[z],baselist\\(y++[z]),w)]++leafMake(x,y,zs,w)
						  else []

main = print("asdfasdf")