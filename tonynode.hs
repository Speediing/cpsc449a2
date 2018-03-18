
testme = 5

--addMe  ::  Int->Int->Int
addMe x y = x + y


--nodeAdv [] = print("guess we're done")
--nodeAdv (x:xs) = 

cutout  ::  Eq a=>a->[a]->[a]
cutout n [] = []
cutout n (x:xs)
  | x==n     = xs
  | x/=n     = x:cutout n xs
  
  
 --(depth,[assignment list],[remaining list], penalty) 
rootNode = (0,[],"abc",0)

--processTree :: (Integer,[Char],[Char],Integer)->(Integer,[Char])
-- (0,[],"abc",0) = (47,"cba")

--treeGrow  ::  [(Integer,[Char],[Char],Integer)] -> [(Integer,[Char],[Char],Integer)]
--treeGrow [x:xs] = leafMake x:treeGrow xs


--code here takes a node and returns a list of that node's children if applicable
--STRUCTURE node(depth,currentassignemtn,remaining tasks,penalty value) 
--CURRENT PROBLEM: im not sure how to preserve the remaining tasks properly.
leafMake :: (Integer,[Char],[Char],Integer) -> [(Integer,[Char],[Char],Integer)]
leafMake (4,y,[],z) = [(4,y,[],z)]
leafMake (x,y,[],z) = []
leafMake (x,y,z:zs,w) = [(x+1,y++[z],zs,w)]++leafMake(x,y,zs,w)

main = print("asdfasdf")