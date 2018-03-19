import Data.List



----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fpaCheck	::	String -> Bool
fpaCheck [] = True																					-- Setup Function I/O
fpaCheck 	assignment		=	let index = (length assignment - 1)														-- Get the index/depth of current node
								in ((length assignment, last assignment) `elem` fpaList)						-- Search for a tuple match of form (Int, Char) within the fmList and return
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------								
fmCheck		::	String -> Bool
fmCheck []	= False 																							-- Setup Function I/O
fmCheck		assignment		=	let index = (length assignment - 1)													-- Get the index/depth of current node
								in ((length assignment, last assignment) `elem` fmList)						-- Search for a tuple match of form (Int, Char) within the fmList and return
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------												
tntCheck	::	String -> Bool	
tntCheck [] = False																						-- Setup Function I/O
tntCheck	assignment		=	let index = (length assignment - 1)													-- Get the index/depth of current node.
								in if index == 0																		-- Check to see if index is 0.
									then False																			-- If so return False
									else if index == 7																	-- Check to see if index is 7.
										then (((assignment !! index), (assignment !! 0)) `elem` tntList) && (((assignment !! (index - 1)), (assignment !! index)) `elem` tntList)			
										-- If so attempt to find the edge case tuple, and then attempt to find find the tuple of (index - 1, index)
										else (((assignment !! (index - 1)), (assignment !! index)) `elem` tntList)	-- Attempt to find find the tuple of (index - 1, index)
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------								
checkHC		::	String -> Bool																							-- Setup Function I/O
checkHC 	assignment		= 	if fpaCheck assignment == False															-- Verify fpaCheck's return
									then False																			-- If False, return False
									else if fmCheck assignment == True													-- Else verify fmCheck's return
											then False																	-- If True, return False
											else if tntCheck assignment == True											-- Else Verify tntCheck's return
												then False																-- If True, return False
												else True																-- Else return t
--JESUSCODEJESUSCODEJESUSCODEJESUSCODEJESUSCODEJESUSCODEJESUSCODEJESUSCODEJESUSCODEJESUSCODEJESUSCODEJESUSCODEJESUSCODEJESUSCODEJESUSCODEJESUSCODE





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
  
  
fmList = [(3,'b')]
tntList = [('a','b')]
fpaList = []

rootNode = (0,"","abcd",0)

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
leafMake (x,y,z:zs,w) = if checkHC (y++[z]) == True
                          then treeGrow[(x+1,y++[z],baselist\\(y++[z]),w)]++leafMake(x,y,zs,w)
						  else leafMake(x,y,zs,w)

main = print("asdfasdf")