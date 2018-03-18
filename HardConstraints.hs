{-
	CPSC 449
	assignment 2
	Hard Constraints
	Jesus Cuadra
	Version 1.03
-}
module HardConstraints
( checkHC
) where


----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fpaCheck	::	String -> Bool
fpaCheck [] = False																						-- Setup Function I/O
fpaCheck 	assignment		=	let index = (length assignment - 1)														-- Get the index/depth of current node
								in ((length assignment, last assignment) `elem` fmList)						-- Search for a tuple match of form (Int, Char) within the fmList and return
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