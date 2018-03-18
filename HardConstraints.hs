{-
	CPSC 449
	Assignment 2
	Hard Constraints
	Jesus Cuadra
	Version 1.03
-}
module HardConstraints
( checkHC
) where
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fpaCheck	::	[Char] -> Bool																							-- Setup Function I/O
fpaCheck 	Assignment		=	let index = (length Assignment - 1)														-- Get the index/depth of current node
								in return (my_element (length Assignment, last Assignment) fmList)						-- Search for a tuple match of form (Int, Char) within the fmList and return
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------								
fmCheck		::	[Char] -> Bool																							-- Setup Function I/O
fmCheck		Assignment		=	let index :: (length Assignment - 1)													-- Get the index/depth of current node
								in return (my_element (length Assignment, last Assignment) fmList)						-- Search for a tuple match of form (Int, Char) within the fmList and return
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------												
tntCheck	::	[Char] -> Bool																							-- Setup Function I/O
tntCheck	Assignment		=	let index :: (length Assignment - 1)													-- Get the index/depth of current node.
								in if index == 0																		-- Check to see if index is 0.
									then False																			-- If so return False
									else if index == 7																	-- Check to see if index is 7.
										then (my_element ((Assignment !! index), (Assignment !! 0)) tntList) && (my_element ((Assignment !! (index - 1)), (Assignment !! index)) tntList)			
										-- If so attempt to find the edge case tuple, and then attempt to find find the tuple of (index - 1, index)
										else (my_element ((Assignment !! (index - 1)), (Assignment !! index)) tntList)	-- Attempt to find find the tuple of (index - 1, index)
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------								
checkHC		::	[Char] -> Bool																							-- Setup Function I/O
checkHC 	Assignment		= 	if fpaCheck Assignment == False															-- Verify fpaCheck's return
									then False																			-- If False, return False
									else if fmCheck Assignment == True													-- Else verify fmCheck's return
											then False																	-- If True, return False
											else if tntCheck Assignment == True											-- Else Verify tntCheck's return
												then False																-- If True, return False
												else True																-- Else return True