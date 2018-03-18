{-
	CPSC 449
	Assignment 2
	Hard Constraints
	Jesus Cuadra
	Version 1.03
-}

fpaCheck	::	[Char] -> Bool															-- Setup Function I/O
fpaCheck 	Assignment		= 	index :: (length Assignment - 1)						-- Get the index/depth of current node
								task :: Assignment !! index								-- Retrieve associated task
								return (my_element `(length Assignment, task)` fpaList)	-- Search for a tuple match of form (Int, Char) withing the fpaList and return
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------								
fmCheck		::	[Char] -> Bool															-- Setup Function I/O
fmCheck		Assignment		=	index :: (length Assignment - 1)						-- Get the index/depth of current node
								task :: Assignment !! index								-- Retrieve associated task
								return (my_element `(length Assignment, task)` fmList)	-- Search for a tuple match of form (Int, Char) withing the fmList and return
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------												
tntCheck	::	[Char] -> Bool															-- Setup Function I/O
tntCheck	Assignment		=	index :: (length Assignment - 1)						-- Get the index/depth of current node
							if index = 0												-- Check to see if index is 0
								return False											-- If so, ignore check and return False
								else do prevIndex :: (index - 1)						-- Else, get the previous index/depth of the node
								task :: Assignment !! index								-- Retrieve associated task
								prevTask :: Assignment !! prevIndex						-- Retrieve previous associated task
								return (my_element `(prevTask, task)` tntList)			-- Search for a tuple match of form (Int, Int) withing the tntList and return
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------								
chedkHC		::	[Char] -> Bool															-- Setup Function I/O
checkHC 	Assignment		= 	do fpaCheck Assignment									-- Call fpaCheck
									if fpaCheck == False								-- Verify fpaCheck's return
										return False									-- If False, return False
									else do fmCheck Assignment							-- Else call fmCheck
										if fmCheck == True								-- Verify fmCheck's return
											return False								-- If True, return False
										else do tntCheck Assignment						-- Else call tntCheck
											if tntCheck == True							-- Verify tntCheck's return
												return False							-- If True, return False
											else return True							-- Else return True