import System.Environment
import System.Directory
import System.IO 
import Data.List
import Data.Maybe
import Data.Char
import Text.Read
import System.Exit
import Data.Bool
import Data.List.Split



main = do 
	(args) <- getArgs
	let	fileName = args !! 0 
		outPutName = args !! 1
		
	contents <- readFile fileName
	
	let	contentsList  		= (lines contents)
		contentsList' 		= noWP contentsList
		errorName			= checkName contentsList' 0 False
		errorFPA			= checkFPA errorName 0 False	
		errorFM 			= checkForbiddenMachine errorFPA 0 False
		errorTNT 			= checkTooNearTask errorFM 0 False
		errorMP 			= checkMatrix errorTNT 0 False
		errorTNP 			= checkTooNearPenalities errorMP 0 False

		
	
	if ("exit 1" `elem` errorName) 			then (errorOutPut outPutName 1 >> exitSuccess) 	else return()	
	if ("exit 1" `elem` errorFPA) 				then (errorOutPut outPutName 1 >> exitSuccess) 	else return()
	
	if ("exit 3" `elem` errorFPA) 				then (errorOutPut outPutName 3 >> exitSuccess) 	else return()
	
	let	contentsList'' 	=  removeNull $ contentsList'
		fpaString 		= splitContents contentsList'' (convertToInt $ indexHeader contentsList'' 2) (convertToInt $ indexHeader contentsList'' 1)
		fpaDupless 		= nub fpaString
	
	if 	length fpaDupless > 8 					then (errorOutPut outPutName 2 >> exitSuccess) 	else return()
	
	let	fpaList 		= makeMT fpaDupless
		invalidMT 		= removeDuplicates fpaList
	
	
	if (length invalidMT) /= (length fpaList) 		then (errorOutPut outPutName 2 >> exitSuccess) 	else return()
	

	if ("exit 1" `elem` errorFM) 				then (errorOutPut outPutName 1 >> exitSuccess) 	else return()
	if ("exit 3" `elem` errorFM) 				then (errorOutPut outPutName 3 >> exitSuccess) 	else return()
	
	let	fmString 		= splitContents contentsList'' (convertToInt $ indexHeader contentsList'' 3) (convertToInt $ indexHeader contentsList'' 2)
		fmDupless 		= nub fmString
		fmList			= makeMT fmDupless
	
	if ("exit 1" `elem` errorTNT) 				then (errorOutPut outPutName 1 >> exitSuccess) 	else return()
	if ("exit 3" `elem` errorTNT) 				then (errorOutPut outPutName 3 >> exitSuccess) 	else return()
	
	let	tntString 		= splitContents contentsList'' (convertToInt $ indexHeader contentsList'' 4) (convertToInt $ indexHeader contentsList'' 3)
		tntDupless 		= nub tntString
		tntList 		= makeTT tntDupless
		
	if ("exit 1" `elem` errorMP) 				then (errorOutPut outPutName 1 >> exitSuccess) 	else return()	
	if ("exit 4" `elem` errorMP) 				then (errorOutPut outPutName 4 >> exitSuccess) 	else return()	
	if ("exit 6" `elem` errorMP) 				then (errorOutPut outPutName 6 >> exitSuccess) 	else return()	
	
	let mpString 		= splitContents contentsList'' (convertToInt $ indexHeader contentsList'' 5) (convertToInt $ indexHeader contentsList'' 4)
	
	if (length mpString < 8) 					then (errorOutPut outPutName 4 >> exitSuccess) 	else return()	
	
	let mpList 			= map (map read) (map words (mpString)) :: [[Int]]
	
	if ("exit 1" `elem` errorTNT) 				then (errorOutPut outPutName 1 >> exitSuccess) 	else return()	
	if ("exit 5" `elem` errorTNT) 				then (errorOutPut outPutName 4 >> exitSuccess) 	else return()	
	if ("exit 6" `elem` errorTNT) 				then (errorOutPut outPutName 6 >> exitSuccess) 	else return()	
	
	let	tnpString 		= splitContents contentsList'' (5) (5)
		tnpDupless 		= nub tnpString
		tnpList' 		= makeTTP (map stripBrackets (tnpDupless))
		tnpList 		= updateValue tnpList'
		
	print fpaList
	print fmList
	print tntList
	print mpList
	print tnpString
	print tnpList
	
stripBrackets :: String -> String
stripBrackets []	= []
stripBrackets (x:xs)	| x == '(', (last xs) == ')'	= init xs
			| otherwise			= x:xs
			
countWP :: String -> Int
countWP "" = 0
countWP (x:xs)
	|x == ' ' = 1 + countWP xs
	|otherwise = 0 + countWP xs
-------------------------------------------------------------------------------------------------------------------------------	
removeDuplicates xs = nubBy cmpKeyAndVal xs 
  where
    cmpKeyAndVal (x, y) (x', y') 									= x == x' || y == y'
-------------------------------------------------------------------------------------------------------------------------------
updateValue xs = reverse (nubBy cmpKeyAndVal' (reverse xs))
	where
		cmpKeyAndVal' (x , y , z) (x'	, y' , z') = x == x' && y == y' 
-------------------------------------------------------------------------------------------------------------------------------	

makeMT :: [String] -> [(Int,Char)]
makeMT []	= []
makeMT (('(':a:',':b:')':[]):xs)									= ((machineToInt a, b):(makeMT xs))
makeMT x	= []
-------------------------------------------------------------------------------------------------------------------------------
makeTT :: [String] -> [(Char,Char)]
makeTT []	= []
makeTT (('(':a:',':b:')':[]):xs)									= ((a, b):(makeTT xs))
makeTT x	= []

makeTTP :: [String] -> [(Char,Char,Int)]
makeTTP [] = []
makeTTP ((a:',':b:',':xs):ys) 										= (a , b , (read xs :: Int)):(makeTTP ys)
makeTTP x 	= [] 
-------------------------------------------------------------------------------------------------------------------------------	
machineToInt :: Char -> Int
machineToInt x
	|x == '1'														= 1
	|x == '2' 														= 2
	|x == '3'														= 3
	|x == '4'														= 4
	|x == '5'														= 5
	|x == '6'														= 6
	|x == '7'														= 7
	|x == '8'														= 8
-------------------------------------------------------------------------------------------------------------------------------
checkMatrix :: [String] -> Int -> Bool -> [String]
checkMatrix [] _ _ = []
checkMatrix (x:xs) n m 
	|n > 8 														= "exit 4":[]
	|null x = checkMatrix xs n True								
	|leadingWhiteSpace x 											= "exit 1":[]
	|x == "too-near penalities" && m == True						= xs
	|x == "too-near penalities" && n /= 8							= "exit 1":[]
	|length (words x) /= 8 && n < 8 								= "exit 4":[]
	|(countWP x) /= 7	&& 	x /= "too-near penalities"				= "exit 1":[]
	|isVMP' (words x)		 										= checkMatrix xs (n + 1) False
	|not (isVMP' (words x)) && n <= 8								= "exit 6":[]
	|x == "too-near penalities"										= "exit 1":[]
	|otherwise 														= "exit 1":[]	
	
	
	

-- checkFormating :: String -> Bool
-- checkFormating (a:' ':a:' ':a:' ':a:' ':a:' ':a:' ':a:' ':a:[]) 

-- checkFormating  = False

{-CHECKS TO SEE IF THE TRIPLES ARE IN THE RIGHT FORMAT-}
isTriple :: String -> Bool
isTriple []		= False
isTriple x
		|head x == '(' && last x == ')' && count x == 2 				= True
		|otherwise 															= False


isVMP' :: [String] -> Bool
isVMP' [] = True
isVMP' (x:xs)
	|isNothing (readMaybe x :: Maybe Int ) 						= False
	|fromJust (readMaybe x :: Maybe Int) >= 0						= isVMP' xs
	

-------------------------------------------------------------------------------------------------------------------------------	
checkName :: [String] -> Int -> Bool -> [String]
checkName [] _ _ = []
checkName (x:xs) n o 
	|null x = checkName xs n True
	|leadingWhiteSpace x = "exit 1":[]
	|x == "Name:" 													= checkName xs (n + 1) False
	|(length (words x)) == 1 && x /= "Name:"						= checkName xs (n + 1) False
	|x == "forced partial assignment:" && n == 2 && o == True 	= xs 
	|x == "forced partial assignment:" && n /= 2 					= "exit 1":[]
	|x /= "forced partial assignment:" && n == 2 					= "exit 1":[]
	|(length (words x)) /= 1 										= "exit 1":[]
	|otherwise 														= "exit 1":[]
-------------------------------------------------------------------------------------------------------------------------------	
checkFPA :: [String] -> Int -> Bool -> [String]
checkFPA [] _ _ = []
checkFPA (x:xs) n m
	|null x 														= checkFPA xs n True
	|leadingWhiteSpace x 											= "exit 1":[]
	|x == "forbidden machine:" && m == True						= xs
	|x == "forced partial assignment:" 							= "exit 1":[]
	|isPair x && isValidMT x										= checkFPA xs n False
	|not (isPair x) && x /= "forbidden machine:"					= "exit 1":[]
	|length (words x) /= 1	&& x /= "forbidden machine:"			= "exit 1":[]
	|not (isValidMT x) && x /= "forbidden machine:" 				= "exit 3":[]
	|x == "too-near tasks:"											= "exit 1":[]
	|x == "machine penalties:"										= "exit 1":[]
	|x == "too-near penalities"										= "exit 1":[]
	|otherwise 														= "exit 1":[]	
-------------------------------------------------------------------------------------------------------------------------------
checkForbiddenMachine :: [String] -> Int -> Bool -> [String]
checkForbiddenMachine [] _ _  = []
checkForbiddenMachine (x:xs) n m		
	|null x 														= checkForbiddenMachine xs n True
	|leadingWhiteSpace x 											= "exit 1":[]
	|x == "too-near tasks:" && m == True							= xs
	|x == "forbidden machine:" 									= "exit 1":[]
	|isPair x && isValidMT x										= checkForbiddenMachine xs n False
	|not (isPair x) && x /= "too-near tasks:"						= "exit 1":[]
	|length (words x) /= 1	&& x /= "too-near task:"				= "exit 1":[]
	|not (isValidMT x) && x /= "too-near tasks:" 					= "exit 3":[]
	|x == "machine penalties:"										= "exit 1":[]
	|x == "too-near penalities"										= "exit 1":[]
	|otherwise 														= "exit 1":[]	
-------------------------------------------------------------------------------------------------------------------------------
checkTooNearTask :: [String] -> Int -> Bool -> [String]
checkTooNearTask [] _ _  = []
checkTooNearTask (x:xs) n m		
	|null x 														= checkTooNearTask xs n True
	|leadingWhiteSpace x 											= "exit 1":[]
	|x == "machine penalties:" && m == True						= xs
	|x == "too-near tasks:" 										= "exit 1":[]
	|isPair x && isValidTT x										= checkTooNearTask xs n False
	|not (isPair x) && x /= "too-near tasks:"						= "exit 1":[]
	|length (words x) /= 1	&& x /= "too-near task:"				= "exit 1":[]
	|not (isValidTT x) && x /= "too-near tasks:" 					= "exit 3":[]
	|x == "too-near penalities"										= "exit 1":[]
	|otherwise 														= "exit 1":[]
------------------------------------------------------------------------------------------------------------------------------
checkTooNearPenalities :: [String] -> Int -> Bool -> [String]
checkTooNearPenalities [] _ _  = []
checkTooNearPenalities (x:xs) n m		
	|null x 														= checkTooNearPenalities xs n True
	|leadingWhiteSpace x											= "exit 1":[]
	|x == "too-near penalities" 									= "exit 1":[]
	|isTriple x && isValidTTtoNear x && (isValidPenality $ wtfAmIDoingWithMyLife x)	  								= checkTooNearPenalities xs n False
	|not (isTriple x)												= "exit 1":[]
	|length (words x) /= 1											= "exit 1":[]
	|not (isValidTTtoNear x)   						 			= "exit 5":[]
	|not (isValidPenality $ wtfAmIDoingWithMyLife x)				= "exit 6":[]
	|otherwise 														= "exit 1":[]


	
replace :: Eq a => a -> a -> [a] -> [a]
replace a b = map $ \c -> if c == a then b else c

wtfAmIDoingWithMyLife :: String -> String
wtfAmIDoingWithMyLife [] = []
wtfAmIDoingWithMyLife x = last $ words $ replace ')' ' ' $ replace '(' ' ' $ replace ',' ' ' (x)


isValidTTtoNear :: String -> Bool
isValidTTtoNear [] = False
isValidTTtoNear ('(':a:',':b:',':xs)
	|isTask a , isTask b 											= True
	|otherwise 														= False
isValidTTtoNear (x)													= False 

isValidPenality :: String -> Bool
isValidPenality [] = False 
isValidPenality (x)
	|isNothing (readMaybe (x) :: Maybe Int ) 						= False
	|fromJust (readMaybe (x) :: Maybe Int) >= 0					= True
	|otherwise 														= False

--returnString :: String -> 
-------------------------------------------------------------------------------------------------------------------------------
naturalNumber' :: Int -> Bool
naturalNumber' x
	|x >= 0 = True
-------------------------------------------------------------------------------------------------------------------------------	

errorOutPut :: String -> Int -> IO()
errorOutPut y x
	|x == 1 														=  writeFile y "Error while parsing input file"
	|x == 2 														=  writeFile y "partial assignment error"
	|x == 3 														=  writeFile y "invalid machine/task"
	|x == 4  														=  writeFile y "machine penalty error"
	|x == 5  														=  writeFile y "invalid task"
	|x == 6  														=  writeFile y "invalid penalty"
	|x == 7  														=  writeFile y "No valid solution possible!"	
-------------------------------------------------------------------------------------------------------------------------------	
isValidMT :: String -> Bool
isValidMT [] = False
isValidMT ('(':a:',':b:')':[])
	|isMachine a , isTask b 										= True
	|otherwise 														= False
isValidMT (x)														= False
-------------------------------------------------------------------------------------------------------------------------------
isValidTT :: String -> Bool
isValidTT [] = False
isValidTT ('(':a:',':b:')':[])
	|isTask a , isTask b 											= True
	|otherwise 														= False
isValidTT (x)														= False 
-------------------------------------------------------------------------------------------------------------------------------	
-- isValidTTP :: String -> Bool 
-- isValidTTP [] = False
-- isValidTTP (xs:x:')':[])
	-- |isTask a , isTask b , if isJust (readMaybe (wtfAmIDoingWithMyLife c) :: Maybe Int) && fromJust (readMaybe (wtfAmIDoingWithMyLife c) :: Maybe Int) >= 0  then True else False		= True
	-- |otherwise 																																		= False
-- isValidTTP (x) 																																		= False
-------------------------------------------------------------------------------------------------------------------------------
isMachine :: Char -> Bool
isMachine x
	|x == '1'														= True
	|x == '2' 														= True
	|x == '3'														= True
	|x == '4'														= True
	|x == '5'														= True
	|x == '6'														= True
	|x == '7'														= True
	|x == '8'														= True
	|otherwise														= False
-------------------------------------------------------------------------------------------------------------------------------	

isTask :: Char -> Bool
isTask x
	|x == 'A'														= True
	|x == 'B' 														= True
	|x == 'C'														= True
	|x == 'D'														= True
	|x == 'E'														= True
	|x == 'F'														= True
	|x == 'G'														= True
	|x == 'H'														= True
	|otherwise														= False
-------------------------------------------------------------------------------------------------------------------------------	

{-REMOVE ALL WHITESPACE AT THE END OF EVERY LINE OF INPUTLIST-}
noWP :: [String] -> [String]
noWP [] 															= [] 
noWP xs 															= map (dropWhileEnd isSpace) xs
------------------------------------------------------------------------------------------------
{-CHECK FOR LEADING WHITESPACE OF INPUT-}
leadingWhiteSpace :: String -> Bool
leadingWhiteSpace x 
	|(take 1 x == ' ':[])  										= True
	|otherwise 														= False	
-----------------------------------------------------------------------------------------------
{-CONVERT MAYBE INTS OF THE HEADERS INDICES TO INTS-}
convertToInt :: Maybe int -> int
convertToInt xs 													= fromJust(xs)
-----------------------------------------------------------------------------------------------
{-RETURN THE HEADERS INDICES IN THE FORM OF A OF MAYBE INT-}
indexHeader ::(Ord a, Num a, Eq a) => [String] -> a -> Maybe Int
indexHeader xs a 													= (elemIndex(getHeader a)xs)
-----------------------------------------------------------------------------------------------
{-RETURN THE APPROPRIATE HEADER, VERY BASIC HELPER-}
getHeader :: (Num x, Eq x) => x -> String
getHeader a
	| a == 0 														= "Name:"
	| a == 1 														= "forced partial assignment:"
	| a == 2 														= "forbidden machine:"
	| a == 3 														= "too-near tasks:"
	| a == 4 														= "machine penalties:"
	| a == 5														= "too-near penalities"
	
-----------------------------------------------------------------------------------------------
{-KEEP ONLY NONE NULL ELEMENTS IN THE LIST-}
removeNull :: [String] -> [String]
removeNull xs 														= [ x | x <- xs , not (null x) ]	
-----------------------------------------------------------------------------------------------
{-SPLIT THE LIST UP BY HEADER AND RETURN THEIR CONTENTS-}
splitContents :: [String] -> Int -> Int -> [String]
splitContents [] _ _ 												= []
splitContents xs n m 
	|n == 5 && m == 5												= drop ((convertToInt (elemIndex(getHeader 5)xs)) + 1)  xs
	|otherwise 														= let temp = take (n) xs  
																	  in let final = drop (m + 1) temp
																	  in final
-----------------------------------------------------------------------------------------------
{-CHECKS TO SEE IF A PAIR IS IN THE RIGHT FORMAT-}
isPair :: String -> Bool
isPair x
	|head x == '(' && last x == ')' && count x == 1 				= True
	|otherwise 															= False	
------------------------------------------------------------------------------------------------
{-COUNTS THE NUMBER OF FCOMMAS IN A STRING-}
count :: String -> Int
count "" = 0
count (x:xs)
	|x == ',' 														= 1 + count xs
	|otherwise 														= 0 + count xs	


-- thingy :: String -> Bool
-- thingy 	('(':' ':',':' ':')':[])									= True
-- thingy	('(':' ':_:',':_:')':[])									= True
-- thingy	('(':_:',':' ':_:')':[]) 									= True
-- thingy x 															= False

	
	
--TODO
-- inside of pairs there can be no spaces
-- Check that there are no empty pairs 
-- check that there are no empty triples 
 
	
	
	
	
	
	
	
	
	
	

	-- let	noEndSpace = noWP ( lines contents ) 																										-- remove the white space from the end of all the lines 
		-- allExist = checkHeader noEndSpace 0																										-- return list of boolean showing if all the headers exist all the headers exist 									
	-- print noEndSpace
	-- if (truthCheck allExist) then putStrLn "good to go all headers exist" else putStrLn "Error while parsing input file"							-- check list to assert that all elements are true 
																																					-- -- TODO should continue the program if true
																																					-- -- TODO output "Error while parsing input file" 
																												
	-- let	theEndIsNye = convertToInt $ (indexHeader noEndSpace 0)																					-- obtain a list containing the list of each header
		-- spacingTrue = (checkBlankLine noEndSpace theEndIsNye 1)																					-- use list obtained of booleans True if Blank line above head,False if no Blank line
	-- print spacingTrue
	-- -- TODO CHECK TO SEE THAT NAME IS THE FIRST ELEMENT OF 
	-- --THE LIST IF NOT OUTPUT "Error while parssing input file"
	
	-- if (truthCheck spacingTrue) then putStrLn "good to go 1 white space between each header" else putStrLn "Error while parsing input file"		-- check list to assert all elements are true
																																					-- -- TODO continue if all true
																																					-- -- "Error while parsing input file" 
	-- let	contentsList' =  removeNull $ noEndSpace																									-- since all the white space is checked remove all the null lines from contents
		-- tempOne = convertToInt $ (indexHeader contentsList' 0)																					-- re-index the header because of the change
	
	
	-- if leadingWhiteSpace contentsList' then putStrLn "good to go leadingWhiteSpace" else putStrLn "Error while parsing input file" 				--check for whiteSpace that occurs at the beginning of a line
	-- --split the contents of the main list into their respective sections in order to test their contents
	-- let	nameList 		= splitContents contentsList' tempOne  1 0 																				-- name, the length of of this list should only be 1, and the name should only consist of alphabet characters					
		-- 																				-- forced partial assignment, length of this list depends on if there are exact clones of eachother, ideally there should be a maximum of 8
		-- fmString		= splitContents contentsList' tempOne  3 2																				-- forbidden machine
		-- tntString		= splitContents contentsList' tempOne  4 3																				-- too near task
		-- mpString		= splitContents contentsList' tempOne  5 4																				-- machine penalties, 8x8
		-- tnpString		= splitContents contentsList' tempOne  6 5																				-- too near penalities
	-- --	poop'''' 		= isMatrix mpString
		
	-- -- this checks to see if forced partial assignment, forced machine, and too near tasks are in the proper format of "( , )"
	
	-- truthCheck (isPair $ tntString) then putStrLn "good to go pair (_,_)" else putStrLn "Error while parsing input file" 
	

	-- -- checks to see if too near penalities is in the form ( , , ) 
	-- if truthCheck (isTriple tnpString) then putStrLn "good to go (_,_,_)" else putStrLn "Error while parsing input File" 
	
	-- if ((length mpString) == 8 && (truthCheck (isMatrix mpString))) then putStrLn "good to go matrix" else putStrLn "machine penalty error"

	-- --print poop''''
	
	-- -- checks to see if all the elements of the matrix are natural numbers
	-- -- TODO make sure there is only one space between each element
	-- if truthCheck (isVMP $ map words mpString) then putStrLn "good to go penalty" else putStrLn	"invalid penalty"
	
	-- -- convert matrix to [[int]]
	-- let	poop 
	
		
	-- -- check the converted list to see if they are int's >= 0
	-- if  truthCheck (naturalNumberMatrix poop)  then putStrLn "good to go penalty >= 0" else putStrLn "invalid penalty"
	
	
	
	
	
	-- -- CHANGE THE NAME OF PENIS
	-- --let penis = isVMP $ map words mpString >>>>>> I DONT REMEMBER WHAT THIS DOES



	
	-- --print $ map words mpString
		
	-- -- print nameList
	-- -- print fpaString
	-- -- print fmString
	-- -- print tntString
	-- --print mpString
	-- -- print tnpString
	
	

	


-- ---------------------------------------------------------------------------------------------
-- {-DEBUGGING PRINT THE INPUT LIST-}
-- printElements :: [String] -> IO()
-- printElements = mapM_ putStrLn


-- ---------------------------------------------------------------------------------------------
-- {- CHECK TO SEE IF THE HEADERS EXIST IN THE INPUT LIST TAKING FROM INPUT FILE-}
-- checkHeader :: [String] -> Int -> [Bool]
-- xs `checkHeader` a  
	-- | a < 5 				= (getHeader a `elem` xs):(checkHeader xs (a + 1))
	-- | a == 5 				= (getHeader a `elem` xs):[]

-- ---------------------------------------------------------------------------------------------
-- {-CHECK TO SEE THAT THERE IS AT LEAST ONE LINE BETWEEN EACH HEADER FALSE IF NO TRUE IF YES-}
-- checkBlankLine :: [String] -> [Int] -> Int -> [Bool]
-- checkBlankLine [] _ _		= False:[]
-- checkBlankLine _ [] _ 		= False:[]
-- checkBlankLine xs ys n 	
	-- |n < 5 				= (null (xs !! (getIndexNull ys n))):(checkBlankLine xs ys (n + 1))
	-- |n == 5  				= (null (xs !! (getIndexNull ys n))):[]
-- ---------------------------------------------------------------------------------------------
-- {-GET THE INDEX OF THE LINE ABOVE EACH HEADER-}
-- getIndexNull :: [Int] -> Int -> Int
-- getIndexNull [] _ = 1
-- getIndexNull xs n = (xs !! n) - 1
-- ---------------------------------------------------------------------------------------------
-- {-CHECK LIST OF BOOL TO SEE IF THERE EXISTS A FALSE, RETURN FALSE IF IT DOES, TRUE IF THE ENTIRE LIST IST TRUE-}
-- truthCheck :: [Bool] -> Bool
-- truthCheck []           	= True
-- truthCheck (x:xs)  
    -- | not x         	= False
    -- | otherwise        	= truthCheck xs


-- ---------------------------------------------------------------------------------------------
{-HELPER OF SPLITCONTENTS-}
-- getIndexn :: Int -> Int
-- getIndexn x = (x !
---------------------------------------------------------------------------------------------
-- {-HELPER OF SPLITCONTENTS-}
-- getIndexm :: [Int] -> Int -> Int
-- getIndexm [] _ = 1
-- getIndexm xs n = (xs !! n) + 1
---------------------------------------------------------------------------------------------



-- ----------------------------------------------------------------------------------------------	
{-CHECKS TO SEE IF THE MATRIX HAS THE APPROPRIATE NUMBER OF ELEMENTS ON EACH LINE-}
-- isMatrix :: String -> Bool
-- isMatrix [] = []
-- isMatrix () 
	-- |
	-- |((length (words x)) == 8) 	= True:isMatrix xs
	-- |otherwise  					= False:[]

-- ----------------------------------------------------------------------------------------------

-- ----------------------------------------------------------------------------------------------

-- ----------------------------------------------------------------------------------------------
-- {-ISVALIDMACHINEPENALITY, CHECKS TO SEE IF THE CONTENTS OF THE MATRIX ARE INTS-}
-- isVMP :: [[String]] -> [Bool]
-- isVMP [] = []
-- isVMP (x:xs)
	-- |truthCheck(isVMP' x)			 = True:isVMP xs
	-- |otherwise 				 		 =	False:[] 


-- {-CHECKS TO SEE IF MATRIX ONLY CONTAINS NATURAL NUMBERS INCLUDING 0. CHECKS WHOLE LIST-}
-- naturalNumberMatrix :: [[Int]] -> [Bool]
-- naturalNumberMatrix [] = []
-- naturalNumberMatrix (x:xs)
	-- |all (>= 0) x 	=  True:naturalNumberMatrix xs
	-- |otherwise 		= 	False:[]
-- {-THIS IS TO CHECK TO SEE IF THE TRIPLE HAS A NATURAL NUMBER-}

-- ----------------------------------------------------------------------------------------------


-- {-STYLISTIC FUNCTION TO CLEAN MAIN CODE UP-}
-- pairCombiner :: [String] -> [String] -> [String] -> [String]
-- pairCombiner xs ys zs = xs++ys++zs 
 




-- -- TODO 
-- -- check the contents of the constraints that they are within the range of {A-H} and {1-8}
-- -- check for duplicates
-- -- check the matrix to see that there is only one space between each number 
-- -- check the that the triple machine penalty is a natural number 
-- -- strip the brackets and comma's from the constraints
-- -- check to see if the names are in the correct order
-- -- check if the forced partial assignment has 2 pairs with the same machine or the same penalty
-- -- check to see if fpa tnt fm have machines and tasks in {a-h} and {1-8}
-- -- check to see if in the triples that the tasks are in A-H 
-- -- check to see if triple is natural number 
-- -- make sure that if the letters are in the same order in the triple it updates the penalty only













-- -- verifies a string is a valid (mach,task) pair
-- -- isVANP :: String -> Bool
-- -- isVANP []	= False
-- -- isVANP ('(':x:',':y:')':[])	| validMachine x, validTask y	= True | otherwise	= False
-- -- isVANP (x)	= False


