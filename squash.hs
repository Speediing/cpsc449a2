
getSecond :: (a,b,c,d) -> d
getSecond (_,_,_,d)= d

squash :: [(Int,[Char],[Char],Int)] -> [(Int,[Char],[Char],Int)]
squash [] = []
squash (x:xs:xss)
	|getSecond x > getSecond xs = xs:squash xss
	|getSecond x <= getSecond xs = x:squash xss
squash x
	|getSecond (x !! 0) > getSecond(x !! 1) = [x !! 1]
	|getSecond (x !! 0) <= getSecond (x !! 1) = [x !! 0]
	|otherwise 										= x
main = print("bleh")

getLowest :: [(Int,[Char],[Char],Int)] -> (Int,[Char],[Char],Int)
getLowest x
	|getSecond (x !! 0) > getSecond(x !! 1) = x !! 1
	|getSecond (x !! 0) <= getSecond (x !! 1) = x !! 0
