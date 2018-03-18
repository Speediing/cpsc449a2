
--fuck haskell
--I used recursion to simulate for loops 
import Data.List
test = [[3,2,1],[2,1,1],[1,1,1]]
array = [1,2,3]
--Function that returns the general penalty from the double penatly matrix
--Takes the generated list and and top index(should always be set to 7--just makes recusion easier)
--Assumes input list is ex, [1,2,3] where 1,2,3 are the machines and mpList is global
-- is equal to lines 42-48 in the java softconstraint code
genralPen list topIndex = 
  return (((mpList !! (topIndex))!!(list !!topIndex)) + (genralPen list topIndex - 1)

--Function that returns the too near penalty from the double penatly matrix
--Takes the generated list and and top index(should always be set to size minus 1 of too near array--just makes recusion easier)
--list is list we are checking
--is equal to 49-70 of java soft constarint code
tooNearPen list topIndex = 
  return (listcheck list topIndex 7) + (tooNearPen list topIndex- 1)


--Helper Function for inner loop in getting too near penalties
--is equal to 49-70 of java soft constarint code
listcheck list topIndex loopcount=
  if loopcount == 7
    --This is checking the case where the last is too close to the first
  then if (fst (tnpList !! topIndex) == (list !! 7) ) && (snd (tnpList !! topIndex) == (list !! 0) )
    return penalty  + listcheck list topIndex loopcount-1
    --This is checking every other case
  else if (fst (tnpList !! topIndex) == (list !! loopcount) ) && (snd (tnpList !! topIndex) == (list !! loopcount+1) )
    return penalty + listcheck list topIndex loopcount-1 

--This is the fuction to call for softconstarints
--should return the pen val as an int
--dont @ me if it breaks
softconstraint list = 
  return (genralPen list 7) + (tooNearPen list (size tnpList-1)))

main =  softconstraint array