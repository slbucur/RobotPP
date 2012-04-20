module Robot
where

import Matrix
import Types
import Debug.Trace
import Data.List

--scade o lista mica dintr-o lista mare
-- listSubstract [2,3,4] [1,2,4,5] = [1,5]
--listSubstract :: [Int] -> [Int] -> [Int]
listSubstract smallList bigList = filter (\ x -> notElem x smallList) bigList

--da un alt Cardinal in cazul in care cel ales este o coliziune
--avoidCollision :: Cardinal -> [Cardinal] -> [Cardinal]
avoidCollision asmCard cs = if (notElem asmCard cs )
	then 	asmCard
	else 	(listSubstract cs [S,E,N,W]) !! 0
--da pozitia la care se va afla robotul dupa ce se va duce in directia Cardinal
newPosition:: (Int,Int)->Cardinal->(Int,Int)
newPosition (x, y) card
	| card == N = (x - 1, y)
	| card == S = (x + 1, y)
	| card == E = (x, y + 1)
	| card == W = (x, y - 1)
	| otherwise = (x, y)

--face o lista cu valorile scanner-ului/ implicite pentru vecini
makeNeighList:: (Int,Int) -> Matrix -> [Int]
makeNeighList (x,y) matr =
		north:south:east:west:[]
	where 
		north = getMatrixElement (newPosition (x,y) N) matr
		south = getMatrixElement (newPosition (x,y) S) matr
		east = getMatrixElement (newPosition (x,y) E) matr
		west = getMatrixElement (newPosition (x,y) W) matr

--ia un index dintr-o lista makeNeighList si il face un cardinal
toCardinal:: Int->Cardinal
toCardinal ind
	| ind == 0 = N
	| ind == 1 = S
	| ind == 2 = E
	| ind == 3 = W
	
takeBestCard neighList s =
	if( maximum(neighList) > s)
		then elemIndex (maximum(neighList), neighList)
		else 
			if(minimum(neighList) /= maximum(neighList))
			then elemIndex((-1),neighList)
			else 0
			
{-
When the robot enters the mine it receives as input the size of the mine (it
is always placed at (0, 0)). This function should return the initial memory
element of the robot.
-}
--startRobot :: Size -> a
startRobot size = trace (show size) ( (bordedMatrix (fst(size)) (snd(size))), (1,1))-- TODO

{-
At each time step the robot sends a light beam in all 4 cardinal directions,
receives the reflected rays and computes their intensity (the first argument
of the function).

The robot sees nearby pits. The second argument of this function is the list
of neighbouring pits near the robot (if empty, there are no pits).

Taking into account the memory of the robot (third argument of the function),
it must return a tuple containing a new cardinal direction to go to and a new
memory element.

If the cardinal direction chosen goes to a pit or an wall the robot is
destroyed. If the new cell contains minerals they are immediately collected.
-}
-- perceiveAndAct :: SVal -> [Cardinal] -> a -> (Action, a)
perceiveAndAct s cs m = trace (show (makeNeighList (x,y) matr){-(setMatrixElement (x,y) matr s)(newPosition (x,y) (avoidCollision E cs))-}) 
	( (toCardinal(takeBestCard((makeNeighList((x,y),matr)),s))){--(Just (avoidCollision E cs))--},
	(updatedMatr,(newPosition (x,y) (avoidCollision E cs))))
	where
		matr = fst(m)
		x = fst(snd(m))
		y = snd(snd(m))
		updatedMatr = setMatrixElement (x,y) matr s
