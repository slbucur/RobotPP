module Robot
where

import Matrix
import Types
import Debug.Trace

third (_, _, x) = x

--scade o lista mica dintr-o lista mare
-- listSubstract [2,3,4] [1,2,4,5] = [1,5]
--listSubstract :: [Int] -> [Int] -> [Int]
listSubstract smallList bigList = filter (\ x -> notElem x smallList) bigList

--da un alt Cardinal in cazul in care cel ales este o coliziune
--avoidColision :: Cardinal -> [Cardinal] -> [Cardinal]
avoidCollision asmCard cs = if (notElem asmCard cs )
	then 	asmCard
	else 	(listSubstract cs [S,E,N,W]) !! 0
--da pozitia la care se va afla robotul dupa ce se va duce in directia Cardinal
--newPosition:: (Int,Int)->Cardinal->(Int,Int)
newPosition (x, y) card
	| card == N = (x - 1, y)
	| card == S = (x + 1, y)
	| card == E = (x, y + 1)
	| card == W = (x, y - 1)
	| otherwise = (x, y)
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
perceiveAndAct s cs m = trace (show (newPosition (x,y) (avoidCollision E cs))) 
	((Just (avoidCollision E cs)), (matr,(newPosition (x,y) (avoidCollision E cs))))
	where
		matr = fst(m)
		x = fst(snd(m))
		y = snd(snd(m))
