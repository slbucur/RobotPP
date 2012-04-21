module Robot
where

import Matrix
import Types
import Debug.Trace
import Data.List
import Data.Maybe
import Data.Map

a = bordedMatrix 2 3
b = setMatrixElement (1,1) (zeroBordedMatrix 10 10) (-1)
--scade o lista mica dintr-o lista mare
-- listSubstract [2,3,4] [1,2,4,5] = [1,5]
--listSubstract :: [Int] -> [Int] -> [Int]
listSubstract smallList bigList = Data.List.filter (\ x -> notElem x smallList) bigList

--da un alt Cardinal in cazul in care cel ales este o coliziune
--avoidCollision :: Cardinal -> [Cardinal] -> [Cardinal]
avoidCollision asmCard cs = if (notElem asmCard cs )
	then 	asmCard
	else 	(listSubstract cs [S,E,N,W]) !! 0

-- pune gropile descoperite pe harta
putHoles :: (Int, Int) -> Matrix -> [Cardinal] -> Matrix
putHoles pos matr cs = if ( (length cs) == 0)
	then matr
	else putHoles pos 
		(setMatrixElement (newPosition pos (head cs)) matr minInt) 
		(tail cs)
	
--da pozitia la care se va afla robotul dupa ce se va duce in directia Cardinal
newPosition:: (Int,Int)-> Cardinal->(Int,Int)
newPosition (x, y) card
	| card == N = (x - 1, y)
	| card == S = (x + 1, y)
	| card == E = (x, y + 1)
	| card == W = (x, y - 1)
	| otherwise = (x, y)
	
--da pozitia la care se va afla robotul dupa ce se va duce in directia Cardinal
newJustPosition:: (Int,Int)->Maybe Cardinal->(Int,Int)
newJustPosition (x, y) card
	| card == Just N = (x - 1, y)
	| card == Just S = (x + 1, y)
	| card == Just E = (x, y + 1)
	| card == Just W = (x, y - 1)
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
--lista vecinilor accesibili		
makeAccNeighPos:: (Int,Int) -> Matrix -> [(Int,Int)]
makeAccNeighPos (x,y) matr = 
		Data.List.filter (\ pos -> (getMatrixElement pos matr) > minInt) neighPos
	where
		north = (newPosition (x,y) N)
		south = (newPosition (x,y) S)
		east =  (newPosition (x,y) E)
		west =  (newPosition (x,y) W)
		neighPos = north:south:east:west:[]
		
--ia un index dintr-o lista makeNeighList si il face un cardinal
toCardinal:: Maybe Int->Maybe Cardinal
toCardinal ind
	| ind == Just 0 = Just N
	| ind == Just 1 = Just S
	| ind == Just 2 = Just E
	| ind == Just 3 = Just W
	| otherwise = Nothing

--returneaza cea mai buna alegere a pozitiei la care se va duce robotul
takeBestCard2:: [Int]->Int->Maybe Cardinal	
takeBestCard2 neighList s = {-trace (show (neighList,s))-}
	(if( maxNeigh > s) --avem o valoare mai mare decat cea curenta
	then toCardinal(maxNeighPoz) 
	else 
		if( haveUnexploredPoz)
		then toCardinal(firstUnexploredPoz)
			else 
				if(maxNeigh > 0)
					then toCardinal(maxNeighPoz) 
					else Just S)
	where
		maxNeigh = maximum(neighList)
		maxNeighPoz = elemIndex (maxNeigh) neighList
		firstUnexploredPoz = elemIndex (-1) neighList
		haveUnexploredPoz = (not(isNothing(firstUnexploredPoz)))


takeBestCard:: (Int, Int) -> Matrix -> Int -> (Matrix,Maybe Cardinal)
takeBestCard (x,y) matr s = trace (show ((x,y),matr))
	(if(maxNeigh > s)
	then (matr,toCardinal(maxNeighPoz))
	else 
		if (haveUnexploredPoz)
		then (matr, toCardinal(firstUnexploredPoz))
		else 
			if (maxNeigh > 0)
			then trace "I'm here" (setCurrentZero, toCardinal(maxNeighPoz))
			else 
				((simpleMakePath (x,y) matr), Nothing))
	where
		neighList = (makeNeighList (x,y) matr)
		maxNeigh = maximum(neighList)
		maxNeighPoz = elemIndex (maxNeigh) neighList
		firstUnexploredPoz = elemIndex (-1) neighList
		haveUnexploredPoz = (not(isNothing(firstUnexploredPoz)))
		setCurrentZero = setMatrixElement (x,y) matr 0
		
--Adauga vecinii accesibili in tabelul de parinti
insertChildren:: [(Int,Int)] -> (Int,Int) -> Map (Int, Int) (Int,Int)
				-> Map (Int, Int) (Int,Int) 
insertChildren children parent parentMap =
	if( length(children) == 0)
	then parentMap
	else insertChildren (tail(children)) parent 
		(Data.Map.insert (head(children)) parent parentMap)
		
--Functie care cauta cea mai apropiata pozitie neexplorata 
--si returneaza tabelul parintilor si pozitia gasita
bfs :: [(Int,Int)] -> [(Int,Int)] -> Map (Int, Int) (Int,Int) -> Matrix 
	-> ((Int,Int),  Map (Int, Int) (Int,Int))
bfs posList visited parentMap matr = {-trace (show (currentUnvisitedNegih,visited))-}
	(if ((length posList) == 0)
		then (currentPos, parentMap)
		else 
			if  (getMatrixElement currentPos matr) == (-1)
			then (currentPos, parentMap)
			else {-((0,0), parentMap)-}
			 (bfs ((tail posList) ++ currentUnvisitedNegih)
				  (nub (currentPos : visited))
				  newParentMap matr)
	)		 
	where 
		currentPos = head(posList)
		currentNeigh = (makeAccNeighPos currentPos matr)
		currentUnvisitedNegih = listSubstract visited currentNeigh
		newParentMap = insertChildren currentUnvisitedNegih currentPos parentMap

--functie simplificata de bfs
simpleBfs :: (Int,Int) -> Matrix
		-> ((Int,Int),  Map (Int, Int) (Int,Int))
simpleBfs pos matr = 
			bfs [pos] [] Data.Map.empty matr

--functie care face o cale de pozitii neexplorate pana la
-- pozitia gasita in bfs
makePath :: (Int,Int) -> Matrix -> Map (Int, Int) (Int,Int) -> Matrix
makePath pos matr parentMap = if (currentParent == Nothing)
	then matr
	else (makePath (fromJust currentParent) newMatr parentMap)
	
	where
	currentParent = Data.Map.lookup pos parentMap
	newMatr = (setMatrixElement pos matr (-1))
	
-- functie simplificata de makePath
simpleMakePath :: (Int,Int) -> Matrix -> Matrix	
simpleMakePath pos matr = makePath (fst(bfsRes)) matr (snd(bfsRes))
	where
		bfsRes = simpleBfs pos matr
	
emptyMap :: Int -> Map (Int, Int) (Int,Int)
emptyMap a = Data.Map.empty		
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
perceiveAndAct s cs m = trace (show (updatedMatr,(x,y)))
	(	nextCard, 
	{-(Just (avoidCollision E cs)),-}
	(updatedMatr,updatedPos))
	where
		matr = fst(m)
		x = fst(snd(m))
		y = snd(snd(m))
		--matricea rezultata in urma analizei pozitiei curente
		nextMatr = (fst (takeBestCard (x,y) matr s))
		--pozitia la care se va afla robotul tura urmatoare
		nextCard = (snd (takeBestCard (x,y) updatedMatr s))
		bestCardResult = (takeBestCard (x,y) updatedMatr s)
		updatedMatr = putHoles (x,y) (setMatrixElement (x,y) nextMatr s) cs
		updatedPos = (newJustPosition (x,y) nextCard)
		
