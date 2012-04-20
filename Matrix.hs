module Matrix
where
{-
Formarea matricei de inceput si functiile ajutatoare
accesarii si updatarii elementelor acesteia
-}
--Din pacate trebuie sa ma bazez pe tiparea automata, 
--pentru ca Haskell face diferenta intre Int si Integer
--dandu-mi erori de compilare
import Data.Sequence

type Matrix = ((Int, Int), Data.Sequence.Seq Int)
--Secventa de n zerouri
--zeros :: Int ->Seq Int
zeros a = Data.Sequence.replicate a 0

--Secventa de n minus unu -uri ( ce nume :) )
minusones :: Int -> Data.Sequence.Seq Int
minusones a = Data.Sequence.replicate a (-1)


-- Elementul -00
minInt = -1000000

--Matrice bordata in lateral cu -00 (valoare foarte mica)
semiBordedMatrixSeq :: Int -> Int -> Data.Sequence.Seq Int
semiBordedMatrixSeq m n = 
	if ( m == 1)
	then (minInt <| minusones(n) )|> minInt
	else ((minInt <| minusones(n) )|> minInt) >< semiBordedMatrixSeq (m-1) n

--Matrice bordata cu -00
bordedMatrix :: Int -> Int -> Matrix
bordedMatrix m n = ((m+2,n+2),((Data.Sequence.replicate (n+2) minInt) ><
		   (semiBordedMatrixSeq m n) ><
		   (Data.Sequence.replicate (n+2) minInt)))
		   
--returneaza numarul de coloane/linii dintr-o matrice
--getCol/Lin ((Int,Int), Matrix) -> Int
getCol matrix = snd(fst(matrix))
getLin matrix = fst(fst(matrix))

--returneaza elementul unei matrice cu n coloane aflat la pozitia i j
getMatrixElement :: Int -> Int -> Matrix -> Int
getMatrixElement i j matrix = Data.Sequence.index (snd(matrix)) ((n*i)+j)
	where 
		m = getLin matrix
		n = getCol matrix
		
 
--returneaza o matrice cu elementul la pozitia i j inlocuit cu a
setMatrixElement ::  Int -> Int ->((Int, Int), Data.Sequence.Seq Int)-> Int -> ((Int, Int), Data.Sequence.Seq Int)
setMatrixElement i j matrix a = ((m,n), Data.Sequence.update ((n*i)+j) a (snd(matrix)))
	where
		m = getLin matrix
		n = getCol matrix
			

