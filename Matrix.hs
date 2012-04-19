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

--Secventa de n zerouri
--zeros :: Int ->Seq Int
zeros a = Data.Sequence.replicate a 0

--Secventa de n minus unu -uri ( ce nume :) )
--minusones :: Int -> Seq Int
minusones a = Data.Sequence.replicate a (-1)

--Matrice formata numai din zerouri (de fapt o secventa de n*m zerouri)
--zeroMatrix :: Int -> Int ->Seq Int
zeroMatrix m n = Data.Sequence.replicate (m*n) 0

-- Elementul -00
minInt = -1000000

--Matrice bordata in lateral cu -00 (valoare foarte mica)
--semiBordedMatrix :: Int -> Int -> Seq Int
semiBordedMatrix m n = 
	if ( m == 1)
	then (minInt <| minusones(n) )|> minInt
	else ((minInt <| minusones(n) )|> minInt) >< semiBordedMatrix (m-1) n

--Matrice bordata cu -00
--bordedMatrix :: Int -> Int -> Seq Int
bordedMatrix m n = (Data.Sequence.replicate (n+2) minInt) ><
		   (semiBordedMatrix m n) ><
		   (Data.Sequence.replicate (n+2) minInt)

--returneaza elementul unei matrice cu n coloane aflat la pozitia i j
--getMatrixElement :: Int -> Int ->Int -> Seq Int -> Int
getMatrixElement i j n matrix = Data.Sequence.index matrix ((n*i)+j)
 
--returneaza o matrice cu elementul la pozitia i j inlocuit cu a
--setMatrixElement ::  Int -> Int ->Int -> Seq Int -> Int -> Seq Int
setMatrixElement i j n matrix a = Data.Sequence.update ((n*i)+j) a matrix
			

