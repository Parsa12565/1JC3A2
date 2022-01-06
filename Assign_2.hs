{- Assignment 2
 - Name: Parsa Zanganeh
 - Date: 2020-10-22
 -}
module Assign_2 where

macid :: String
macid = "zanganep"

type Euclid2D = (Double,Double)

{- -----------------------------------------------------------------
 - getX,getY
 - -----------------------------------------------------------------
 - Description: returns the x/y coordinate, respectivley
 -}
getX :: Euclid2D -> Double
getX e = fst e

getY :: Euclid2D -> Double
getY e = snd e

{- -----------------------------------------------------------------
 - scalarMult
 - -----------------------------------------------------------------
 - Description: returns the scalar multiplication of its input
 -}
scalarMult :: Double -> Euclid2D -> Euclid2D
scalarMult s e = (s*(getX e),s*(getY e))

{- -----------------------------------------------------------------
 - add
 - -----------------------------------------------------------------
 - Description: performs 2D vector addition
 -}
add :: Euclid2D -> Euclid2D -> Euclid2D
add e1 e2 = ((getX e1)+(getX e2),(getY e1)+(getY e2))

{- -----------------------------------------------------------------
 - innerProduct
 - -----------------------------------------------------------------
 - Description: implements the inner product operation for 2D Euclidean space
 -}
innerProduct :: Euclid2D -> Euclid2D -> Double
innerProduct e1 e2 = (getX e1)*(getX e2)+(getY e1)*(getY e2)

{- -----------------------------------------------------------------
 - distance
 - -----------------------------------------------------------------
 - Description: implements the Euclidean distance between two elements
 -}
distance :: Euclid2D -> Euclid2D -> Double
distance e1 e2 = distanceAux (add (e2) (scalarMult (-1) e1))

distanceAux :: Euclid2D -> Double
distanceAux e = ((getX e)**2 + (getY e)**2)**0.5

{- -----------------------------------------------------------------
 - maxDistance
 - -----------------------------------------------------------------
 - Description: returns the element with the largest distance from (0,0)
 -}
maxDistance :: [Euclid2D] -> Euclid2D
maxDistance es = if(es == [])
                    then (0,0)
                    else if(distance (0,0) (head es)>=distance (0,0) (maxDistance (tail es)))
                            then head es
                            else maxDistance (tail es)

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
Function: getX
Test Case Number: 1
Input: (1,2)
Excpected Output: 1
Actual Output: 1.0

Function: getX
Test Case Number: 2
Input: (3,4)
Excpected Output: 3
Actual Output: 3.0

Function: getX
Test Case Number: 3
Input: (5,6)
Excpected Output: 5
Actual Output: 5.0

Function: getY
Test Case Number: 1
Input: (1,2)
Excpected Output: 2
Actual Output: 2.0

Function: getY
Test Case Number: 2
Input: (3,4)
Excpected Output: 4
Actual Output: 4.0

Function: getY
Test Case Number: 3
Input: (5,6)
Excpected Output: 6
Actual Output: 6.0

Function: scalarMult
Test Case Number: 1
Input: 2 (3,4)
Excpected Output: (6,8)
Actual Output: (6.0,8.0)

Function: scalarMult
Test Case Number: 2
Input: 5 (6,7)
Excpected Output: (30,35)
Actual Output: (30.0,35.0)

Function: scalarMult
Test Case Number: 3
Input: 8 (9,10)
Excpected Output: (72,80)
Actual Output: (72.0,80.0)

Function: add
Test Case Number: 1
Input: (1,2) (3,4)
Excpected Output: (4,6)
Actual Output: (4.0,6.0)

Function: add
Test Case Number: 2
Input: (5,6) (7,8)
Excpected Output: (12,14)
Actual Output: (12.0,14.0)

Function: add
Test Case Number: 3
Input: (9,10) (11,12)
Excpected Output: (20,22)
Actual Output: (20.0,22.0)

Function: innerProduct
Test Case Number: 1
Input: (1,2) (3,4)
Excpected Output: 11
Actual Output: 11.0

Function: innerProduct
Test Case Number: 2
Input: (5,6) (7,8)
Excpected Output: 83
Actual Output: 83.0

Function: innerProduct
Test Case Number: 3
Input: (9,10) (11,12)
Excpected Output: 219
Actual Output: 219.0

Function: distance
Test Case Number: 1
Input: (1,2) (3,4)
Excpected Output: 8**0.5
Actual Output: 2.8284271247461903

Function: distance
Test Case Number: 2
Input: (0,0) (3,4)
Excpected Output: 5
Actual Output: 5.0

Function: distance
Test Case Number: 3
Input: (-1,-1) (2,3)
Excpected Output: 5
Actual Output: 5.0

Function: maxDistance
Test Case Number: 1
Input: []
Excpected Output: (0,0)
Actual Output: (0.0,0.0)

Function: maxDistance
Test Case Number: 2
Input: [(1,2),(2,1)]
Excpected Output: (1,2)
Actual Output: (1.0,2.0)

Function: maxDistance
Test Case Number: 3
Input: [(1,2),(3,4),(5,6)]
Excpected Output: (5,6)
Actual Output: (5.0,6.0)
-}