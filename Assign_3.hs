{- Assignment 3
 - Name: Usman Asad
 - Date: Nov-03-2019
 -}
module Assign_3 where

import qualified Data.Map.Strict as IM

macid :: String
macid = "asadu"

data Poly a = X
            | Coef a
            | Sum (Poly a) (Poly a)
            | Prod (Poly a) (Poly a)
  deriving Show


newtype PolyList a = PolyList [a]
  deriving Show


{- -----------------------------------------------------------------
 - polyValue
 - -----------------------------------------------------------------
 - Description: Given a Poly Nomial of data type Poly, this function
 - returns the value of the Poly Nomial at N. In mathematical notation,
 - this function returns P(n), where P is the given function and n is the
 - value of X that you want the value for. The function returns an integer
 - value.
 -}
polyValue :: Num a => Poly a -> a -> a
polyValue (X) n = n
polyValue (Coef a) n = a
polyValue (Sum poly1 poly2) n = (polyValue poly1 n) + (polyValue poly2 n)
polyValue (Prod poly1 poly2) n = (polyValue poly1 n) * (polyValue poly2 n)

{- -----------------------------------------------------------------
 - polyListValue
 - -----------------------------------------------------------------
 - Description: Given a Poly Nomial in list form where each value of
 - the list represents X's coefficient (going from a0*x^0 ... an*x^n)
 - the function returns the value of the Poly nomial at X = n. The 
 - arithmetic used to calculate the value is based off of horners method.
 - The function returns an integer value
 -}
polyListValue :: (Num a,Eq a) => PolyList a -> a -> a
polyListValue (PolyList []) n = 0
polyListValue (PolyList [p1]) n = p1
polyListValue (PolyList (index:list)) n = index + n * polyListValue (PolyList ((head list : tail list))) n


{- -----------------------------------------------------------------
 - polyListValue
 - -----------------------------------------------------------------
 - Description: Given two lists (p1 & pq) and a flag (n), this func-
 - -tion checks to see which lists value is smaller and then equalizes
 - them by adding extra 0's at the end of the list until they are of 
 - the same length and returns the new "equalized list". The purpose of 
 - the flag is basically to identify which list you are trying to equalize
 - 0 means list p1, and 1 means list q1. The return type is a list
 -}
listEqualizer :: (Num a, Eq a) => [a] -> [a] -> a -> [a] 
listEqualizer (p1) (q1) (n) = 
      let
        lenP = length p1
        lenQ = length q1
      in  
        if ((lenP) < (lenQ)) then listEqualizer ((concat[p1,[0]])) (q1) (n)
        else if ((lenQ) < (lenP)) then listEqualizer ((concat[q1,[0]])) (p1) (n)
        else if ((lenP) == (lenQ)) && (n == 0) then (p1)
        else if ((lenP) == (lenQ)) && (n == 1) then (q1)
        else error "You shouldn't get here lol"


{- -----------------------------------------------------------------
 - polyListSum
 - -----------------------------------------------------------------
 - Description: Given two PolyLists, this function returns the Sum of
 - the two PolyLists. Adding the two PolyLists is equivilent to  adding
 - two PolyNomials in standard form. The return type is another PolyList
 -}
polyListSum :: (Num a,Eq a) => PolyList a -> PolyList a -> PolyList a
polyListSum (PolyList []) (PolyList []) = PolyList []
polyListSum (PolyList []) (PolyList q1) = PolyList q1
polyListSum (PolyList p1) (PolyList []) = PolyList p1
polyListSum (PolyList ps) (PolyList qs) = let
                                             newP = listEqualizer (ps) (qs) 0
                                             newQ = listEqualizer (ps) (qs) 1
                                          in 
                                             if ((length ps) == (length qs)) then PolyList (zipWith (+) ps qs)
                                             else PolyList (zipWith (+) (newP) (newQ))
  

{- -----------------------------------------------------------------
 - polyListDegree
 - -----------------------------------------------------------------
 - Description: Given a Poly Nomial represented in form PolyList, this
 - function returns an integer that represents the degree of the PolyNomial
 -}
polyListDegree :: (Num a,Eq a) => PolyList a -> Integer
polyListDegree (PolyList []) = error "No Polynomial Found! Your PolyList may be empty."
polyListDegree (PolyList p1) = (toInteger(length p1) - 1)


{- -----------------------------------------------------------------
 - multElement
 - -----------------------------------------------------------------
 - Description: multElement, takes in two polyLists and multiplies the 
 - entire 2nd polyLIst by the first element of the first list. Returning
 - a new PolyLIst with the transformation applied. 
 -}
multElement :: (Num a, Eq a) => PolyList a -> PolyList a -> PolyList a 
multElement (PolyList (index:poly)) (PolyList (b)) = PolyList (map (index*) b)

{- -----------------------------------------------------------------
 - indexShifter
 - -----------------------------------------------------------------
 - Description: Index shifter, takes in a PolyList and "shifts it" by 
 - adding a 0 to the start. This is done because Once we've multiplied 
 - the first element of one of the lists, we need to shift one to represent
 - being multiplied by a higher power of x. This padding is done by using
 - cons to add a 0 to the start of the list, this essentially shifts our 
 - index by one.
 -}
indexShifter :: (Num a, Eq a) => PolyList a -> PolyList a
indexShifter (PolyList p1)= PolyList(0:p1)

{- -----------------------------------------------------------------
 - polyListProd
 - -----------------------------------------------------------------
 - Description: polyListProd takes in two Poly Nomails in Poly List
 - form and returns their Product, also as type PolyList. THe arit-
 - -hmetic is based off of 'Distributivity of * over +' where you 
 - first distribute the constant, then shift and distribute the next
 - constant while adding them all together.
 -}
polyListProd :: (Num a,Eq a) => PolyList a -> PolyList a -> PolyList a
polyListProd (PolyList p1) (PolyList []) = PolyList []
polyListProd (PolyList []) (PolyList p1) = PolyList [] 
polyListProd (PolyList (current:ps)) (PolyList qs) = 
                        let 
                            mulCurrentByQs = multElement (PolyList (current:ps)) (PolyList qs)
                            shiftNext = indexShifter (polyListProd (PolyList ps) (PolyList qs))
                        in  
                            polyListSum (mulCurrentByQs) (shiftNext)

{- -----------------------------------------------------------------
 - polyListToPoly
 - -----------------------------------------------------------------
 - Description: polyListToPoly takes in a Poly Nomial in PolyList form
 - and returns the same Poly Nomial represented in type Poly.
 -}
polyListToPoly :: Num a => PolyList a -> Poly a
polyListToPoly (PolyList []) = (Coef 0)
polyListToPoly (PolyList [a]) = (Coef a) 
polyListToPoly (PolyList (index:list)) = (Sum (Coef index) (Prod (X) (polyListToPoly (PolyList ((head list : tail list)))))) 

{- -----------------------------------------------------------------
 - polyToPolyList
 - -----------------------------------------------------------------
 - Description: The function polyToPolyList takes in a Poly nomial 
 - in the form of Poly, and converts it to a Poly nomial of type 
 - PolyList. The return value is a PolyList.
 -}
polyToPolyList :: (Num a,Eq a) => Poly a -> PolyList a
polyToPolyList (Coef a) = PolyList [a]
polyToPolyList (X) = PolyList [0,1]
polyToPolyList (Sum (poly1) (poly2)) = polyListSum (polyToPolyList (poly1)) (polyToPolyList (poly2)) 
polyToPolyList (Prod (poly1) (poly2)) = polyListProd (polyToPolyList (poly1)) (polyToPolyList (poly2))



{- -----------------------------------------------------------------
   Test Cases:
  -----------------------------------------------------------------
  Function: polyValue
  Test Case Number: 1
  Input: (X) 5
  Expected Output: 5
  Actual Output: 5

  Function: polyValue
  Test Case Number: 2
  Input: (Coef 5) 1
  Expected Output: 5
  Actual Output: 5

  Function: polyValue
  Test Case Number: 3
  Input: (Sum (X) (Coef 3)) 7
  Expected Output: 10
  Actual Output: 10

  Function: polyValue
  Test Case Number: 4
  Input: (Prod (X) (Coef 3)) 4
  Expected Output: 12
  Actual Output: 12

-----------------------------------------------------------------

  Function: polyListValue
  Test Case Number: 1
  Input: (PolyList []) 2
  Expected Output: 0
  Actual Output: 0

  Function: polyListValue
  Test Case Number: 2
  Input: (PolyList [1,1,1]) 2
  Expected Output: 7
  Actual Output: 7

  Function: polyListValue
  Test Case Number: 3
  Input: (PolyList [4]) 1
  Expected Output: 4
  Actual Output: 4

-----------------------------------------------------------------

  Function: polyListSum
  Test Case Number: 1
  Input: (PolyList []) (PolyList [])
  Expected Output: PolyList []
  Actual Output: PolyList [] 

  Function: polyListSum
  Test Case Number: 2
  Input: (PolyList [1,2,3,4]) (PolyList [])
  Expected Output: PolyList [1,2,3,4]
  Actual Output: PolyList [1,2,3,4] 

  Function: polyListSum
  Test Case Number: 3
  Input: (PolyList [1,1]) (PolyList [9,9,3,2,5])
  Expected Output: PolyList [10,1O,3,2,5]
  Actual Output: PolyList [10,1O,3,2,5] 

  Function: polyListSum
  Test Case Number: 4
  Input: (PolyList [-1,-1]) (PolyList [9,9,3,2,5])
  Expected Output: PolyList [8,8,3,2,5]
  Actual Output: PolyList [8,8,3,2,5] 

-----------------------------------------------------------------

  Function: polyListDegree
  Test Case Number: 1
  Input: (PolyList [1,1])
  Expected Output: 1
  Actual Output: 1 

  Function: polyListDegree
  Test Case Number: 2
  Input: (PolyList [])
  Expected Output: 'Exception: No Polynomial Found! Your PolyList may be empty.'
  Actual Output: 'Exception: No Polynomial Found! Your PolyList may be empty.'

  Function: polyListDegree
  Test Case Number: 3
  Input: (PolyList [3,2,19,51,7,1])
  Expected Output: 5
  Actual Output: 5

-----------------------------------------------------------------

  Function: polyListProd
  Test Case Number: 1
  Input: (PolyList [1,2,3]) (PolyList [])
  Expected Output: (PolyList [])
  Actual Output: (PolyList [])

  Function: polyListProd
  Test Case Number: 2
  Input: (PolyList []) (PolyList [])
  Expected Output: (PolyList [])
  Actual Output: (PolyList [])

  Function: polyListProd
  Test Case Number: 3
  Input: (PolyList [1,3,2]) (PolyList [5,2])
  Expected Output: (PolyList [5,17,16,4])
  Actual Output: (PolyList [5,17,16,4])

-----------------------------------------------------------------

  Function: polyListToPoly
  Test Case Number: 1
  Input: (PolyList [])
  Expected Output: Coef 0
  Actual Output: Coef 0

  Function: polyListToPoly
  Test Case Number: 2
  Input: (PolyList [3])
  Expected Output: Coef 3
  Actual Output: Coef 3

  Function: polyListToPoly
  Test Case Number: 3
  Input: (PolyList [1,2,3])
  Expected Output: Sum (Coef 1) (Prod X (Sum (Coef 2) (Prod X (Coef 3))))
  Actual Output: Sum (Coef 1) (Prod X (Sum (Coef 2) (Prod X (Coef 3))))

-----------------------------------------------------------------

  Function: polyToPolyList
  Test Case Number: 1
  Input: (Sum (Coef 1) (Prod X (Sum (Coef 2) (Prod X (Coef 3)))))
  Expected Output: PolyList [1,2,3]
  Actual Output: PolyList [1,2,3]

  Function: polyToPolyList
  Test Case Number: 2
  Input: (X)
  Expected Output: PolyList [0,1]
  Actual Output: PolyList [0,1]

  Function: polyToPolyList
  Test Case Number: 3
  Input: (Coef 20)
  Expected Output: PolyList [20]
  Actual Output: PolyList [20]

-----------------------------------------------------------------
-----------------------------------------------------------------
 -}