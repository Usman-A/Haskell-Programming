{- Assignment 2
 - Name: Usman Asad
 - Date: 17/10/2019
 -}
module Assign_2 where

macid :: String
macid = "Asadu"

type GaussianInt = (Integer,Integer)

{- -----------------------------------------------------------------
 - gaussReal
 - -----------------------------------------------------------------
 - Description: gaussReal returns the "real number" part of a Gauss-
 - -ian Integer. Where x is the real, and y is the imaginary part. 
 - A gaussian int takes the form of (x + yi)
 -}
gaussReal :: GaussianInt -> Integer
gaussReal (x,y) = x

{- -----------------------------------------------------------------
 - gaussImag
 - -----------------------------------------------------------------
 - Description: gaussImag returns the "Imaginary" part of a Gauss-
 - -ian Integer. Where x is the real, and y is the imaginary part. 
 - A gaussian int takes the form of (x + yi)
 -}
gaussImag :: GaussianInt -> Integer
gaussImag (x,y) = y


{- -----------------------------------------------------------------
 - gausConj
 - -----------------------------------------------------------------
 - Description: gaussConj returns the conjugate of the given Gaussian
 - Integer. The conjugate is just the negative of the imaginary part
 - of the integer.
 -}
gaussConj :: GaussianInt -> GaussianInt
gaussConj (x,y) = (x,-y)

{- -----------------------------------------------------------------
 - gaussAdd
 - -----------------------------------------------------------------
 - Description: gaussAdd returns the sum of the two given Gaussian
 - Integers. The sum of gaussian numbers is as simple as adding the
 - two real parts with each other, and the two imaginary parts with 
 - each other. The returned value is also of type GaussianInt
 -}
gaussAdd :: GaussianInt -> GaussianInt -> GaussianInt
gaussAdd (x0,y0) (x1,y1) = (x0 + x1, y0 + y1)
    


{- -----------------------------------------------------------------
 - gaussMult
 - -----------------------------------------------------------------
 - Description: gaussMult returns the product of the two given Gaussian
 - Integers. This function just uses the Gaussian formula used to find
 - the imaginary part and the real part of the product. The returned
 - value is also of type GaussianInt
 -}
gaussMult :: GaussianInt -> GaussianInt -> GaussianInt
gaussMult (x0,y0) (x1,y1) = ((x0*x1 - y0*y1), (x0*y1 + y0*x1))

{- -----------------------------------------------------------------
 - gaussNorm
 - -----------------------------------------------------------------
 - Description: gaussNorm returns the Normal value of the two given 
 - Gaussian Integers. The Normal is found by multiplying the Gauss-
 - -ian Integer with its conjugate. The Normal value returned is 
 - of type Int. 
 -}
gaussNorm :: GaussianInt -> Integer
gaussNorm (x,y) = gaussReal(gaussMult (x,y) (gaussConj (x,y)))


{- -----------------------------------------------------------------
 - getIndex
 - -----------------------------------------------------------------
 - Description: The function getIndex, takes a list of integers, a
 - number within the list and a pointer value and returns the index 
 - of the item in the List.  If there are multiple instances of the 
 - same value in the list, it will return the first occurance of the 
 - item. 
 - I couldn't figure out a way to count the iterations of a recursive
 - function without including it as a parameter of the function. This
 - function `getIndex` will only work if the pointer is set to 0 when 
 - it is first called. It will also only work if the item is actually
 - within the list.  
 -}
getIndex :: ([Integer],Integer,Int) -> Int
getIndex (list,item,pointer)
        | list == [] = 0
        | head(list) == item = pointer
        | otherwise = getIndex (tail(list),item,(pointer + 1))
   
        
{- -----------------------------------------------------------------
 - maxGaussNorm
 - -----------------------------------------------------------------
 - Description: maxGaussNorm takes in a list of Gaussian Integers and
 - returns the Gaussian Int with the greatest Normal. This is achieved 
 - by converting the list of Gaussian Integers to one consisting of their
 - normals, and then finding the position of its maximum. Using this 
 - position, you can return the Gaussian Integer in the list with the  
 - greatest Normal.
 -}
maxGaussNorm :: [GaussianInt] -> GaussianInt
maxGaussNorm [] = (0,0)
maxGaussNorm gs = let
            normList = [gaussNorm(x) | x <- gs]
            in gs !! (getIndex(normList, maximum(normList), 0))
  
            


{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -
 - ----------------------------------------------------------------- 
 - -----------------------------------------------------------------
 - - Function: gaussConj
 - - Test Case Number: 1
 - - Input: (1,2)
 - - Expected Output: (1,-2)
 - - Acutal Output: (1,-2)
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: gaussConj
 - - Test Case Number: 2
 - - Input: (2,-3)
 - - Expected Output: (2,3)
 - - Acutal Output: (2,3)
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: gaussConj
 - - Test Case Number: 3
 - - Input: (0,0)
 - - Expected Output: (0,0)
 - - Acutal Output: (0,0)
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: gaussAdd
 - - Test Case Number: 1
 - - Input: (1,1) (2,3)
 - - Expected Output: (3,4)
 - - Acutal Output: (3,4)
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: gaussAdd
 - - Test Case Number: 2
 - - Input: (-3,-6) (5,8)
 - - Expected Output: (2,2)
 - - Acutal Output: (2,2)
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: gaussAdd
 - - Test Case Number: 3
 - - Input: (1,-4) (0,0)
 - - Expected Output: (1,-4)
 - - Acutal Output: (1,-4)
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: gaussMult
 - - Test Case Number: 1
 - - Input: (1,1) (2,-5)
 - - Expected Output: (7,-3)
 - - Acutal Output: (7,-3)
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: gaussMult
 - - Test Case Number: 2
 - - Input: (0,0) (1,2)
 - - Expected Output: (0,0)
 - - Acutal Output: (0,0)
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: gaussMult
 - - Test Case Number: 3
 - - Input: (-5,-3) (-11,-1)
 - - Expected Output: (52,38)
 - - Acutal Output: (52,38)
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: gaussNorm
 - - Test Case Number: 1
 - - Input: (0,0)
 - - Expected Output: 0
 - - Acutal Output: 0
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: gaussNorm
 - - Test Case Number: 2
 - - Input: (3,4)
 - - Expected Output: 25
 - - Acutal Output: 25
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: gaussNorm
 - - Test Case Number: 3
 - - Input: (-3,-4)
 - - Expected Output: 25
 - - Acutal Output: 25
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: gaussNorm
 - - Test Case Number: 4
 - - Input: (-2,1)
 - - Expected Output: 5
 - - Acutal Output: 5
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: maxGaussNorm
 - - Test Case Number: 1
 - - Input: []
 - - Expected Output: (0,0)
 - - Acutal Output: (0,0)
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: maxGaussNorm
 - - Test Case Number: 2
 - - Input: [(1,1),(0,0),(3,4),(9,5),(5,6),(4,5)]
 - - Expected Output: (9,5)
 - - Acutal Output: (9,5)
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: maxGaussNorm
 - - Test Case Number: 3
 - - Input: [(-3,-4),(-3,-4),(-3,-4),(-3,-4),(-3,-4),(-3,-4)]
 - - Expected Output: (-3,-4)
 - - Acutal Output: (-3,-4)
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: maxGaussNorm
 - - Test Case Number: 4
 - - Input: [(2,-4),(5,6),(1,1),(3,-5),(-12,-7),(2,2)]
 - - Expected Output: (-12,-7)
 - - Acutal Output: (-12,-7)
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: maxGaussNorm
 - - Test Case Number: 5
 - - Input: [(1,1)]
 - - Expected Output: (1,1)
 - - Acutal Output: (1,1)
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 -}
