{- Assignment 1
 - Name: Usman Asad
 - Date: September 29th
 -}
module Assign_1 where

macid :: String
macid = "Asadu"

{- -----------------------------------------------------------------
 - cubicQ
 - -----------------------------------------------------------------
 - Description: cubicQ is a function that takes in 3 variables, each
 - of these variables are of type 'Double'. Once evalutad, the func-
 - -tion returns a Q value of type `Double`
 -}
cubicQ :: Double -> Double -> Double -> Double
cubicQ a b c = ((3 * a * c) - (b ** 2))/(9 * (a ** 2))

{- -----------------------------------------------------------------
 - cubicR
 - -----------------------------------------------------------------
 - Description: cubicR is another function that takes in 4 variables,
 - each of these variables are of type 'Double'. Once evalutad using 
 - Cardano's theorems, the function returns a R value of type `Double`
 -}
cubicR :: Double -> Double -> Double -> Double -> Double
cubicR a b c d = ((9 * a * b * c) - (27 * d * (a ** 2)) - (2 * (b ** 3)))/(54 * (a ** 3))

{- -----------------------------------------------------------------
 - cubicDisc
 - -----------------------------------------------------------------
 - Description: cubicDisk is a funtion that calculates the discrimi-
 - -nant of a cubic function. It takes two parameters that are of 
 - type `Double`. It then returns the discriminant as a `Double`.
 - if the absolute value is very close to 0 it will set it to 0
 - this happens because of floating point inacuracies.
 -}
cubicDisc :: Double -> Double -> Double
cubicDisc q r = let
            in if (abs((q ** 3) + (r ** 2)) < 1e-14)
                then 0
                else (q ** 3) + (r ** 2)

{- -----------------------------------------------------------------
 - cubeRoot
 - -----------------------------------------------------------------
 - Description: The function cubeRoot, takes in a singular `Double`   (a ** (1/3))
 - parameter and returns the cube root as a `Double`.
 -}
cubeRoot :: Double -> Double
cubeRoot a = let
         in if (a < 0)
            then -(abs (a)) ** (1/3) 
            else (a ** (1/3)) 

{- -----------------------------------------------------------------
 - cubicS
 - -----------------------------------------------------------------
 - Description: cubicS takes in two `Double` values, operates on them
 - according to Cardano's Theorem and solves for S. Then returns the 
 - value also as a `Double`
 -}
cubicS :: Double -> Double -> Double
cubicS q r = cubeRoot(r + sqrt ((q ** 3) + (r ** 2)))

{- -----------------------------------------------------------------
 - cubicT
 - -----------------------------------------------------------------
 - Description: cubicT takes in two `Double` values, operates on them
 - according to Cardano's Theorem and solves for T. Then returns the 
 - value also as a `Double`
 -}
cubicT :: Double -> Double -> Double
cubicT q r = cubeRoot(r - sqrt ((q ** 3) + (r ** 2)))

{- -----------------------------------------------------------------
 - cubicRealSolutions
 - -----------------------------------------------------------------
 - Description: This function takes 4 `Double` inputs and uses them to
 - find the roots of a cubic function. It calls on previously defined
 - functions to properly use Cardano's Formula. We check if the discr-
 - -iminant is positive,negative or 0. Depending on the result we return
 - different sized list's containt the roots. The lists Type is `Double` 
 -}
cubicRealSolutions :: Double -> Double -> Double -> Double -> [Double]
cubicRealSolutions a b c d = let 
                        q = cubicQ a b c    
                        r = cubicR a b c d 
                        disc = cubicDisc q r
                        s = cubicS q r
                        t = cubicT q r   
                        x1 = s + t - (b / (3 * a)) 
                        x2 = -((s + t)/2) - (b / (3 * a)) + (sqrt(3) * (s - t))/2 
                        in if (disc < 0)
                            then []
                            else if (disc > 0)
                                 then [x1]
                                 else [x1,x2,x2]

{- -----------------------------------------------------------------
 - Test Case for when discriminant is 0
 - -----------------------------------------------------------------
 -}
testCase1 :: String
testCase1 = let
                in if ((cubicRealSolutions 1 6 12 8) == [-2,-2,-2] )
                    then "Test case 1 is True"
                    else "Test case 1 is False"  

{- -----------------------------------------------------------------
 - Test Case for when discriminant is > 0
 - -----------------------------------------------------------------
 -}
testCase2 :: String
testCase2 = let
                 in if ((cubicRealSolutions 1 0 3 0) == [0] )
                     then "Test case 2 is True"
                     else "Test case 2 is False"  
 
{- -----------------------------------------------------------------
 - Test Case for when discriminant is < 0
 - -----------------------------------------------------------------
 -}
testCase3 :: String
testCase3 = let
                 in if ((cubicRealSolutions 1 0 (-3) 0) == [] )
                     then "Test case 3 is True"
                     else "Test case 3 is False"  
 
  
                    
                    


                              