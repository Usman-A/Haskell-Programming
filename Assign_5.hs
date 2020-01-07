{- Assignment 5
 - Name: Usman Asad
 - Date: Dec - 01 -2019
 -}
 module Assign_5 where

    macid :: String
    macid = "Asadu"
    
    
    {- -----------------------------------------------------------------
     - definiteIntegral
     - -----------------------------------------------------------------
     - Description: TODO add comments on definiteIntegral here
     -}
    {- }
    definiteIntegral :: Double -> Double -> (Double -> Double) -> Integer -> Double
    definiteIntegral a b g n
        | a <= b = - definiteIntegral b a g n
        | otherwise = let 
                        delta = ( b - a) / fromIntegral n
                      in delta * sum [ (g x + g ( x + delta ))/2
                                  | x <- [a, a + delta .. b] ]
    -}


 
    defInt2 :: Double -> Double -> Double -> (Double -> Double) -> Integer -> Double
    defInt2 a x b g n
             | (b - a <= 0.000001) = 0 -- checking if the bounds are the same, using a tolerance for accuracy
             | otherwise = let
                             delta = (b-x) / fromIntegral n   -- Delta value (b-x)/n
                           in  
                             delta * (g a + g(a + delta))/2 + defInt2 (a + delta) x b g n
       
    {- -----------------------------------------------------------------
     - definiteIntegral
     - -----------------------------------------------------------------
     - Description: The function definiteIntegral has 4 parameters
     - a and b which is the range for the definite integral (a the
     - lower bound and b the upper bound). It takes in a function
     - g, and lastly takes in n which represents the number of sh-
     - -apes used. It returns the approximate area between the curve
     - and the x axis.
     -}
    definiteIntegral :: Double -> Double -> (Double -> Double) -> Integer -> Double
    definiteIntegral a b g n 
                    | a == b = 0 
                    | a > b = - definiteIntegral b a g n
                    | otherwise = let
                                    delta = (b-a)/(fromIntegral n)
                                  in 
                                    delta * (g a + g(a + delta))/2 + defInt2 (a + delta) a b g n
      
    
    {- -----------------------------------------------------------------
     - funH
     - -----------------------------------------------------------------
     - Description: the function funH takes in an integer n, and returns
     - the area between the curves f(x) = x ^ (1/n) & g(x) = x ^ n. in 
     - the range 0 to 1
     - function uses a set number of shapes (75) for getting the approx
     - definate intergal, this number gives a good enough approximation 
     - for our needs. 
     -}
     
    funH :: Integer -> Double
    funH n 
        | n <= 0 = undefined
        | otherwise =  let
                         area2 = definiteIntegral 0 1 (\x -> x**(fromIntegral n)) 75
                         area1 = definiteIntegral 0 1 (\x -> x**(1.0/(fromIntegral n))) 75
                       in area1 - area2
    

    {- -----------------------------------------------------------------
     - funH
     - -----------------------------------------------------------------
     - Description: the function funH takes in a double n, and returns
     - the area between the curves f(x) = a ^ (x) & g(x) = 0 (x axis). 
     - in the range -1 to 1. 
     - function uses a set number of shapes (75) for getting the approx
     - definate intergal, this number gives a good enough approximation 
     - for our needs. 
     -}
    funK :: Double -> Double
    funK n 
        | n <= 0 = undefined
        | otherwise = definiteIntegral (-1) 1 (\x -> (n)**(x)) 75
         

    
    {- -----------------------------------------------------------------
     - Test Cases
     - -----------------------------------------------------------------
     -
     - -----------------------------------------------------------------
     - - Function: definiteIntegral
     - - Test Case Number: 1
     - - Input: (0) (1) (\x -> x) 1
     - - Expected Output: 0.5
     - - Acutal Output: 0.5
 
     - - Function: definiteIntegral
     - - Test Case Number: 2
     - - Input: (-2) (2) (\x -> x) 4
     - - Expected Output: 0.0
     - - Acutal Output: 0.0

     - - Function: definiteIntegral
     - - Test Case Number: 3
     - - Input: (-1) (3) (\x -> x**2) 10
     - - Expected Output: 9.44
     - - Acutal Output: 9.44
     - -----------------------------------------------------------------
     - -----------------------------------------------------------------
     - - Function: funH
     - - Test Case Number: 1
     - - Input: (-1)
     - - Expected Output: undefined
     - - Acutal Output: Exception: undefined
 
     - - Function: funH
     - - Test Case Number: 2
     - - Input: 1
     - - Expected Output: 0.0
     - - Acutal Output: 0.0

     - - Function: funH
     - - Test Case Number: 3
     - - Input: 4
     - - Expected Output: 0.598493
     - - Acutal Output: 0.5984925498135634
     - -----------------------------------------------------------------
     - -----------------------------------------------------------------
     - - Function: funK
     - - Test Case Number: 1
     - - Input: (0) 
     - - Expected Output: Undefined
     - - Acutal Output: Exception: undefined
 
     - - Function: funK
     - - Test Case Number: 2
     - - Input: 1
     - - Expected Output: 1.9999999
     - - Acutal Output: 1.9999999999999973

     - - Function: funk
     - - Test Case Number: 3
     - - Input: 50
     - - Expected Output: 12.7825
     - - Acutal Output: 12.782515252869478
     - -----------------------------------------------------------------
     -}
{-
Quick check test cases

Function: defIntegralProp
Property : definiteIntegral a a (\x -> x) 75 == 0
Actual Test Result: Pass

Function: funHProp
Property : funH 6 == 0.7117921937233472
Actual Test Result: Pass

Function: funKProp
Property : funK 3 == 2.427402257978144
Actual Test Result: Pass

-}
     

    