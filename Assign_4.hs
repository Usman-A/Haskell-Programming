{- Assignment 4
 - Name: Usman Asad
 - Date: Nov 16th 2019
 -}
module Assign_4 where

macid :: String
macid = "Asadu"

data MathExpr a = X
                | Coef a
                | Sum (MathExpr a) (MathExpr a)
                | Prod (MathExpr a) (MathExpr a)
                | Quot (MathExpr a) (MathExpr a)
                | Exp (MathExpr a)
                | Log (MathExpr a)
                deriving (Eq,Show,Read)

{- -----------------------------------------------------------------
 - value
 - -----------------------------------------------------------------
 - Description: The function value, takes in a  MathExpr data type
 - and evaluates the expression. Let the expression be f(x), this
 - function returns the value f(n).
 -}
value :: (Floating a, Eq a) => MathExpr a -> a -> a
value (Coef a) n = a
value (X) n = n
value (Sum expr1 expr2) n = (value expr1 n) + (value expr2 n)
value (Prod expr1 expr2) n = (value expr1 n) * (value expr2 n)
value (Quot expr1 expr2) n = (value expr1 n) / (value expr2 n)
value (Exp expr) n = exp (value expr n)
value (Log expr) n = log (value expr n)

{- -----------------------------------------------------------------
 - simp
 - -----------------------------------------------------------------
 - Description: The function Simp, takes in a paramter of type
 - MathExpr, and simplifies it using the simplification rules
 - stated on the assignment sheet, and returns a simplified 
 - MathExpr.
 -}
simp :: (Floating a, Eq a) => MathExpr a -> MathExpr a
simp (Coef a) = (Coef a)
simp (X) = (X)
simp (Sum (Coef  0.0) u) = simp u
simp (Sum u (Coef  0.0)) = simp u
simp (Prod (Coef  0.0) u) = simp (Coef  0.0)
simp (Prod u (Coef  0.0)) = simp (Coef  0.0)
simp (Prod (Coef  1.0) u) = simp u
simp (Prod u (Coef  1.0)) = simp u
simp (Quot u (Coef  1.0)) = simp u
simp (Quot u (Coef  0.0)) = simp (Coef (value (Quot u (Coef  0.0)) 0))
simp (Quot (Coef  0.0) u) = simp (Coef  0.0)
simp (Exp (Coef 0.0)) = simp (Coef  0.0)
simp (Log (Coef 1.0)) = simp (Coef  1.0)
simp (Sum u v) =
  let
    u' = simp u
    v' = simp v
  in 
    if u' == u && v' == v
      then (Sum u v)
      else  simp (Sum u' v')
simp (Prod u v) =
  let
    u' = simp u
    v' = simp v
  in 
    if u' == u && v' == v
      then (Prod u v)
      else  simp (Prod u' v')
simp (Quot u v) =
  let
    u' = simp u
    v' = simp v
  in 
    if u' == u && v' == v
      then (Quot u v)
      else  simp (Quot u' v')
simp (Exp u) =
  let
    u' = simp u
  in 
    if u' == u
      then (Exp u)
      else  simp (Exp u')
simp (Log u) =
  let
    u' = simp u
  in 
    if u' == u
      then (Log u)
      else  simp (Log u')      
      
{- -----------------------------------------------------------------
 - diff
 - -----------------------------------------------------------------
 - Description: This function takes in a MathExpr data type as a pa-
 - -rameter and returns the expressions Derrivitive, also in form of
 - a MathExpr. Before returning the Derrivative, this function first
 - simplifies it using the simp function.
 -}
diff :: (Floating a, Eq a) => MathExpr a -> MathExpr a
diff (Coef a) = (Coef 0)
diff (X) = (Coef 1)
diff (Sum (expr1) (expr2)) = simp (Sum (diff expr1) (diff expr2))
diff (Prod (expr1) (expr2)) = simp (Sum (Prod (diff expr1) (expr2)) (Prod (expr1) (diff expr2)))
diff (Exp (expr1)) = simp (Prod (Exp (expr1)) (diff expr1))
diff (Log (expr1)) = simp (Quot (diff (expr1)) (expr1))

                          
{- -----------------------------------------------------------------
 - readDiffWrite
 - -----------------------------------------------------------------
 - Description: Takes in a filePath to a file containing a MathExpr, gets its 
 - derrivative simplifies it and writes it to a file with the outputFile file
 - path.
 -}
readDiffWrite :: FilePath -> FilePath -> IO ()
readDiffWrite inputFile outputFile = do
                                     inpStr <- readFile inputFile 
                                     writeFile outputFile (show (simp (diff (read inpStr :: MathExpr Double))))
                                        

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 - - Function: value
 - - Test Case Number: 1
 - - Input: (Coef 4) 2
 - - Expected Output: 4.0
 - - Acutal Output: 4.0

 - - Function: value
 - - Test Case Number: 2
 - - Input: (X) 2
 - - Expected Output: 2.0
 - - Acutal Output: 2.0

 - - Function: value
 - - Test Case Number: 3
 - - Input: (Prod (X) (10)) 2
 - - Expected Output: 20.0
 - - Acutal Output: 20.0
==================================
 - - Function: simp
 - - Test Case Number: 1
 - - Input: Quot (X) (Coef  0.0)
 - - Expected Output: Infinity
 - - Acutal Output: Infinity

 - - Function: simp
 - - Test Case Number: 2
 - - Input: Quot (Coef  0) (X) 
 - - Expected Output: 0
 - - Acutal Output: 0

  - - Function: simp
 - - Test Case Number: 3
 - - Input: Prod (Coef  0) (X) 
 - - Expected Output: 0
 - - Acutal Output: 0
====================================
 - - Function: diff
 - - Test Case Number: 1
 - - Input: (Coef  1) 
 - - Expected Output: (Coef 0)
 - - Acutal Output: (Coef 0)
 
 - - Function: diff
 - - Test Case Number: 2
 - - Input: Sum (Coef  1) (X)
 - - Expected Output: (Coef 1)
 - - Acutal Output: (Coef 1)

 - - Function: diff
 - - Test Case Number: 3
 - - Input: Prod (X) (X)
 - - Expected Output: Prod (Coef 2) (X)
 - - Acutal Output: Prod (Coef 2) (X)
 =====================================
 - - Function: readDiffWrite
 - - Test Case Number: 1
 - - Input File Contents:  (Coef  1) 
 - - Expected File Contents: (Coef 0)
 - - Acutal File Contents: (Coef 0)

 - - Function: readDiffWrite
 - - Test Case Number: 2
 - - Input File Contents:  (Sum (Coef 4) (Coef 5)) 
 - - Expected File Contents: (Coef 0)
 - - Acutal File Contents: (Coef 0)

 - - Function: readDiffWrite
 - - Test Case Number: 3
 - - Input File Contents:  (Log (X)) 
 - - Expected File Contents: (Quot (Coef 1) (X))
 - - Acutal File Contents: (Quot (Coef 1) (X))

 - -----------------------------------------------------------------

 -}

