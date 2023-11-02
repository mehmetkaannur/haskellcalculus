{-# LANGUAGE StandaloneDeriving #-}
module Calculus (lookUp, eval, showExpr, diff, maclaurin) where

import Vars
import Expr

import Data.Maybe

type Env = [(String, Double)]

---------------------------------------------------------------------------
-- Type classes and class instances

-- Comment this out if you want to implement your own instance in terms
-- of `showExpr`
deriving instance Show Expr

instance Num Expr where
  fromInteger n = Val (fromInteger n)
  negate a      = Neg a
  (+) a b       = Add a b
  (*) a b       = Mul a b

instance Fractional Expr where
  fromRational a = Val (fromRational a)
  (/) a b        = Div a b

instance Floating Expr where
  sin = Sin
  cos = Cos
  log = Log

---------------------------------------------------------------------------

lookUp :: Eq a => a -> [(a, b)] -> b
lookUp a b = x
  where
    x = fromJust(lookup a b)

{-|
Evaluates a given expression, evaluating any variables to their value within
the provided environment.
-}
eval :: Expr -> Env -> Double
eval (Val a) y   = a
eval (Id a) y    = lookUp a y
eval (Add a b) y = (eval a y) + (eval b y)
eval (Div a b) y = (eval a y) / (eval b y)
eval (Mul a b) y = (eval a y) * (eval b y)
eval (Neg a) y   = -(eval a y)
eval (Sin a) y   = sin(eval a y)
eval (Cos a) y   = cos(eval a y)
eval (Log a) y   = log(eval a y)

{-| OPTIONAL
Pretty prints an expression to a more human-readable form.
-}
showExpr :: Expr -> String
showExpr (Val a)   = show a
showExpr (Id a)    = a
showExpr (Add a b) = "(" ++ showExpr a ++ "+" ++ showExpr b ++ ")"
showExpr (Div a b) = "(" ++ showExpr a ++ "/" ++ showExpr b ++ ")"
showExpr (Mul a b) = "(" ++ showExpr a ++ "*" ++ showExpr b ++ ")"
showExpr (Neg a)   = "-(" ++ showExpr a ++ ")"
showExpr (Sin a)   = "sin(" ++ showExpr a ++ ")"
showExpr (Cos a)   = "cos(" ++ showExpr a ++ ")"
showExpr (Log a)   = "log(" ++ showExpr a ++ ")"

{-|
Symbolically differentiates a term with respect to a given identifier.
-}
diff :: Expr -> String -> Expr
diff (Val a) _   = Val 0.0
diff (Id a) x 
  | a == x       = Val 1.0
  | otherwise    = Val 0.0
diff (Neg a) x   = Neg(diff a x)
diff (Mul a b) x = Add (Mul a (diff b x)) (Mul (diff a x) b)
diff (Add a b) x = Add (diff a x) (diff b x)
diff (Div a b) x = Div (Add (Mul (diff a x) b) (Neg (Mul a (diff b x)))) (Mul b b)
diff (Sin a) x   = Mul (Cos a) (diff a x)
diff (Cos a) x   = Neg (Mul (Sin a) (diff a x))
diff (Log a) x   = Div (diff a x) a 

{-|
Computes the approximation of an expression `f` by expanding the Maclaurin
series on `f` and taking its summation.
-}
maclaurin :: Expr   -- ^ expression to approximate (with `x` free)
          -> Double -- ^ value to give to `x`
          -> Int    -- ^ number of terms to expand
          -> Double -- ^ the approximate result
maclaurin f x n = sum(zipWith (*) evalList (zipWith (/) coeffList factList))
  where
    diffList = take n $ iterate (`diff` "x") f
    evalList = map (\exp -> eval exp [("x", 0.0)]) diffList
    coeffList = take n $ iterate (* x) 1
    factList = take n $ scanl (*) 1[1..]