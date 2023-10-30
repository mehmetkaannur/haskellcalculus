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
  fromInteger = undefined
  negate      = undefined
  (+)         = undefined
  (*)         = undefined

instance Fractional Expr where
  fromRational = undefined
  (/)          = undefined

instance Floating Expr where
  sin = undefined
  cos = undefined
  log = undefined

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
showExpr (Add a b) = showExpr a ++ "+" ++ showExpr b
showExpr (Div a b) = showExpr a ++ "/" ++ showExpr b
showExpr (Mul a b) = showExpr a ++ "*" ++ showExpr b
showExpr (Neg a)   = "-(" ++ showExpr a ++ ")"
showExpr (Sin a)   = "sin(" ++ showExpr a ++ ")"
showExpr (Cos a)   = "cos(" ++ showExpr a ++ ")"
showExpr (Log a)   = "log(" ++ showExpr a ++ ")"

{-|
Symbolically differentiates a term with respect to a given identifier.
-}
diff :: Expr -> String -> Expr
diff (Val a) y   = 0
diff (Id a) y    = 1
diff (Add a b) y = (eval a y) + (eval b y)
diff (Div a b) y = (eval a y) / (eval b y)
diff (Mul a b) y = (eval a y) * (eval b y)
diff (Neg a) y   = -(eval a y)
diff (Sin a) y   = sin(eval a y)
diff (Cos a) y   = cos(eval a y)
diff (Log a) y   = log(eval a y)

{-|
Computes the approximation of an expression `f` by expanding the Maclaurin
series on `f` and taking its summation.
-}
maclaurin :: Expr   -- ^ expression to approximate (with `x` free)
          -> Double -- ^ value to give to `x`
          -> Int    -- ^ number of terms to expand
          -> Double -- ^ the approximate result
maclaurin = undefined
