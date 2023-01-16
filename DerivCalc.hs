data MathExpr a =
    X
  | Coef a
  | Add (MathExpr a) (MathExpr a)
  | Mult (MathExpr a) (MathExpr a)
  | Power (MathExpr a) Int
  | Cos (MathExpr a)
  | Sin (MathExpr a)
  | Abs (MathExpr a)
  deriving (Eq,Show,Read)

eval :: (Floating a, Eq a) => MathExpr a -> a -> a
eval X v = v
eval (Coef n) _ = n
eval (Add x y) v = eval x v + eval y v
eval (Mult x y) v = eval x v * eval y v
eval (Power x n) v = eval x v ^^ n
eval (Cos x) v = cos (eval x v)
eval (Sin x) v = sin (eval x v)
eval (Abs x) v = abs (eval x v)

instance Num a => Num (MathExpr a) where
  x + y         = Add x y
  x * y         = Mult x y
  negate x      = Mult x (Coef (-1)) 
  abs x         = Abs x
  fromInteger i = Coef (fromInteger i)
  
instance Fractional a => Fractional (MathExpr a) where
  recip a        = Power a (-1)
  fromRational a = Coef (fromRational a)

instance Floating a => Floating (MathExpr a) where
  pi      = Coef pi
  sin     = Sin
  cos     = Cos

diff :: (Floating a, Eq a) => MathExpr a -> MathExpr a
diff X = Coef 1
diff (Coef _) = Coef 0
diff (Add x y) = diffX + diffY
  where 
    diffX = diff x
    diffY = diff y
diff (Mult x y) = diffX * y + x * diffY
  where
    diffX = diff x
    diffY = diff y
diff (Power x n) = coeff * exponentRule * diffX
  where
    coeff = fromIntegral n
    exponentRule = Power (x) (n-1)
    diffX = diff x
diff (Sin x) = sinX * diffX
  where
    sinX = Sin x
    diffX = diff x
diff (Cos x) = cosX * diffX
  where
    cosX = Sin x
    diffX = diff x
diff (Abs x) = x * fractionalAbsX * diffX
  where
    fractionalAbsX = recip (abs x)
    diffX = diff x
