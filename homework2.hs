class Eval a where
  eval :: a -> Integer

data Lit = Lit Integer
data Add a b = Add a b
data Mul a b = Mul a b
data Sub a b = Sub a b

instance Eval Lit where
  eval (Lit n) = n

instance Eval (Add x y) where
  eval (Add x y) = eval(x) + eval(y)

instance Eval (Mul a b) where
  eval (Mul a b) = eval(a) * eval(b)

instance Eval(Sub c d) where
  eval (Sub c d) = eval(c) - eval(d)
