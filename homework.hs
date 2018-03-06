data Expr
  = Lit Integer
  | Add Expr Expr
  | Mul Expr Expr
  deriving (Eq, Show)

eval :: Expr -> Integer
eval(Lit c) = c
eval(Add x y) = eval(x) + eval(y)
eval(Mul a b) = eval(a) * eval(b)
