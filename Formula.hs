{-# LANGUAGE GADTs #-}

module Formula where

-- Datatype of formulas
-- --------------------

data Formula ts where
  Body   :: Term Bool                     -> Formula ()
  Forall :: Show a 
         => [a] -> (Term a -> Formula as) -> Formula (a, as)

data Term t where
  Con     :: t -> Term t
  And     :: Term Bool -> Term Bool -> Term Bool
  Or      :: Term Bool -> Term Bool -> Term Bool
  Smaller :: Term Int -> Term Int -> Term Bool
  Plus    :: Term Int -> Term Int -> Term Int
  Name    :: String -> Term t    -- to facilitate pretty printing

-- Pretty printing formulas
-- ------------------------

instance Show t => Show (Term t) where
  show (Con v)       = show v
  show (And p q)     = "(" ++ show p ++ " && " ++ show q ++ ")"
  show (Or p q)      = "(" ++ show p ++ " || " ++ show q ++ ")"
  show (Smaller n m) = "(" ++ show n ++ " < "  ++ show m ++ ")"
  show (Plus n m)    = "(" ++ show n ++ " + "  ++ show m ++ ")"
  show (Name name)   = name

instance Show (Formula ts) where
  show = show' ['x' : show i | i <- [0..]]
    where
      show' :: [String] -> Formula ts' -> String
      show' ns     (Body body)   = show body
      show' (n:ns) (Forall vs p) = "forall " ++ n ++ "::" ++ show vs ++ ". " ++ show' ns (p (Name n))


-- Example formulas
-- ----------------

ex1 :: Formula ()
ex1 = Body (Con True)

ex2 :: Formula (Int, ())
ex2 = Forall [1..10] $ \n ->
        Body $ n `Smaller` (n `Plus` Con 1)

ex3 :: Formula (Bool, (Int, ()))
ex3 = Forall [False, True] $ \p -> 
      Forall [0..2] $ \n -> 
        Body $ p `Or` (Con 0 `Smaller` n)

ex4 :: Formula (Bool, (Int, ()))
ex4 = Forall [False, True] $ \p -> 
      Forall [0..2] $ \n -> 
        Body $ p `And` (Con 3 `Smaller` n)

ex5 :: Formula (Bool, (Int, ()))
ex5 = Forall [False, True] $ \p -> 
      Forall [3..5] $ \n -> 
        Body $ p `Or` ((Con 3 `Smaller` (n `Plus` Con 1)) `And` ((n `Plus` Con 1) `Smaller` Con 5))

ex6 :: Formula (Bool, (Int, (Int, ())))
ex6 = Forall [False, False] $ \p -> 
      Forall [0..2] $ \n -> 
      Forall [1..3] $ \o -> 
        Body $ p `Or` (o `Smaller` n) 

--no possible solution
impossible1 :: Formula (Int, (Bool, ()))
impossible1 = Forall [0,1,2,3] $ \p ->
      Forall [False] $ \n ->
        Body $ n `Or` (p `Smaller` Con 0)

--testing for plus
ex7 :: Formula (Int, (Int, (Int, ())))
ex7 = Forall [0,1,2,3] $ \p ->
    Forall [0,1,2,3] $ \q ->
    Forall [0,1,2,3] $ \g ->
      Body $ ((p `Plus` q) `Smaller` g)

--testing for plus
ex8 :: Formula (Int, (Int, (Int, ())))
ex8 = Forall [0,1,2,3] $ \p ->
    Forall [0,1,2,3] $ \q ->
    Forall [0,1,2,3] $ \g ->
      Body $ ((g `Plus` p `Plus` Con 4) `Smaller` (q `Plus` Con 4))               