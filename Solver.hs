{-# LANGUAGE GADTs #-}

module Solver where

import Formula


-- Evaluating terms
-- ----------------

eval :: Term t -> t
eval (Con i) = i
eval (And e1 e2) = eval e1 && eval e2
eval (Or e1 e2) = eval e1 || eval e2
eval (Smaller i1 i2) = eval i1 < eval i2
eval (Plus i1 i2) = eval i1 + eval i2
eval (Name _) = error "eval: Name"    -- this constructor is not relevant for evaluation


-- Checking formulas
-- -----------------

satisfiable :: Formula ts -> Bool
satisfiable (Body x) = eval x
satisfiable (Forall xs f) = findTrue $ map (\x -> satisfiable $ f $ Con x) xs

solutions :: Formula ts -> [ts]
solutions x = selectTrue (mapChoices x) (mapBool x)

-- Helper

-- Return True if there is at least 1 True inside a list of Bool
findTrue :: [Bool] -> Bool
findTrue [] = False
findTrue (True : xs) = True
findTrue (False : xs) = findTrue xs

--Return all possible choice for solution, regardless it's False or True
mapChoices :: Formula ts -> [ts]
mapChoices (Body x) = [()]
mapChoices (Forall xs f) = concat $ map (\x -> map (\y -> (x,y)) $ mapChoices $ f $ Con x) xs

--Return the Boolean mapping for the choice
mapBool :: Formula ts -> [Bool]
mapBool (Body x) = [eval x]
mapBool (Forall xs f) = concat $ map (\x -> mapBool $ f $ Con x) xs

--Return only the True choice
selectTrue :: [ts] -> [Bool] -> [ts]
selectTrue [] [] = []
selectTrue (x : xs) (True : ys) = x : selectTrue xs ys
selectTrue (x : xs) (False : ys) =	selectTrue xs ys