{-# LANGUAGE GADTs #-}

module MyTest where

import Formula
import Solver

--no possible solution
impossible1 :: Formula (Int, (Bool, ()))
impossible1 = Forall [0,1,2,3] $ \p ->
      Forall [False] $ \n ->
        Body $ n `Or` (p `Smaller` Con 0)

--testing for plus
ex4 :: Formula (Int, (Int, (Int, ())))
ex4 = Forall [0,1,2,3] $ \p ->
    Forall [0,1,2,3] $ \q ->
    Forall [0,1,2,3] $ \g ->
      Body $ ((p `Plus` q) `Smaller` g)

--testing for plus
ex5 :: Formula (Int, (Int, (Int, ())))
ex5 = Forall [0,1,2,3] $ \p ->
    Forall [0,1,2,3] $ \q ->
    Forall [0,1,2,3] $ \g ->
      Body $ ((g `Plus` p `Plus` Con 4) `Smaller` (q `Plus` Con 4))