{-# LANGUAGE Unsafe #-}
{-# LANGUAGE ViewPatterns #-}


--------------------------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2018 Nathan Waivio
-- License     :  BSD3
-- Maintainer  :  Nathan Waivio <nathan.waivio@gmail.com>
-- Stability   :  Stable
-- Portability :  unportable
-- 
-- Interface functions of Cl3 types to/from HMatrix. 
-- 
-------------------------------------------------------------------


module Algebra.Geometric.Cl3.HMatrixInterface
(
 toHMatrix, 
 fromHMatrix
) where

import safe Algebra.Geometric.Cl3 (Cl3(..), toAPS, reduce)
import Numeric.LinearAlgebra.Data (Matrix, atIndex, (><))
import safe Data.Complex (Complex(..))

-- | 'toHMatrix' Convert a Cl3 Cliffor to a HMatrix Matrix
toHMatrix :: Cl3 -> Matrix (Complex Double)
toHMatrix (toAPS -> APS a0 a1 a2 a3 a23 a31 a12 a123) = 
  let a = (a0+a3) :+ (a123+a12)
      b = (a1+a31) :+ (a23-a2)
      c = (a1-a31) :+ (a23+a2)
      d = (a0-a3) :+ (a123-a12)
  in (2><2) [a,b,c,d]
toHMatrix _ = error "Pattern Matching failure of toHMatrix for the toAPS/APS View Pattern"

-- | 'fromHMatrix' Convert from a HMatrix Matrix (2x2 Complex Double hopefully...) to a Cl3 Cliffor
fromHMatrix :: Matrix (Complex Double) -> Cl3
fromHMatrix mat =
  let (a :+ ai) = mat `atIndex` (0,0)
      (b :+ bi) = mat `atIndex` (0,1)
      (c :+ ci) = mat `atIndex` (1,0)
      (d :+ di) = mat `atIndex` (1,1)
      a0 = (a + d) / 2
      a1 = (b + c) / 2
      a2 = (ci - bi) / 2
      a3 = (a - d) / 2
      a23 = (ci + bi) / 2
      a31 = (b - c) / 2
      a12 = (ai - di) / 2
      a123 = (ai + di) / 2
  in reduce (APS a0 a1 a2 a3 a23 a31 a12 a123)

