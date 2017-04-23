module Lib (
  euclids
) where

euclids x 0 = x
euclids x y = euclids y (mod x y)
