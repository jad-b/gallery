module HuttonsRazor where

data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval = error "do it to it"
