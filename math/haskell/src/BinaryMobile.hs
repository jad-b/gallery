module BinaryMobile where

-- SICP Ex 2.29
data Branch = Branch
    { length :: Int -- Length of the branch
    , structure :: Structure -- Node at the end of the branch
    } deriving Show

data Structure = BinaryMobile -- New sub-tree of the mobile
    { left :: Branch
    , right :: Branch
    }
    | Weight --
    { weight :: Int }
    deriving Show
