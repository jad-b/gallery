module BinaryMobile where

-- Update: A more compact implementation
data BinMob a = Mass a | Lever Int (BinMob a) (BinMob a)

-- SICP Ex 2.29
data Branch = Branch
    { length :: Int -- Length of the branch
    , structure :: Structure -- Node at the end of the branch
    } deriving Show

data Structure = BinaryMobile -- New sub-tree of the mobile
    { left :: Branch
    , right :: Branch
    }
    | Weight
    { weight :: Int }
    deriving Show

totalWeight :: Structure -> Int
totalWeight bm = heavy (left bm) + heavy (right bm)
    where heavy br = case structure br of
            Weight w ->  w
            BinaryMobile _ _ -> (totalWeight . structure) br

torque :: Branch -> Int
torque br = BinaryMobile.length br * lift (structure br)
    where lift s = case s of
            Weight w -> w
            BinaryMobile l r -> torque l + torque r

isBalanced :: Structure -> Bool
isBalanced s = fst (helper s)
    where helper s = case s of
            Weight w -> (True, w)
            BinaryMobile l r -> do
                let lBr = helper (structure l)
                let rBr = helper (structure r)
                -- Check if both branches believe themselves to be balanced
                (fst lBr == fst rBr &&
                  -- Compare the torque of each side
                 (BinaryMobile.length l * snd lBr) == (BinaryMobile.length r * snd rBr),
                 -- Return the combined weight of this mobile
                 snd lBr + snd rBr)
