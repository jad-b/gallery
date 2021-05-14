{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-
   Examples:
   * type
   * data
   * newtype
   * Typeclass
   * Type family
   * Data Family
   * Existential Qualification
-}
module Example where

import Prelude hiding (lookup)

--import Prelude (Bool(..), Int, Maybe(..), (==))
--import qualified Prelude as P
{- Type classes define sets of functions, key'd by type variable.

   Eq a => {
        (Int, { (==) :: Int -> Int -> bool
              , (==) :: Int -> Int -> bool }),
        (String, { (==) :: String -> String -> bool
                 , (==) :: String -> String -> bool }),
        (t_1, ...and so on)
   }

   When To use
   -----------
   Great SO answer on "when Typeclass, when Data": https://stackoverflow.com/a/12288184
-}
class Eq2 a where
  (=^=) :: a -> a -> Bool
  (\^=) :: a -> a -> Bool

instance Eq2 Int where
  (=^=) a b = a == b
  (\^=) a b = a == b

{-|
= Multi-Parameter Typeclasses

Multi-parameter typeclasses allow you to define a relation between type _tuples_ => behaviors.

They are the right choice in a few different situations:

1. Implementation depends on more than one type
2. We need to add Constraints on a per-instance basis. E.g., implementing a
generic Map needs Hashable elements when backeby a HashMap, but needs Ord
elements when using a Tree.
3. You want to encode a relationship between types. For instance:

    class Iso a b where
        iso :: a -> b
        osi :: b -> a
-}
class Collection c a where
  cUmpty :: c a
  cUnsert :: a -> c a -> c a
  cUnion :: c a -> c a -> c a

-- S3-like Bucket representation
class GenericBucket b k v where
  gput :: k -> v -> b k v -> b k v

{- FIXME
instance GenericBucket HM.HashMap Int String where
  gput k v b = HM.insert k v b
-}

{-|
= Type families

Type families create a mapping between the input types and the return value


* When to Use

  * Your return type depends on the specifics of your input types

Refs:
1. https://channel9.msdn.com/posts/MDCC-TechTalk-Fun-with-type-functions
2. https://serokell.io/blog/type-families-haskell
-}
class GNum a b
    -- Each associated type gets a kind signature
    -- The kind of SumTy is * -> * -> *, a _type function_: a function whose
    -- variables are types. It uses a & b to compute the final type,
    -- represented here by '*'.
  where
  type SumTy a b :: *
    -- Each method gets a type signature
  (+) :: a -> b -> SumTy a b

instance GNum Int Float where
  type SumTy Int Float = Float
  (+) x y = (sumFloat) (intToFloat x) (y)

-- Define a couple stub functions to get this to compile
intToFloat :: Int -> Float
intToFloat = undefined

sumFloat :: Float -> Float -> Float
sumFloat = undefined

{-
= Data Families
Data families create a [Type Variable] -> Data representation mapping.
We say the class type variables "index" the constructors.

* When to Use

1. Your implementation differs based on the types being provided.

Refs:
1. https://channel9.msdn.com/posts/MDCC-TechTalk-Fun-with-type-functions
2. https://serokell.io/blog/type-families-haskell

class Key k
  where
  data Map k :: * -> *
  empty :: Map k v
  lookup :: k -> Map k v -> Maybe v

instance Key Bool where
  data Map Bool v = MB (Maybe v) (Maybe v)
  empty = MB Nothing Nothing
  lookup True (MB _ mt) = mt
  lookup False (MB mf _) = mf

instance (Key a, Key b) => Key (a, b) where
  data Map (a, b) v = MP (Map a (Map b v))
  empty = MP . empty
  lookup (ka, kb) (MP m) =
    case lookup ka m of
      Nothing -> Nothing
      Just m2 -> lookup kb m2
-}
{-
= Existential Types

* When To Use
1. Hiding details of exported types from users of your API
-}
