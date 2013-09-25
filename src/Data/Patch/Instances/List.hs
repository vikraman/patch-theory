{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Patch.Instances.List where

import Unsafe.Coerce

import Data.Patch
import Data.Patch.Commute
import Data.Patch.Invert

data Primitive a from to where
  Change :: Eq a => [a] -> Int -> a -> a -> Primitive a from to

instance Invert (Primitive a) where
  invert (Change l i a b) = Change l i b a

instance Commute (Primitive a) (Primitive a) where
  commute (p `Then` q) = case (p, q) of
    (Change l i x y, Change l' j z w) | l == l' && i == j && y /= z -> Nothing
                                      | otherwise -> Just (q' `Then` p')
      where p' = unsafeCoerce p
            q' = unsafeCoerce q
