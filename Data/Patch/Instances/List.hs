{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Patch.Instances.List where

import Unsafe.Coerce

import Data.Patch
import Data.Patch.Commute
import Data.Patch.Invert

data Primitive a from to where
  Change :: Eq a => Int -> a -> a -> Primitive a from to

instance Invert (Primitive a) where
  invert (Change i a b) = Change i b a

instance Commute (Primitive a) (Primitive a) where
  commute (p `Then` q) = case (p, q) of
    (Change i x y, Change j z w) | i == j && y /= z -> Nothing
                                 | otherwise -> Just (q' `Then` p')
      where p' = unsafeCoerce p
            q' = unsafeCoerce q
