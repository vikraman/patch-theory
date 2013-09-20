{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Patch.Instances.Eq where

import Unsafe.Coerce

import Data.Patch
import Data.Patch.Commute
import Data.Patch.Invert

data Primitive a from to where
  Change :: Eq a => a -> a -> Primitive a from to

instance Invert (Primitive a) where
  invert (Change a b) = Change b a

instance Commute (Primitive a) (Primitive a) where
  commute (p `Then` q) = case (p, q) of
    (Change x y, Change z w) | y /= z -> Nothing
                             | y == z -> Just (q' `Then` p')
      where p' = unsafeCoerce p
            q' = unsafeCoerce q
