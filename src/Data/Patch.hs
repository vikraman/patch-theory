{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Patch where

import Data.Patch.Commute
import Data.Patch.Invert

data Patch prim from to where
  Primitive :: prim from to -> Patch prim from to

instance Invert prim => Invert (Patch prim) where
  invert (Primitive p) = Primitive (invert p)

instance Commute prim prim => Commute (Patch prim) (Patch prim) where
  commute (Primitive p `Then` Primitive q) = do
    (q' `Then` p') <- commute (p `Then` q)
    return (Primitive q' `Then` Primitive p')
