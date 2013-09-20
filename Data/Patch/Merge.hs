{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Patch.Merge where

import Data.Patch.Commute
import Data.Patch.Invert

data Fork p q to1 to2 where
  Fork :: p from to1 -> q from to2 -> Fork p q to1 to2

class Merge p q where
  merge :: Fork p q mid1 mid2 -> Maybe (Then q p mid1 mid2)

instance (Invert p, Commute p q) => Merge p q where
  merge (Fork p q) = commute (invert p `Then` q)
