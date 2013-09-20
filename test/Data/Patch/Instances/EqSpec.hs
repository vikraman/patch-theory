{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Patch.Instances.EqSpec where

import Data.Patch.Commute
import Data.Patch.Instances.Eq
import Data.Patch.Invert
import Test.Hspec

instance Show a => Show (Primitive a from to)

instance (Show (p from mid), Show (q mid to)) => Show (Then p q from to)

instance Eq a => Eq (Primitive a from to) where
  (==) (Change x y) (Change x' y') = x == x' && y == y'

-- instance (Eq (p from mid), Eq (q mid to)) => Eq (Then p q from to) where
--   (==) (Then p q) (Then p' q') = p == p' && q == q'

spec :: Spec
spec = do
  describe "invert" $ do
    it "should invert a change" $ do
      let c  = Change 'a' 'b'
      let c' = Change 'b' 'a'
      invert c `shouldBe` c'
  -- describe "commute" $ do
  --   it "should commute two commutable changes" $ do
  --     let c  = Change 'a' 'b'
  --     let c' = Change 'b' 'c'
  --     commute (Then c c') `shouldBe` (Just $ Then c' c)
  --   it "should fail to commute two non-commutable changes" $ do
  --     let c  = Change 'a' 'b'
  --     let c' = Change 'c' 'd'
  --     commute (Then c c') `shouldBe` Nothing
