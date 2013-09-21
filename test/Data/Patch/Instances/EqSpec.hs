{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Patch.Instances.EqSpec where

import Data.Maybe

import Data.Patch.Commute
import Data.Patch.Instances.Eq
import Data.Patch.Invert
import Test.Hspec

instance Show (Primitive a from to)
instance Show (Then p q from to)

instance Eq a => Eq (Primitive a from to) where
  (==) (Change x y) (Change x' y') = x == x' && y == y'

spec :: Spec
spec = do
  describe "invert" $ do
    it "should invert a change" $ do
      let c  = Change 'a' 'b'
      let c' = Change 'b' 'a'
      invert c `shouldBe` c'
  describe "commute" $ do
    it "should commute two commutable changes" $ do
      let c  = Change 'a' 'b'
      let c' = Change 'b' 'c'
      commute (Then c c') `shouldSatisfy` isJust
    it "should fail to commute two non-commutable changes" $ do
      let c  = Change 'a' 'b'
      let c' = Change 'c' 'd'
      commute (Then c c') `shouldSatisfy` isNothing
