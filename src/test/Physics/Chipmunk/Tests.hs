
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Physics.Chipmunk.Tests where

import Physics.Chipmunk
import Test.QuickCheck

instance Arbitrary Vector where
    arbitrary = Vector <$> arbitrary <*> arbitrary
