
module Physics.Chipmunk.Tests where


import Control.Applicative

import Physics.Chipmunk

import Test.QuickCheck


instance Arbitrary Vector where
    arbitrary = Vector <$> arbitrary <*> arbitrary

