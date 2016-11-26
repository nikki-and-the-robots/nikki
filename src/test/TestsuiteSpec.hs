
module TestsuiteSpec where

import           Test.Hspec

import           Data.Indexable.Tests
import           Physics.Chipmunk.Types.Tests
import           Sorts.Terminal.Tests
import           Utils.Tests


spec :: Spec
spec = do
  it "quickcheck tests" $ do
    Data.Indexable.Tests.tests
    Sorts.Terminal.Tests.tests
    Utils.Tests.tests
    Physics.Chipmunk.Types.Tests.tests
