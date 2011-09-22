

import Data.Indexable.Tests

import Utils.Tests

import Physics.Chipmunk.Types.Tests
import Physics.Chipmunk.StickyEdges.Tests

import Sorts.Terminal.Tests

import Top.Main.Tests


main :: IO ()
main = do
    Top.Main.Tests.tests
    Data.Indexable.Tests.tests
    Sorts.Terminal.Tests.tests
    Utils.Tests.tests
    Physics.Chipmunk.Types.Tests.tests
    Physics.Chipmunk.StickyEdges.Tests.tests

i :: Monad m => a -> m ()
i = const $ return ()
