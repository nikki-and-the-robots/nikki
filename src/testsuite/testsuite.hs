

import Data.Indexable.Tests

import Utils.Tests

import Physics.Chipmunk.Types.Tests
import Physics.Chipmunk.StickyEdges.Tests


main :: IO ()
main = do
    Data.Indexable.Tests.tests
    Utils.Tests.tests
    Physics.Chipmunk.Types.Tests.tests
    Physics.Chipmunk.StickyEdges.Tests.tests

i :: Monad m => a -> m ()
i = const $ return ()
