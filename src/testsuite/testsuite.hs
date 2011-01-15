

import Utils.Tests

import Physics.Chipmunk.Types.Tests
import Physics.Chipmunk.StickyEdges.Tests


main :: IO ()
main = do
    Utils.Tests.tests
    Physics.Chipmunk.Types.Tests.tests
    Physics.Chipmunk.StickyEdges.Tests.tests

i = const $ return ()
