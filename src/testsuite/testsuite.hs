

import Test.QuickCheck

import Utils

import Physics.Chipmunk.StickyEdges
import Physics.Chipmunk.StickyEdges.Tests

main = quickCheck suite

suite =
    Utils.tests .&.
    Physics.Chipmunk.StickyEdges.tests .&.
    Physics.Chipmunk.StickyEdges.Tests.tests
