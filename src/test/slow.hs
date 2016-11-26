
import           Physics.Chipmunk.StickyEdges.Tests
import           Sorts.Tiles.Baking.Tests

main :: IO ()
main = do
  Physics.Chipmunk.StickyEdges.Tests.tests
  Sorts.Tiles.Baking.Tests.tests
