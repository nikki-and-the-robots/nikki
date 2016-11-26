
import           Data.Indexable.Tests
import           Physics.Chipmunk.Types.Tests
import           Sorts.Terminal.Tests
import           Top.Main.Tests
import           Utils.Tests


main :: IO ()
main = do
    Top.Main.Tests.tests
    Data.Indexable.Tests.tests
    Sorts.Terminal.Tests.tests
    Utils.Tests.tests
    Physics.Chipmunk.Types.Tests.tests
