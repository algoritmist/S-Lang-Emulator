import qualified EmulatorTest
import qualified StandardLibraryTest
import           Test.HUnit

main:: IO Counts
main = runTestTT.TestList $ EmulatorTest.tests ++ StandardLibraryTest.tests
