import qualified EmulatorTest
import           Test.HUnit

main:: IO Counts
main = runTestTT.TestList $ EmulatorTest.tests
