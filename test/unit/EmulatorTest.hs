import           Data.Map   ((!))
import           Emulator
import           ISA
import           Test.HUnit

cpu = Emulator.initDefault

testAddI = TestCase $ assertEqual "add t0 t0 42" (Just 42) $
    let
        result = execute cpu (addI t0 t0 42)
    in
        case result of
            Right cpu' -> Just $ regs cpu' ! t0
            Left err   -> Nothing

testMul = TestCase $ assertEqual "t1 <- 2, t2 <- 21, mul t0 t1 t2" (Just 42) $
    let
        result = do
            cpu' <- execute cpu (addI t1 t1 2)
            cpu'' <- execute cpu' (addI t2 t2 21)
            execute cpu'' (mul t0 t1 t2)
    in
        case result of
            Right cpu' -> Just $ regs cpu' ! t0
            Left err   -> Nothing

tests = TestList [TestLabel "Test AddI" testAddI, TestLabel "Test Mul" testMul]

main :: IO Counts
main = runTestTT tests
