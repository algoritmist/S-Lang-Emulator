import           Data.Map   ((!))
import           Emulator
import           ISA
import           Test.HUnit

cpu = Emulator.initDefault

testAddI :: Test
testAddI = TestCase $ assertEqual "add t0 t0 42" (Right 42) $
    let
        result = execute cpu (addI t0 t0 42)
    in
        case result of
            Right cpu' -> Right $ regs cpu' ! t0
            Left err   -> Left err

testMul = TestCase $ assertEqual "t1 <- 2, t2 <- 21, mul t0 t1 t2" (Right 42) $
    let
        result = do
            cpu' <- execute cpu (addI t1 t1 2)
            cpu'' <- execute cpu' (addI t2 t2 21)
            execute cpu'' (mul t0 t1 t2)
    in
        case result of
            Right cpu' -> Right $ regs cpu' ! t0
            Left err   -> Left err

testSWM = TestCase $ assertEqual "t1 <- 42, data[4] = @t1, @data[4]" (Right 42) $
    let
        result = do
            cpu' <- execute cpu (addI t1 t1 42)
            execute cpu' (swm t1 zero 4)
    in
        case result of
            Right cpu' -> Right $ dMem cpu' ! 4
            Left err   -> Left err

testLWM = TestCase $ assertEqual "data[4] = 42, t1 <- @data[4]" (Right 42) $
    let
        result = do
            cpu' <- execute cpu (addI t2 t2 42)
            cpu'' <- execute cpu' (swm t2 zero 4)
            execute cpu'' (lwm t1 zero 4)
    in
        case result of
            Right cpu' -> Right $ regs cpu' ! t1
            Left err   -> Left err

testSWO = TestCase $ assertEqual "data[4] = 42, out[4] = @data[4]" (Right 42) $
    let
        result = do
            cpu' <- execute cpu (addI t1 t1 42)
            cpu'' <- execute cpu' (addI t2 t2 4)
            cpu''' <- execute cpu'' (swm t1 t2 0)
            execute cpu''' (swo t2 zero 4)
    in
        case result of
            Right cpu' -> Right $ outMem cpu' ! 4
            Left err   -> Left err

testLWI = TestCase $ assertEqual "data[4] = 42, t1 <- @data[4]" (Right 42) $
    let
        cpu = Emulator.initWithInMem [-1, 2, 9, 128, 42, 15]
        result = do
            execute cpu (lwi zero zero 4)
    in
        case result of
            Right cpu' -> Right $ dMem cpu' ! 0
            Left err   -> Left err


tests = TestList [
    TestLabel "Test AddI" testAddI,
    TestLabel "Test Mul" testMul,
    TestLabel "Test SWM" testSWM,
    TestLabel "Test LWM" testLWM,
    TestLabel "Test SWO" testSWO,
    TestLabel "Test LWI" testLWI
    ]

main :: IO Counts
main = runTestTT tests
