{-# LANGUAGE NamedFieldPuns #-}

module Main where
import           Data.Map   (fromList, (!), elems, assocs)
import           DataPath
import           ISA
import           Test.HUnit

testAddI :: Test
testAddI = TestCase $ assertEqual "add t0 t0 42" (42) $
    let
        instrs = [addI t0 t0 42]
        dmem = []
        imem = [0]
        (dps, result) = simulate instrs dmem imem
        rf = regFile $ last dps
    in
        getRegisterValue t0 rf

testMul = TestCase $ assertEqual "t1 <- 2, t2 <- 21, mul t0 t1 t2" (42) $
    let
        instrs = [addI t1 t1 2, addI t2 t2 21, mul t0 t1 t2]
        dmem = []
        imem = [0]
        (dps, result) = simulate instrs dmem imem
        rf = regFile $ last dps
    in
        getRegisterValue t0 rf

testSWM = TestCase $ assertEqual "t1 <- 42, data[4] = @t1, @data[4]" (42) $
    let
        instrs = [addI t1 t1 42, swm t1 zero 4]
        dmem = []
        imem = [0]
        (dps, result) = simulate instrs dmem imem
        dmem' = dMem $ last dps
    in
        getData 4 dmem'

testLWM = TestCase $ assertEqual "data[4] = 42, t1 <- @data[4]" (42) $
    let
        instrs = [addI t2 t2 42, swm t2 zero 4, lwm t1 zero 4]
        dmem = []
        imem = [0]
        (dps, result) = simulate instrs dmem imem
        rf = regFile $ last dps
    in
        getRegisterValue t1 rf

testSWO = TestCase $ assertEqual "t1 <- 42, data[0] = @t1, out[0] = @data[0]" ([(0, 42), (1, 52), (2, 32)]) $
    let
        instrs = [swo zero 0, swo zero 1, swo zero 2]
        dmem = [42, 52, 32]
        imem = [0]
        (dps, result) = simulate instrs dmem imem
        dev = ioDev $ last dps
    in
        assocs $ outStorage dev

testLWI = TestCase $ assertEqual "data[] = 42, t1 <- @data[16]" [(0, 72), (1, 101), (2, 108)] $
    let
        instrs = [lwi zero 0, lwi zero 1, lwi zero 2]
        dmem = []
        imem = [72, 101, 108, 108, 111, 44, 32, 87, 111, 114, 108, 100, 33]
        (dps, result) = simulate instrs dmem imem
        dmem' = dStorage $ dMem $ last dps
    in
        assocs dmem'

testJump = TestCase $ assertEqual "t1 <- 4, jump t1 38, pc <- @t1 + 38" (42) $
    let
        instrs = [addI t1 t1 4, jmp t1 38]
        dmem = []
        imem = [0]
        (dps, result) = simulate instrs dmem imem
        pC = DataPath.pc $ last dps
    in
        pC

tests =
    TestList
    [
        TestLabel "Test AddI" testAddI,
        TestLabel "Test Mul" testMul,
        TestLabel "Test SWM" testSWM,
        TestLabel "Test LWM" testLWM,
        TestLabel "Test SWO" testSWO,
        TestLabel "Test LWI" testLWI,
        TestLabel "Test Jump" testJump
    ]

main:: IO Counts
main = runTestTT tests