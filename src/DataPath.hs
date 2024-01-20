{-# LANGUAGE NamedFieldPuns #-}
module DataPath where
import ISA(Instruction, Opcode, Register, Instruction(MathOp, MathImmideate, RegisterMemory, MemoryMemory, Jump, Halt, Branch, Nop), gpRegs, zero)
import Data.Map(Map, (!), fromList, insert, empty)
import qualified Data.Map as Map(lookup)
import Data.Bits(shiftR)
import Data.List(intercalate)


wordSize = 4
shft = 2
maxOffset = 4096

data InstructionMemory = 
    InstructionMemory 
    {
        addr :: Int,
        iStorage :: Map Int Instruction
    }

initInstructionMemory instrs =
    InstructionMemory
    {
        iStorage = fromList $ zip [0..] instrs
    }

rd0 :: InstructionMemory -> Either String Instruction
rd0 InstructionMemory{addr, iStorage} = case Map.lookup addr iStorage of
    Just v -> Right v
    Nothing -> Left "Error: end of instruction memory reached!"

data Decoder = 
    Decoder 
    {
        instruction :: Instruction,
        opcode :: Opcode,
        rs1 :: Register,
        rs2 :: Register,
        rd :: Register,
        immI :: Int,
        immB :: Int,
        immJ :: Int,
        rOp :: Int -> Int -> Int
    }

initDecoder =
    Decoder
    {
        instruction = Nop,
        opcode = (-1),
        rs1 = zero,
        rs2 = zero,
        rd = zero,
        immI = 0,
        immB = 0,
        immJ = 0,
        rOp = (+)
    }

decode :: Instruction -> ControlUnit -> Decoder -> (ControlUnit, Decoder)
decode instr@(MathOp op rd rs1 rs2 _) cu decoder = 
    let
        cu' = cu{sigPC = True, sigWE1 = True, sigWE2 = False, sigWE3 = False, sigRE = False, sigSndSrc = False, sigAmSrc = False, sigHalt = False}
        decoder' = decoder{instruction = instr, opcode = op, rs1 = rs1, rs2 = rs2, rd = rd, immI = 0}
    in
        (cu', decoder')
decode instr@(MathImmideate op rd rs1 imm) cu decoder = 
    let
        cu' = ControlUnit{sigPC = True, sigWE1 = True, sigWE2 = False, sigWE3 = False, sigRE = False, sigSndSrc = True, sigAmSrc = False, sigHalt = False}
        decoder' = decoder{instruction = instr, opcode = op, rs1 = rs1, rd = rd, immI = imm}
    in
        (cu', decoder')

decode instr@(RegisterMemory 20 rd rs1 imm) cu decoder =
    let
        cu' = cu{sigPC = True, sigWE1 = True, sigWE2 = False, sigWE3 = False, sigRE = False, sigSndSrc = True, sigAmSrc = True}
        decoder' = decoder{instruction = instr, opcode = 20, rs1 = rs1, rd = rd, immI = imm}
    in
        (cu', decoder')

decode instr@(RegisterMemory 21 rd rs1 imm) cu decoder =
    let
        cu' = cu{sigPC = True, sigWE1 = False, sigWE2 = True, sigWE3 = False, sigRE = False, sigSndSrc = True}
        decoder' = decoder{instruction = instr, opcode = 21, rs1 = rs1, rd = rd, immI = imm}
    in
        (cu', decoder')

decode instr@(MemoryMemory 22 _ rs1 imm) cu decoder =
    let
        cu' = cu{sigPC = True, sigWE1 = False, sigWE2 = True, sigWE3 = False, sigRE = True, sigSndSrc = True}
        decoder'= decoder{instruction = instr, opcode = 22, rs1 = rs1, immI = imm}
    in
        (cu', decoder')

decode instr@(MemoryMemory 23 _ rs1 imm) cu decoder =
    let
        cu' = cu{sigPC = True, sigWE1 = False, sigWE2 = False, sigWE3 = True, sigRE = False, sigSndSrc = True}
        decoder' = decoder{instruction = instr, opcode = 23, rs1 = rs1, immI = imm}
    in
        (cu', decoder')

decode instr@(Branch op _ rs1 rs2 imm) cu decoder = 
    let
        cu' = cu{sigWE1 = False, sigWE2 = False, sigWE3 = False, sigRE = False, sigJB = True}
        decoder' = decoder{instruction = instr, opcode = op, rs1 = rs1, rs2 = rs2, immB = imm}
    in
        (cu', decoder')

decode instr@(Jump op rd imm) cu decoder =
    let
        cu' = cu{sigPC = False, sigWE1 = False, sigWE2 = False, sigWE3 = False, sigRE = False, sigJB = False}
        decoder'= decoder{instruction = instr, opcode = op, rd = rd, immJ = imm}
    in
        (cu', decoder')

decode instr@Halt cu decoder =
    let cu' = cu{sigHalt = True}
    in
        (cu', decoder)

decode instr@Nop cu decoder =
    let
        cu' = cu{sigPC = True, sigWE1 = False, sigWE2 = False, sigWE3 = False, sigRE = False, sigJB = False}
    in
        (cu', decoder)

setBranchSignal :: Decoder -> ALU -> ControlUnit -> ControlUnit
setBranchSignal decoder alu cu =
    let
        op = opcode decoder
        z = aluZero alu
        n = aluNegative alu
        sig = case op of
            4 -> z
            5 -> not z
            6 -> not z && not n
            7 -> n
            _ -> False
    in
        cu{sigPC = not sig}

data RegisterFile = 
    RegisterFile 
    {
        a1 :: Register,
        a2 :: Register,
        a3 :: Register,
        we1 :: Bool,
        wr :: Int,
        rStorage :: Map Register Int,
        aluOp :: Int -> Int -> Int
    }

initRegisterFile =
    RegisterFile
    {
        a1 = zero,
        a2 = zero,
        a3 = zero,
        we1 = False,
        wr = 0,
        rStorage = fromList (zip ISA.gpRegs (repeat 0)),
        aluOp = (+)
    }

registerAccessError reg = "Error: accessing non-general purpose register " ++ show reg ++ " not allowed"

rd1 :: RegisterFile -> Either String Int
rd1 RegisterFile{a1, rStorage} = case Map.lookup a1 rStorage of
    Just v -> Right v
    Nothing -> Left $ registerAccessError a1
rd2 :: RegisterFile -> Either String Int
rd2 RegisterFile{a2, rStorage} = case Map.lookup a2 rStorage of
    Just v -> Right v
    Nothing -> Left $ registerAccessError a2
rd3 :: RegisterFile -> Either String Int
rd3 RegisterFile{a3, rStorage} = case Map.lookup a3 rStorage of
    Just v -> Right v
    Nothing -> Left $ registerAccessError a3

writeRegister :: RegisterFile -> RegisterFile
writeRegister rf@RegisterFile{wr, a3, rStorage, we1} = if we1 && a3 /= zero then rf{rStorage = insert a3 wr rStorage} else rf

data DataMemory = 
    DataMemory
    {
        wData :: Int,
        offset :: Int,
        we2 :: Bool,
        dStorage :: Map Int Int
    }

initDataMemory dt =
    DataMemory
    {
        dStorage = fromList $ zip [0..] dt
    }

rd4 :: DataMemory -> Either String Int
rd4 DataMemory{offset, dStorage} = case Map.lookup offset dStorage of
    Just v -> Right v
    Nothing -> 
        if offset < maxOffset
            then Right 0
            else Left $ "Error: acessing memory at unknown address " ++ show offset

writeData :: DataMemory -> DataMemory
writeData dm@DataMemory{wData, offset, we2, dStorage} = if we2 then dm{dStorage = insert offset wData dStorage} else dm

data IODevice = 
    IODevice
    {
        mData :: Int,
        re :: Bool,
        we3 :: Bool,
        inPtr :: Int,
        outPtr :: Int,
        inStorage :: Map Int Int,
        outStorage :: Map Int Int
    }

initIODevice :: [Int] -> IODevice
initIODevice im = 
    IODevice
    {
        mData = 0,
        re = False,
        we3 = False,
        inPtr = 0,
        outPtr = 0,
        inStorage = fromList $ zip [0..] im,
        outStorage = empty
    }

instance Show IODevice where
    show IODevice{inPtr, outPtr} = "rin : " ++ show inPtr ++ ", rout: " ++ show outPtr

rd5 :: IODevice -> Either String Int
rd5 IODevice{inStorage, inPtr} = case Map.lookup (shift3 inPtr) inStorage of
    Just v -> Right v
    Nothing -> Left "IODevice: Input EOF reached"

readIn :: IODevice -> IODevice
readIn dev@IODevice{re, inPtr} = if re then dev{inPtr = inPtr + wordSize} else dev

writeOut :: IODevice -> IODevice
writeOut dev@IODevice{outStorage, outPtr, mData, we3} = if we3 then dev{outStorage = insert mData (shift3 outPtr) outStorage, outPtr = outPtr + wordSize} else dev

data ALU = ALU 
    {
        srcA :: Int,
        srcB :: Int,
        op :: Int -> Int -> Int
    }

aluOut :: ALU -> Int
aluOut ALU{srcA, srcB, op} = srcA `op` srcB

aluZero :: ALU -> Bool
aluZero alu = aluOut alu == 0

aluNegative :: ALU -> Bool
aluNegative alu = aluOut alu < 0

data Multiplexor = 
    Multiplexor
    {
        sel :: Bool,
        sig0 :: Int,
        sig1 :: Int
    }

outSig :: Multiplexor -> Int
outSig mux = if sel mux then sig1 mux else sig0 mux

data ControlUnit = 
    ControlUnit
    {
        sigJB :: Bool,
        sigPC :: Bool,
        sigWE1 :: Bool,
        sigWE2 :: Bool,
        sigWE3 :: Bool,
        sigRE :: Bool,
        sigSndSrc :: Bool,
        sigAmSrc :: Bool,
        sigRdSrc :: Bool,
        sigHalt :: Bool
    }

initControlUnit =
    ControlUnit
    {
        sigJB = False,
        sigPC = False,
        sigWE1 = False,
        sigWE2 = False,
        sigWE3 = False,
        sigRE = False,
        sigSndSrc = False,
        sigAmSrc = False,
        sigRdSrc = False,
        sigHalt = False
    }

data DataPath = 
    DataPath 
    {
        pc :: Int,
        instrMem :: InstructionMemory,
        regFile :: RegisterFile,
        dMem :: DataMemory,
        decoder :: Decoder,
        ioDev :: IODevice,
        mainAlu :: ALU,
        pcAlu :: ALU,
        brAlu :: ALU,
        jmpAlu :: ALU,
        jbMux :: Multiplexor,
        pcMux :: Multiplexor,
        sndMux :: Multiplexor,
        amMux :: Multiplexor,
        rdMux :: Multiplexor
    }

initDataPath :: [Instruction] -> [Int] -> [Int] -> DataPath
initDataPath instrs dm im = 
    DataPath 
    {
        pc = 0,
        instrMem = initInstructionMemory instrs,
        regFile = initRegisterFile,
        dMem = initDataMemory dm,
        decoder = initDecoder,
        ioDev = initIODevice im,
        mainAlu = ALU{srcA = 0, srcB = 0, op = (+)},
        pcAlu = ALU{srcA = 0, srcB = wordSize, op = (+)},
        brAlu = ALU{srcA = 0, srcB = 0, op = (+)},
        jmpAlu = ALU{srcA = 0, srcB = 0, op = (+)},
        jbMux = Multiplexor{sel = False, sig0 = 0, sig1 = 0},
        pcMux = Multiplexor{sel = False, sig0 = 0, sig1 = 0},
        sndMux = Multiplexor{sel = False, sig0 = 0, sig1 = 0},
        amMux = Multiplexor{sel = False, sig0 = 0, sig1 = 0},
        rdMux = Multiplexor{sel = False, sig0 = 0, sig1 = 0}
    }

instance Show DataPath where
    show DataPath{pc, regFile, dMem, ioDev} = intercalate  ", " ["pc:", show pc, show (rStorage regFile), show (dStorage dMem), show ioDev]

shift3 :: Int -> Int
shift3 x = x `shiftR` shft

instructionFetch :: (ControlUnit, DataPath) -> Either String (ControlUnit, DataPath)
instructionFetch (cu, dp@DataPath{pc, instrMem, pcAlu}) = 
    let
        pcAlu' = pcAlu {srcA = pc}
        instrMem' = instrMem{addr = shift3 pc}
    in
        Right (cu, dp{instrMem = instrMem',pcAlu = pcAlu'})

instructionDecode :: (ControlUnit, DataPath) -> Either String (ControlUnit, DataPath)
instructionDecode (cu, dp@DataPath{regFile, instrMem, brAlu, pcAlu, decoder}) = do
    instr <- rd0 instrMem
    let (cu', decoder') = decode instr cu decoder
    let brAlu' = brAlu {srcA = immB decoder', srcB = aluOut pcAlu}
    let regFile' = regFile{a1 = rs1 decoder', a2 = rs2 decoder', a3 = rd decoder', we1 = sigWE1 cu', aluOp = rOp decoder'}
    case sigHalt cu' of 
        False -> Right (cu', dp{regFile = regFile', brAlu = brAlu', decoder = decoder'})
        True -> Left "Halt: Stopping execution"

execute :: (ControlUnit, DataPath) -> Either String (ControlUnit, DataPath)
execute (cu, dp@DataPath{regFile, decoder, ioDev, brAlu, jmpAlu, pcAlu}) = do
    rd2Value <- rd2 regFile
    let sndMux' = Multiplexor{sel = sigSndSrc cu, sig0 = rd2Value, sig1 = immI decoder}
    rd1Value <- rd1 regFile
    let mainAlu' = ALU{srcA = rd1Value, srcB = outSig sndMux', op = aluOp regFile}
    let cu' = setBranchSignal decoder mainAlu' cu
    rd3Value <- rd3 regFile
    let jmpAlu' = jmpAlu{srcA = immJ decoder, srcB = rd3Value}
    let jbMux' =  Multiplexor{sel = sigJB cu', sig0 = aluOut jmpAlu', sig1 = aluOut brAlu}
    let pcMux' = Multiplexor{sel = sigPC cu', sig0 = outSig jbMux', sig1 = aluOut pcAlu}
    let pc' = outSig pcMux'
    rd3Value <- rd3 regFile
    rd5Value <- rd5 ioDev
    let rdMux' = Multiplexor{sel = sigRdSrc cu', sig0 = rd3Value, sig1 = rd5Value}
    Right (cu', dp{sndMux = sndMux', pc = pc', pcMux = pcMux', mainAlu = mainAlu', rdMux = rdMux'})

accessMemory :: (ControlUnit, DataPath) -> Either String (ControlUnit, DataPath)
accessMemory (cu, dp@DataPath{dMem, mainAlu, rdMux, ioDev}) = do
    let dMem' = writeData dMem{offset = aluOut mainAlu, wData = outSig rdMux, we2 = sigWE2 cu}
    rd4Value <- rd4 dMem'
    let ioDev' = readIn.writeOut $ ioDev{mData = rd4Value, re = sigRE cu, we3 = sigWE3 cu}
    Right (cu, dp{dMem = dMem', ioDev = ioDev'})

writeBack :: (ControlUnit, DataPath) -> Either String (ControlUnit, DataPath)
writeBack (cu, dp@DataPath{dMem, regFile, mainAlu}) = do
    rd4Value <- rd4 dMem
    let amMux' = Multiplexor{sel = sigAmSrc cu, sig0 = aluOut mainAlu, sig1 = rd4Value}
    let regFile' = writeRegister regFile{wr = outSig amMux'}
    Right (cu, dp{amMux = amMux', regFile = regFile'})

tick :: (ControlUnit, DataPath) -> Either String (ControlUnit, DataPath)
tick (cu, dp) = writeBack =<< accessMemory =<< execute =<< instructionDecode =<< instructionFetch (cu, dp)



ticks :: (ControlUnit, DataPath) -> ([DataPath], String)
ticks cd = case tick cd of
    Left err -> ([], err)
    Right (cu, dp) ->
        let
            (dp', msg) = ticks (cu, dp)
        in
            (dp : dp', msg)

simulate :: [Instruction] -> [Int] -> [Int] -> ([DataPath], String)
simulate instrs dm im =
    let
        cu = initControlUnit
        dp = initDataPath instrs dm im
    in
        ticks (cu, dp)