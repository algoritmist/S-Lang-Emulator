{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections  #-}

module Emulator(CPU(regs, iMem, dMem, inMem, outMem), initDefault, setInMem, setDataMem, setIstructionMem, execute, emulate) where
import           Data.Map (Map, assocs, elems, empty, fromList, insert, size,
                           (!))
import           Data.Map as Map (lookup)
import qualified ISA

type ExitCode = String

maxDMem :: Int
maxDMem = 4096
maxIMem :: Int
maxIMem = 4096
maxInMem :: Int
maxInMem = 16
maxOutMem :: Int
maxOutMem = 16

data CPU = CPU{
    regs   :: Map ISA.Register Int,
    iMem   :: Map Int ISA.Instruction,
    dMem   :: Map Int Int,
    inMem  :: Map Int Int,
    outMem :: Map Int Int
    } deriving(Eq)

instance Show CPU where
    show CPU{regs, dMem} = "CPU{regs: " ++ show (assocs regs) ++ "}"

initDefault :: CPU
initDefault =
    let
        generator = map (, 0) (iterate (+ 4) 0)
        regs' = fromList $ map (, 0) ISA.registers
        regs = insert ISA.sp (maxDMem - 4) regs'
        iMem = empty
        dMem = empty
        inMem = empty
        outMem = empty
    in CPU regs iMem dMem inMem outMem

setInMem :: CPU -> [Int] -> CPU
setInMem cpu inMem = cpu{inMem = fromList $ zip (iterate (+ 4) 0)   inMem}

setIstructionMem :: CPU -> [ISA.Instruction] -> CPU
setIstructionMem cpu iMem = cpu{iMem = fromList $ zip (iterate (+ 4) 0)   iMem}

setDataMem :: CPU -> [Int] -> CPU
setDataMem cpu dataMem =
    let
        mem = fromList $ zip (iterate (+ 4) 0)   dataMem
        rs = regs cpu
    in
        cpu{dMem = mem, regs = insert ISA.dr (4 * length dataMem) rs}


emulate :: CPU -> ([CPU], ExitCode)
emulate cpu@CPU{regs, iMem} =
    let
        pc = regs ! ISA.pc
        instruction' = Map.lookup pc iMem
        result = case instruction' of
            Nothing          -> Left $ "no instruction at address " ++ show pc
            Just instruction -> execute cpu instruction
    in
        case result of
            Left err -> ([cpu], err)
            Right cpu' ->
                let
                    (cpus, result) = emulate $ postExecute cpu'
                in
                    (cpu : cpus, result)

postExecute :: CPU -> CPU
postExecute (CPU regs iMem dMem inMem outMem) =
    let
        pc = regs ! ISA.pc
        regs' = insert ISA.pc ((pc + 4) `mod` (4 * size iMem)) regs
    in
        CPU regs' iMem dMem inMem outMem

execute :: CPU -> ISA.Instruction -> Either String CPU
execute cpu ISA.Halt = Left "Halt: stopping execution"
execute cpu ISA.Nop = Right cpu
execute cpu (ISA.Jump _ rd imm) = do
    let alu = (regs cpu ! rd) + imm
    let regs' = insert ISA.pc (alu + regs cpu ! ISA.pc) $ regs cpu
    Right cpu{regs = regs'}

execute (CPU regs iMem dMem inMem outMem) (ISA.MathOp opcode rd rs1 rs2 _) = do
    op <- case opcode of
        0 -> Right (+)
        1 -> Right (-)
        2 -> Right (*)
        3 -> Right Prelude.div
        8 -> Right Prelude.mod
        _ -> Left $ "Error: No math operation with opcode " ++ show opcode
    let alu = (regs ! rs1) `op` (regs ! rs2)
    let regs' = insert rd alu regs
    Right $ CPU regs' iMem dMem inMem outMem

execute (CPU regs iMem dMem inMem outMem) (ISA.Branch opcode _ rs1 rs2 imm) = do
    op <- case opcode of
        4 -> Right (==)
        5 -> Right (/=)
        6 -> Right (>)
        7 -> Right (<)
        _ -> Left $ "Error: No branch instruction with opcode " ++ show opcode
    let pc = regs ! ISA.pc
    let regs' = if (regs ! rs1) `op` (regs ! rs2) then insert ISA.pc (pc + imm) regs else regs
    Right $ CPU regs' iMem dMem inMem outMem

execute (CPU regs iMem dMem inMem outMem) (ISA.MathImmideate opcode rd rs1 imm) = do
    op <- case opcode of
        16 -> Right (+)
        17 -> Right (-)
        18 -> Right (*)
        19 -> Right Prelude.div
        24 -> Right Prelude.mod
        _  -> Left $ "Error: No math operation with opcode " ++ show opcode

    let alu = (regs ! rs1) `op` imm
    let regs' = insert rd alu regs
    Right $ CPU regs' iMem dMem inMem outMem

execute (CPU regs iMem dMem inMem outMem) (ISA.RegisterMemory opcode rd rs1 imm) = do
    let addr = regs ! rs1 + imm
    case opcode of
        20 -> do
            let dValue = dMem ! addr
            let regs' = insert rd dValue regs
            Right $ CPU regs' iMem dMem inMem outMem
        21 -> do
            let rValue = regs ! rd
            let dMem' = insert addr rValue dMem
            Right $ CPU regs iMem dMem' inMem outMem
        _ -> Left $ "Invalid operation with opcode" ++ show opcode

execute (CPU regs iMem dMem inMem outMem) (ISA.MemoryMemory opcode rd rs1 imm) = do
    let rAddr = regs ! rd
    let addr = regs ! rs1 + imm
    case opcode of
        22 -> do
            let inValue = inMem ! addr
            let dMem' = insert rAddr inValue dMem
            Right $ CPU regs iMem dMem' inMem outMem
        23 -> do
            let outValue = dMem ! rAddr
            let outMem' = insert addr outValue outMem
            Right $ CPU regs iMem dMem inMem outMem'
        _ -> Left $ "Invalid operation with opcode" ++ show opcode

execute _ _ = Left "can't execute pseudo instructions"
