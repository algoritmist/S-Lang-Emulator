{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections  #-}

module Emulator where
import           Data.Map (Map, fromList, insert, (!))
import           ISA

maxDMem = 4096
maxIMem = 4096
maxInMem = 16
maxOutMem = 16

invalidRegister = Left "Error: invalid register"

data CPU = CPU{
    regs   :: Map Register Int,
    iMem   :: Map Int Instruction,
    dMem   :: Map Int Int,
    inMem  :: Map Int Int,
    outMem :: Map Int Int
    } deriving(Show)



initDefault :: CPU
initDefault =
    let
        generator = map (, 0) (iterate (+ 1) 0)
        regs = fromList $ map (, 0) registers
        iMem = fromList $ take maxIMem $  map (, Nop) (iterate (+ 1) 0)
        dMem = fromList  $ take maxDMem generator
        inMem = fromList $ take maxInMem generator
        outMem = fromList $ take maxOutMem generator
    in CPU regs iMem dMem inMem outMem

initWithInMem :: [Int] -> CPU
initWithInMem inMem = initDefault{inMem = fromList $ zip (iterate (+1) 0) inMem}


emulate :: CPU -> IO ()
emulate cpu@CPU{regs, iMem} = do
    let pc = regs ! ISA.pc
    let instruction = iMem ! pc
    let result = execute cpu instruction
    case result of
        Left err -> fail err
        Right cpu' -> do
            let cpu'' = postExecute cpu'
            print cpu''
            emulate cpu''

postExecute :: CPU -> CPU
postExecute (CPU regs iMem dMem inMem outMem) =
    let
        pc = regs ! ISA.pc
        regs' = insert ISA.pc (pc + 4) regs
    in
        CPU regs' iMem dMem inMem outMem

execute :: CPU -> Instruction -> Either String CPU
execute (CPU regs iMem dMem inMem outMem) (MathOp opcode rd rs1 rs2 _) = do
    op <- case opcode of
        0 -> Right (+)
        1 -> Right (-)
        2 -> Right (*)
        3 -> Right Prelude.div
        _ -> Left $ "Error: No math operation with opcode " ++ show opcode
    let alu = (regs ! rs1) `op` (regs ! rs2)
    let regs' = insert rd alu regs
    Right $ CPU regs' iMem dMem inMem outMem

execute (CPU regs iMem dMem inMem outMem) (Branch opcode _ rs1 rs2 imm) = do
    op <- case opcode of
        4 -> Right (==)
        5 -> Right (/=)
        6 -> Right (>)
        7 -> Right (<)
        _ -> Left $ "Error: No branch instruction with opcode " ++ show opcode
    let alu = (regs ! rs1) `op` (regs ! rs2)
    let pc = regs ! ISA.pc
    let regs' = insert ISA.pc (pc + imm) regs
    Right $ CPU regs' iMem dMem inMem outMem

execute (CPU regs iMem dMem inMem outMem) (MathImmideate opcode rd rs1 imm) = do
    op <- case opcode of
        16 -> Right (+)
        17 -> Right (-)
        18 -> Right (*)
        19 -> Right Prelude.div
        _  -> Left $ "Error: No math operation with opcode " ++ show opcode

    let alu = (regs ! rs1) `op` imm
    let regs' = insert rd alu regs
    Right $ CPU regs' iMem dMem inMem outMem

execute (CPU regs iMem dMem inMem outMem) (RegisterMemory opcode rd rs1 imm) = do
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

execute (CPU regs iMem dMem inMem outMem) (MemoryMemory opcode rd rs1 imm) = do
    let rAddr = regs ! rd
    let addr = regs ! rs1 + imm
    case opcode of
        22 -> do
            let inValue = inMem ! addr
            let dMem' = insert rAddr inValue dMem
            Right $ CPU regs iMem dMem' inMem outMem
        23 -> do
            let outValue = dMem ! addr
            let outMem' = insert rAddr outValue outMem
            Right $ CPU regs iMem dMem inMem outMem'
        _ -> Left $ "Invalid operation with opcode" ++ show opcode
