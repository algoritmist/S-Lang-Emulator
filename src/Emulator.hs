module Emulator where
import           Data.Map as Map
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
        generator = Prelude.map (, 0) (iterate (+ 1) 0)
        regs = Map.fromList $ Prelude.map (, 0) registers
        iMem = Map.fromList $ Prelude.take maxIMem $  Prelude.map (, Nop) (iterate (+ 1) 0)
        dMem = Map.fromList  $ Prelude.take maxDMem generator
        inMem = Map.fromList $ Prelude.take maxInMem generator
        outMem = Map.fromList $ Prelude.take maxOutMem generator
    in CPU regs iMem dMem inMem outMem

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
postExecute cpu@(CPU regs iMem dMem inMem outMem) =
    let
        pc = regs ! ISA.pc
        regs' = Map.insert ISA.pc (pc + 4) regs
    in
        CPU regs' iMem dMem inMem outMem

execute :: CPU -> Instruction -> Either String CPU
execute (CPU regs iMem dMem inMem outMem) operator@(MathOp opcode rd rs1 rs2 _) = do
    op <- case opcode of
        0 -> Right (+)
        1 -> Right (-)
        2 -> Right (*)
        3 -> Right Prelude.div
        _ -> Left $ "Error: No math operation with opcode " ++ show opcode
    let alu = (regs ! rs1) `op` (regs ! rs2)
    let regs' = Map.insert rd alu regs
    Right $ CPU regs' iMem dMem inMem outMem

execute cpu@(CPU regs iMem dMem inMem outMem) branch@(Branch opcode _ rs1 rs2 imm) = do
    op <- case opcode of
        4 -> Right (==)
        5 -> Right (/=)
        6 -> Right (>)
        7 -> Right (<)
        _ -> Left $ "Error: No branch instruction with opcode " ++ show opcode
    let alu = (regs ! rs1) `op` (regs ! rs2)
    let pc = regs ! ISA.pc
    let regs' = Map.insert ISA.pc (pc + imm) regs
    Right $ CPU regs' iMem dMem inMem outMem

execute cpu@(CPU regs iMem dMem inMem outMem) oparator@(MathImmideate opcode rd rs1 imm) = do
    op <- case opcode of
        16 -> Right (+)
        17 -> Right (-)
        18 -> Right (*)
        19 -> Right Prelude.div
        _  -> Left $ "Error: No math operation with opcode " ++ show opcode

    let alu = (regs ! rs1) `op` imm
    let regs' = Map.insert rd alu regs
    Right $ CPU regs' iMem dMem inMem outMem

execute cpu@(CPU regs iMem dMem inMem outMem) operation@(RegisterMemory opcode rd rs1 imm) = do
    let addr = regs ! rs1 + imm
    case opcode of
        20 -> do
            let dValue = dMem ! addr
            let regs' = Map.insert rd dValue regs
            Right $ CPU regs' iMem dMem inMem outMem
        21 -> do
            let rValue = regs ! rd
            let dMem' = Map.insert addr rValue dMem
            Right $ CPU regs iMem dMem' inMem outMem
        _ -> Left $ "Invalid operation with opcode" ++ show opcode

execute cpu@(CPU regs iMem dMem inMem outMem) operation@(MemoryMemory opcode rd rs1 imm) = do
    let rAddr = regs ! rd
    let addr = regs ! rs1 + imm
    case opcode of
        22 -> do
            let inValue = inMem ! addr
            let dMem' = Map.insert rAddr inValue dMem
            Right $ CPU regs iMem dMem' inMem outMem
        23 -> do
            let outValue = dMem ! addr
            let outMem' = Map.insert rAddr outValue outMem
            Right $ CPU regs iMem dMem inMem outMem'
        _ -> Left $ "Invalid operation with opcode" ++ show opcode
