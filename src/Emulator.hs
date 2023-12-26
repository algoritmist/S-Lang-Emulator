{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections  #-}

module Emulator where
import           Data.Map (Map, fromList, insert, (!))
import qualified ISA

maxDMem :: Int
maxDMem = 4096
maxIMem :: Int
maxIMem = 4096
maxInMem :: Int
maxInMem = 16
maxOutMem :: Int
maxOutMem = 16

invalidRegister :: Either String b
invalidRegister = Left "Error: invalid register"

data CPU = CPU{
    regs   :: Map ISA.Register Int,
    iMem   :: Map Int ISA.Instruction,
    dMem   :: Map Int Int,
    inMem  :: Map Int Int,
    outMem :: Map Int Int
    } deriving(Show)



initDefault :: CPU
initDefault =
    let
        generator = map (, 0) (iterate (+ 1) 0)
        regs = fromList $ map (, 0) ISA.registers
        iMem = fromList $ take maxIMem $  map (, ISA.Nop) (iterate (+ 1) 0)
        dMem = fromList  $ take maxDMem generator
        inMem = fromList $ take maxInMem generator
        outMem = fromList $ take maxOutMem generator
    in CPU regs iMem dMem inMem outMem

initWithInMem :: [Int] -> CPU
initWithInMem inMem = initDefault{inMem = fromList $ zip (iterate (+1) 0)   inMem}


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

execute :: CPU -> ISA.Instruction -> Either String CPU
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
            let outValue = dMem ! addr
            let outMem' = insert rAddr outValue outMem
            Right $ CPU regs iMem dMem inMem outMem'
        _ -> Left $ "Invalid operation with opcode" ++ show opcode
