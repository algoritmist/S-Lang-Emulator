module Emulator where
import ISA

data CPU = {
    registers :: Map Register    Int,
    instructionMemory :: [Instruction],
    dataMemory :: [Int],
    inMemory :: [Int],
    outMemory :: [Int]
    } deriving(Show)

initDefault :: Int -> Int -> CPU
initDefault inM = 
    let
        regs = take 32 $ repeat 0
        iMem = take 4096 $ repeat 0
        dMem = take 4096 $ repeat 0
        outMem = take 16 $ repeat 0
    in CPU regs iMem dMem inMem outMem

execute :: CPU -> IO ()
execute Ret = debug "Execution finished"
execute cpu = do
    pc <- tail $ regs
    instruction <- iMem!!pc
    fail $ "Error: pc out of bounds"
    return $ sequece executeHelper
    alu <- pure (+) <*> findKey rs1 regs <*> findKey rs2 regs
    fail $ format "Error: Line {0}, invalid register format" [pc / 4]
    regs' <- Map.insert rd alu regs
    regs'' <- Map.insert pc (pc + 4) regs'
    CPU' <- CPU regs'' iMem dMem inMem outMem
    return $ sequece (show CPU : execute CPU')

executeHelper :: CPU -> Instruction -> Either fail CPU
executeHelper (CPU regs iMem dMem inMem outMem) operator@(MathOp rd rs1 rs2 opcode) = do
    funct <- case operator of
        Add _ _ _ _ -> Right (+)
        Sub _ _ _ _ -> Right (-)
        Mul _ _ _ _ -> Right (*)
        Div _ _ _ _ -> Right (/)
        otherwise -> Left (fail $ "Invalid operation with opcode " ++ show opcode)
    case funct of 
        (Left err) -> return err
        (Right )