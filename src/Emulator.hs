module Emulator where
import Data.Map as Map
import ISA

MAX_DMEM = 4096
MAX_IMEM = 4096
MAX_INMEM = 16
MAX_OUTMEM = 16

data CPU = CPU{
    registers :: Map Register Int,
    instructionMemory :: Map Instruction Int,
    dataMemory :: Map Int Int,
    inMemory :: Map Int Int,
    outMemory :: Map Int Int
    } deriving(Show)

initDefault :: CPU
initDefault = 
    let
        regs = Map.fromList [("zero", 0), ("ra", 0), ("a0", 0), ("a1", 0), ("t0", 0), ("t1", 0), ("t2", 0), ("s0", 0), ("s1", 0), ("s2", 0), ("pc", 0)]
        iMem = replicate MAX_IMEM 0
        dMem = replicate MAX_DMEM 0
        inMem = replicate  MAX_INMEM 0
        outMem = replicate MAX_OUTMEM 0
    in CPU regs iMem dMem inMem outMem

getPC :: CPU -> Register
getPC CPU{regs} = regs ! "pc"

emulate :: CPU -> IO ()
emulate cpu = do
    pc <- regs ! pc
    instruction <- iMem ! pc
    result <- execute cpu instruction
    case result of
        Left err -> err
        Right cpu' -> do
            cpu'' <- postExecute cpu'
            show cpu''
            emulate cpu''

postExecute :: CPU -> CPU
postExecute cpu@(CPU regs iMem dMem inMem outMem) =
    let
        pc = getPC cpu
        regs' = Map.insert "pc" (pc + 4) regs
    in
        CPU regs' iMem dMem inMem outMem
    
execute :: CPU -> Instruction -> Either String CPU
execute (CPU regs iMem dMem inMem outMem) operator@(MathOp rd rs1 rs2 _ opcode) = do
    funct <- case operator of
        Add {}-> Right (+)
        Sub {} -> Right (-)
        Mul {} -> Right (*)
        Div {} -> Right (/)
        _ -> Left $ "Error: No math operation with opcode " ++ show opcode
    case funct of 
        Left err -> Left err
        Right op -> do
            alu <- op <$> findKey rs1 regs <*> findKey rs2 regs
            case alu of
                Nothing -> Left "Error: Invalid register"
                Just value -> do
                    regs' <- Map.insert rd alu regs
                    Right $ CPU regs' iMem dMem inMem outMem

execute cpu@(CPU regs iMem dMem inMem outMem) branch@(Branch rs1 rs2 imm opcode) = do
    funct <- case branch of
        JE {} -> Right (==)
        JNE {} -> Right (=/)
        JG {} -> Right (>)
        JL {} -> Right (<)
        _ -> Left $ "Error: No branch instruction with opcode " ++ opcode
    case funct of
        Left err -> Left err
        Right op -> do
            alu <- op <$> findKey rs1 regs <*> findKey rs2 regs
            case alu of
                Nothing -> Left: "Error: invalid register"
                Just False -> Right cpu
                Just True -> do
                    pc <- getPC cpu
                    regs' <- Map.insert "pc" (pc + imm) regs
                    Right $ CPU regs' iMem dMem inMem outMem

execute cpu@(CPU regs iMem dMem inMem outMem) oparator@(MathImmideate rd rs1 imm opcode) = do
    funct <- case operator of
        AddI {}-> Right (+)
        SubI {} -> Right (-)
        MulI {} -> Right (*)
        DivI {} -> Right (/)
        _ -> Left $ "Error: No math operation with opcode " ++ show opcode
    case funct of 
        Left err -> Left err
        Right op -> do
            alu <- op <$> findKey rs1 regs <*> imm
            case alu of
                Nothing -> Left "Error: Invalid register"
                Just value -> do
                    regs' <- Map.insert rd alu regs
                    Right $ CPU regs' iMem dMem inMem outMem

execute cpu@(CPU regs iMem dMem inMem outMem) operation@(RegisterMemory rd rs1 imm opcode) = do
    alu <- (+) <$> findKey rs1 regs <*> imm
    case alu of
        Nothing -> Left "Error: Invalid register"
        Just addr -> do 
            case operation of
                LWM {} -> do
                    dValue <- dMem ! addr
                    regs' <- Map.insert rd dValue regs
                    Right $ CPU regs' iMem dMem inMem outMem
                SWM {} -> do
                    rValue <- regs ! rd 
                    dMem' <- Map.insert addr rValue dMem
                    Right $ CPU regs iMem dMem' inMem outMem  
        
execute cpu@(CPU regs iMem dMem inMem outMem) operation@(MemoryMemory rd rs1 imm opcode) = do
    rAddr <- regs ! rd
    alu <- (+) <$> findKey rs1 regs <*> imm
    case alu of
        Nothing -> Left "Error: Invalid register"
        Just addr -> do
            case operation of
                LWI {} -> do
                    inValue <- inMem ! addr
                    dMem' <- Map.insert rAddr inValue dMem
                    Right $ CPU regs iMem dMem' inMem outMem  
                SWO {} -> do
                    outValue <- dMem ! addr
                    outMem' <- Map.insert rAddr outValue outMem
                    Right $ CPU regs iMem dMem inMem outMem'