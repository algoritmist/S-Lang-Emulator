module ISA where

type Register = Int
zero = "x0"
ra = "x1"
pc = "x31"

type Offset = Int

{-
I-type: imm - 12 bit signed, rs2 - 5 bit, rs1 - 5 bit, rd - 5 bit, opcode - 5 bit
II-tpye: imm - 17 bit signed, rs1 - 5 bit, rd - 5 bit, opcode - 5 bit
III-type: imm - 27 bit signed, opcode - 5 bit
-}
data Instruction =
    MathOp    {imm :: Int, rs2 :: Register, rs1 :: Register, rd :: Register, opcode :: Int} |
    Branch    {imm :: Int, rs2 :: Register, rs1 :: Register, rd :: Register, opcode :: Int} |
    RegisterMemory {imm :: Int, rs1 :: Register, rd :: Register, opcode :: Int} |
    MemoryMemory {imm :: Int, rs1 :: Register, rd :: Register, opcode :: Int} |
    MathImmideate {imm :: Int, rs1 :: Register, rd :: Register, opcode :: Int} |
    Jump {imm :: Int, opcode :: Int}

valid :: Instruction -> Bool
valid (T1 imm rs2 rs1 rd opcode) =
    (-2^11 <= imm && imm < 2^11) && (0 <= rs2 && rs2 <= 31) && (0 <= rs1 && rs1 <= 31) &&
    (0 <= rd && rd < opcode) && (0 <= opcode && opcode < 2^4)

valid (T2 imm rs2 rs1 opcode) =
    (-2^16 <= imm && imm < 2^16) && (0 <= rs2 && rs2 <= 31) && (0 <= rs1 && rs1 <= 31) &&
    (2^4 <= opcode && opcode < 2^5 -2)

valid (T3 imm rs2 rs1 opcode) = (-2^26 <= imm && imm < 2^27) && (opcode == 2^5-1)

-- R-R instructions
type Add rd rs1 rs2 = MathOp 0 rd rs1 rs2 0
type Sub rd rs1 rs2 = MathOp 1 rd rs1 rs2 0
type Mul rd rs1 rs2 = MathOp 2 rs2 rs1 rd 0
type Div rd rs1 rs2 = MathOp 3 rs2 rs1 rd 0
-- Branch instructions
type JE rs1 rs2 imm = Branch 4 pc rs1 rs2 4 -- if(@rs1 == @rs2) pc <- pc + imm
type JG rs1 rs2 imm = Branch 5 pc rs1 rs2 imm
type JNE rs1 rs2 imm = Branch 6 pc rs1 rs2 imm
type JL rs1 rs2 imm = Branch 7 pc rs1 rs2 imm
type Ret = Branch 8 pc ra zero 0 -- pc <- ra + 4, pop ra
type Jmp imm = Jump imm 30

-- R-I instructions
type AddI rd rs1 imm = MathImmideate rd rs1 imm 16
type SubI rd rs1 imm = MathImmideate rd rs1 imm 17
type MulI rd rs1 imm = MathImmideate rd rs1 imm 18
type DivI rd rs1 imm = MathImmideate rd rs1 imm 19

-- R-M instructions
type LWM rd rs1 imm = RegisterMemory rd rs1 imm 20
type SWM rd rs1 imm = RegisterMemory rd rs1 imm 21

-- M-M instructions
type LWI rd rs1 imm = MemoryMemory rd rs1 imm 22
type SWO rd rs1 imm = MemoryMemory rd rs1 imm 23

-- Pseudo instructions
push :: Register -> [Instruction]
push rs = [Add pc pc 1, SWM pc rs 0]
pop :: Register -> [Instruction]
pop rd = [LWM rd pc 0, Sub pc pc 1]
call :: Offset -> [Instruction]
call imm = Add ra pc zero : push ra ++ [Jmp imm]
