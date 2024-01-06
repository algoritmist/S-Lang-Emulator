{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module ISA where
import           Data.List (find)

type Name = String

data Register = Register{rType :: RegisterType, name :: Name} deriving(Ord, Eq)
data RegisterType = Special | Zero | Argument | Saved | Temporary deriving(Ord, Eq)


instance Show Register where
    show = name

zero :: Register
zero = Register Zero "zero"
ra :: Register
ra = Register Special "ra"
rin :: Register
rin = Register Special "rin"
dr :: Register
dr = Register Special "dr"
pc :: Register
pc = Register Special "pc"
sp :: Register
sp = Register Special "sp"
a0 :: Register
a0 = Register Argument "a0"
a1 :: Register
a1 = Register Argument "a1"
a2 :: Register
a2 = Register Argument "a2"
s0 :: Register
s0 = Register Saved "s0"
s1 :: Register
s1 = Register Saved "s1"
s2 :: Register
s2 = Register Saved "s2"
t0 :: Register
t0 = Register Temporary "t0"
t1 :: Register
t1 = Register Temporary "t1"
t2 :: Register
t2 = Register Temporary "t2"
t3 :: Register
t3 = Register Temporary "t3"
rout :: Register
rout = Register Special "rout"
tr :: Register
tr = Register Special "tr"
jp :: Register
jp = Register Special "jp"

registers :: [Register]
registers = [zero, ra, pc, sp, dr, rin, a0, a1, a2, s0, s1, s2, t0, t1, t2, t3, rout, tr, jp]

toReg :: String -> Register
toReg s = case find (\r -> name r == s) registers of
    Just reg -> reg
    _        -> error "Register does not exist"


type Opcode = Int
type Rd = Register
type Rs1 = Register
type Rs2 = Register
type Imm = Int
type LabelName = String

{-
I-type: imm - 12 bit signed, rs2 - 5 bit, rs1 - 5 bit, rd - 5 bit, opcode - 5 bit
II-tpye: imm - 17 bit signed, rs1 - 5 bit, rd - 5 bit, opcode - 5 bit
III-type: imm - 27 bit signed, opcode - 5 bit
-}

data Instruction =
    MathOp Opcode Rd Rs1 Rs2 Imm |
    Branch    Opcode Rd Rs1 Rs2 Imm |
    RegisterMemory Opcode Rd Rs1 Imm |
    MemoryMemory Opcode Rd Rs1 Imm |
    MathImmideate Opcode Rd Rs1 Imm |
    Jump Opcode Rd Imm |
    Nop |
    Halt |
    Label Name |
    PseudoBranch Opcode Rd Rs1 Rs2 LabelName |
    PseudoJump Opcode LabelName |
    PseudoCall Rd Imm |
    PseudoLabelCall LabelName
    deriving(Show, Eq)

-- R-R instructions
add :: Rd -> Rs1 -> Rs2 -> Instruction
add rd rs1 rs2 = MathOp 0 rd rs1 rs2 0
sub :: Rd -> Rs1 -> Rs2 -> Instruction
sub rd rs1 rs2 = MathOp 1 rd rs1 rs2 0
mul :: Rd -> Rs1 -> Rs2 -> Instruction
mul rd rs1 rs2 = MathOp 2 rd rs1 rs2 0
div :: Rd -> Rs1 -> Rs2 -> Instruction
div rd rs1 rs2 = MathOp 3 rd rs1 rs2 0
-- Branch instructions
je :: Rs1 -> Rs2 -> Imm -> Instruction
je = Branch 4 pc -- if(@rs1 == @rs2) pc <- pc + imm
jne :: Rs1 -> Rs2 -> Imm -> Instruction
jne = Branch 5 pc
jg :: Rs1 -> Rs2 -> Imm -> Instruction
jg = Branch 6 pc
jl :: Rs1 -> Rs2 -> Imm -> Instruction
jl = Branch 7 pc
jmp :: Rd -> Imm -> Instruction
jmp = Jump 31

-- R-I instructions
addI :: Rd -> Rs1 -> Imm -> Instruction
addI = MathImmideate 16
subI :: Rd -> Rs1 -> Imm -> Instruction
subI = MathImmideate 17
mulI :: Rd -> Rs1 -> Imm -> Instruction
mulI = MathImmideate 18
divI :: Rd -> Rs1 -> Imm -> Instruction
divI = MathImmideate 19

-- R-M instructions
lwm :: Rd -> Rs1 -> Imm -> Instruction
lwm = RegisterMemory 20
swm :: Rd -> Rs1 -> Imm -> Instruction
swm = RegisterMemory 21

-- M-M instructions
lwi :: Rd -> Rs1 -> Imm -> Instruction
lwi = MemoryMemory 22
swo :: Rd -> Rs1 -> Imm -> Instruction
swo = MemoryMemory 23

-- Pseudo instructions
push :: Register -> [Instruction]
push rs = [swm rs sp 0, subI sp sp 4]
pop :: Register -> [Instruction]
pop rd = [addI sp sp 4, lwm rd sp 0]
ret :: Instruction
ret = addI pc ra 4 -- pc <- ra, pop ra

type Offset = Int
call :: Register -> Offset -> [Instruction]
call rd imm =  push ra ++ [add ra pc zero, jmp rd (imm - 12)] ++ pop ra

-- Pseudo branch instructions
jel :: Rs1 -> Rs2 -> LabelName -> Instruction
jel = PseudoBranch 4 pc
jnel :: Rs1 -> Rs2 -> LabelName -> Instruction
jnel = PseudoBranch 5 pc
jgl :: Rs1 -> Rs2 -> LabelName -> Instruction
jgl = PseudoBranch 6 pc
jll :: Rs1 -> Rs2 -> LabelName -> Instruction
jll = PseudoBranch 7 pc
jmpl :: LabelName -> Instruction
jmpl = PseudoJump 31
