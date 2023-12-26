module ISA where

type Name = String

data Register =
    Zero Name |
    Argument Name |
    Saved Name |
    Temprorary Name |
    Special Name
    deriving(Show, Ord, Eq)

zero :: Register
zero = Zero "x0"
ra :: Register
ra = Special "ra"
pc :: Register
pc = Special "pc"
sp :: Register
sp = Special "sp"
a0 :: Register
a0 = Argument "a0"
a1 :: Register
a1 = Argument "a1"
a2 :: Register
a2 = Argument "a2"
s0 :: Register
s0 = Saved "s0"
s1 :: Register
s1 = Saved "s1"
s2 :: Register
s2 = Saved "s2"
t0 :: Register
t0 = Saved "t0"
t1 :: Register
t1 = Saved "t1"
t2 :: Register
t2 = Saved "t2"

registers :: [Register]
registers = [zero, ra, pc, sp, a0, a1, a2, s0, s1, s2, t0, t1, t2]


type Opcode = Int
type Rd = Register
type Rs1 = Register
type Rs2 = Register
type Imm = Int

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
    Nop
    deriving(Show)

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
jmp = Jump 8

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
push rs = [addI sp sp 4, swm sp rs 0]
pop :: Register -> [Instruction]
pop rd = [lwm rd sp 0, subI sp sp 4]
ret :: [Instruction]
ret =  jmp ra 0: pop pc  -- pc <- ra + 4, pop ra

type Offset = Int
call :: Register -> Offset -> [Instruction]
call rd imm = add ra pc zero : push ra ++ [jmp rd imm]
