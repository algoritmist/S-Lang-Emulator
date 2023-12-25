module ISA where

type Name = String

data Register =
    Zero Name |
    Argument Name |
    Saved Name |
    Temprorary Name |
    Special Name deriving(Show, Ord, Eq)

zero = Zero "x0"
ra = Special "ra"
pc = Special "pc"
sp = Special "sp"
a0 = Argument "a0"
a1 = Argument "a1"
a2 = Argument "a2"
s0 = Saved "s0"
s1 = Saved "s1"
s2 = Saved "s2"
t0 = Saved "t0"
t1 = Saved "t1"
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
add rd rs1 rs2 = MathOp 0 rd rs1 rs2 0
sub rd rs1 rs2 = MathOp 1 rd rs1 rs2 0
mul rd rs1 rs2 = MathOp 2 rd rs1 rs2 0
div rd rs1 rs2 = MathOp 3 rd rs1 rs2 0
-- Branch instructions
je = Branch 4 pc -- if(@rs1 == @rs2) pc <- pc + imm
jne = Branch 5 pc
jg = Branch 6 pc
jl = Branch 7 pc
jmp = Jump 8

-- R-I instructions
addI = MathImmideate 16
subI = MathImmideate 17
mulI = MathImmideate 18
divI = MathImmideate 19

-- R-M instructions
lwm = RegisterMemory 20
swm = RegisterMemory 21

-- M-M instructions
lwi = MemoryMemory 22
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
