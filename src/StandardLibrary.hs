module StandardLibrary where
import           ISA
input :: [Instruction]
input =
    [
        swm a0 dr 0,
        add a1 dr zero,
        je a0 zero 20,
        addI dr dr 4,
        lwi dr rin 0,
        addI rin rin 4,
        subI a0 a0 4,
        jmp zero (-24),
        add a0 a1 zero
    ] ++ [ret]
