module Parser(program) where

import qualified Data.Map                               as Map
import qualified ISA
import           Text.Parsec.Expr
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as Token
import           Text.ParserCombinators.Parsec.Token    (comma, lexeme)

lookupR :: Eq b => b -> Map.Map c b -> c
lookupR v = fst . head . Map.assocs . Map.filter (== v)

languageDef :: Token.LanguageDef a
languageDef = emptyDef  {Token.commentLine = "#"}

lexer = Token.makeTokenParser languageDef

whiteSpace = Token.whiteSpace lexer


identifier = Token.identifier lexer -- parses an identifier


mathOp = string "add" <|> string "sub" <|> string "mul" <|> string "divs"
mathIOp = string "addI" <|> string "subI" <|> string "mulI" <|> string "divI"
branchOp = try (string "je") <|> try (string "jne") <|> try (string "jl") <|> try (string "jg")
registerMemoryOp = string "lwm" <|> string "swm"
memoryMemoryOp = string "lwi" <|> string "swo"
jumpOp = string "jump"
pseudoBranchOp = try (string "jel") <|> try (string "jnel") <|> try (string "jll") <|> try (string "jgl")
pseudoJumpOp = string "jumpl"

int = fromInteger <$> Token.integer lexer

program :: Parser [ISA.Instruction]
program = many1 isa

isa :: Parser ISA.Instruction
isa =
    try labelDef <|>
    try mathI <|>
    try registerMemory <|>
    try memoryMemory <|>
    try math <|>
    try branch <|>
    try jump <|>
    try pseudoBranch <|>
    try pseudoJump <|>
    try ret <|>
    try halt <|>
    try call <|>
    try labelCall

halt :: Parser ISA.Instruction
halt = do
    whiteSpace
    string "halt"
    whiteSpace
    return ISA.Halt
ret :: Parser ISA.Instruction
ret = do
    whiteSpace
    string "ret"
    whiteSpace
    return ISA.ret

call :: Parser ISA.Instruction
call = do
    whiteSpace
    string "call"
    whiteSpace
    rd <- identifier
    whiteSpace
    imm <- int
    whiteSpace
    let rd' = ISA.toReg rd
    return $ ISA.PseudoCall rd' imm

labelCall :: Parser ISA.Instruction
labelCall = do
    whiteSpace
    string "call"
    whiteSpace
    labelName <- identifier
    whiteSpace
    return $ ISA.PseudoLabelCall labelName

labelDef :: Parser ISA.Instruction
labelDef = do
    whiteSpace
    name <- identifier
    char ':'
    whiteSpace
    return $ ISA.Label name

math :: Parser ISA.Instruction
math = do
    whiteSpace
    name <- mathOp
    whiteSpace
    rd <- identifier
    whiteSpace
    rs1 <- identifier
    whiteSpace
    rs2 <- identifier
    whiteSpace
    let rd' = ISA.toReg rd
    let rs1' = ISA.toReg rs1
    let rs2' = ISA.toReg rs2
    return $ op' name rd' rs1' rs2'
    where
        op' "add" = ISA.add
        op' "sub" = ISA.sub
        op' "mul" = ISA.mul
        op' "div" = ISA.div
        op' s = error $ "error: expected add or sub or mul or div, but got " ++ s

mathI :: Parser ISA.Instruction
mathI = do
    whiteSpace
    name <- mathIOp
    whiteSpace
    rd <- identifier
    whiteSpace
    rs1 <- identifier
    whiteSpace
    imm <- int
    whiteSpace
    let rd' = ISA.toReg rd
    let rs1' = ISA.toReg rs1
    return $ op' name rd' rs1' imm
    where
        op' "addI" = ISA.addI
        op' "subI" = ISA.subI
        op' "mulI" = ISA.mulI
        op' "divI" = ISA.divI
        op' s = error $ "error: expected addI or subI or mulI or divI, but got " ++ s

branch :: Parser ISA.Instruction
branch = do
    whiteSpace
    name <- branchOp
    whiteSpace
    rs1 <- identifier
    whiteSpace
    rs2 <- identifier
    whiteSpace
    imm <- int
    whiteSpace
    let rs1' = ISA.toReg rs1
    let rs2' = ISA.toReg rs2
    return $ op' name rs1' rs2' imm
    where
        op' "je" = ISA.je
        op' "jne" = ISA.jne
        op' "jg" = ISA.jg
        op' "jl" = ISA.jl
        op' s = error $ "error: expected je or jne or jg or jl, but got " ++ s

registerMemory :: Parser ISA.Instruction
registerMemory = do
    whiteSpace
    name <- registerMemoryOp
    whiteSpace
    rd <- identifier
    whiteSpace
    rs1 <- identifier
    whiteSpace
    imm <- int
    whiteSpace
    let rd' = ISA.toReg rd
    let rs1' = ISA.toReg rs1
    return $ op' name rd' rs1' imm
    where
        op' "lwm" = ISA.lwm
        op' "swm" = ISA.swm
        op' s     = error $ "error: expected lwm or swm, but got " ++ s

memoryMemory :: Parser ISA.Instruction
memoryMemory = do
    whiteSpace
    name <- memoryMemoryOp
    whiteSpace
    rd <- identifier
    whiteSpace
    rs1 <- identifier
    whiteSpace
    imm <- int
    whiteSpace
    let rd' = ISA.toReg rd
    let rs1' = ISA.toReg rs1
    return $ op' name rd' rs1' imm
    where
        op' "lwi" = ISA.lwi
        op' "swo" = ISA.swo
        op' s     = error $ "error: lwi or swo, but got " ++ s

jump :: Parser ISA.Instruction
jump = do
    whiteSpace
    name <- jumpOp
    whiteSpace
    rd <- identifier
    whiteSpace
    imm <- int
    whiteSpace
    let rd' = ISA.toReg rd
    return $ op' name rd' imm
    where
        op' "jump" = ISA.jmp
        op' s      = error $ "error: lwi or swo, but got " ++ s

pseudoBranch :: Parser ISA.Instruction
pseudoBranch = do
    whiteSpace
    name <- pseudoBranchOp
    whiteSpace
    rs1 <- identifier
    whiteSpace
    rs2 <- identifier
    whiteSpace
    label <- identifier
    whiteSpace
    let rs1' = ISA.toReg rs1
    let rs2' = ISA.toReg rs2
    return $ op' name rs1' rs2' label
    where
        op' "jel" = ISA.jel
        op' "jnel" = ISA.jnel
        op' "jgl" = ISA.jgl
        op' "jll" = ISA.jll
        op' s = error $ "error: expected jel or jnel or jgl or jll, but got " ++ s

pseudoJump :: Parser ISA.Instruction
pseudoJump = do
    whiteSpace
    name <- pseudoJumpOp
    whiteSpace
    label <- identifier
    whiteSpace
    return $ op' name label
    where
        op' "jumpl" = ISA.jmpl
        op' s       = error $ "error: lwi or swo, but got " ++ s
