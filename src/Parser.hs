module Parser(program) where

import qualified Data.Map                               as Map
import qualified ISA
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as Token

languageDef :: Token.LanguageDef a
languageDef = emptyDef  {Token.commentLine = "#"}

lexer = Token.makeTokenParser languageDef

whiteSpace = Token.whiteSpace lexer


identifier = Token.identifier lexer -- parses an identifier


mathOp = string "add" <|> string "sub" <|> try (string "mul") <|> try (string "mod") <|> string "div"
mathIOp = string "addI" <|> string "subI" <|> try (string "mulI") <|> try (string "modI") <|> string "divI"
branchOp = try (string "be") <|> try (string "bne") <|> try (string "bl") <|> try (string "bg")
registerMemoryOp = string "lwm" <|> string "swm"
memoryMemoryOp = string "lwi" <|> string "swo"
jumpOp = string "jump"
pseudoBranchOp = try (string "bel") <|> try (string "bnel") <|> try (string "bll") <|> try (string "bgl")
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
    return ISA.Ret

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
        op' "mod" = ISA.mod
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
        op' "modI" = ISA.modI
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
        op' "be" = ISA.be
        op' "bne" = ISA.bne
        op' "bg" = ISA.bg
        op' "bl" = ISA.bl
        op' s = error $ "error: expected be or bne or bg or bl, but got " ++ s

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
    rs1 <- identifier
    whiteSpace
    imm <- int
    whiteSpace
    let rs1' = ISA.toReg rs1
    return $ op' name rs1' imm
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
        op' "bel" = ISA.bel
        op' "bnel" = ISA.bnel
        op' "bgl" = ISA.bgl
        op' "bll" = ISA.bll
        op' s = error $ "error: expected bel or bnel or bgl or bll, but got " ++ s

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
