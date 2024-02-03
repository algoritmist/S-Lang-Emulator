{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TupleSections         #-}


module Translator(translate) where
import           Data.Char (ord)
import           Data.List (sortOn)
import           Data.Map  (Map, assocs, elems, empty, fromList, insert, keys)
import           Data.Map  as Map (lookup)
import           ISA
import           Language

data MemoryMapper =
    MemoryMapper
    {
        variableMap     :: Map Variable Register,
        dataMap         :: Map [Int] Int,
        drPtr           :: Int,
        varCount        :: Int,
        generatedLabels :: Int,
        usedSaved       :: [Register]
    }

maxArgsError :: Int -> String
maxArgsError args = "Max number of supported arguments: " ++ show args
undefinedVariable :: Variable -> String
undefinedVariable var = "No such variable with name " ++ show var

initDefault :: MemoryMapper
initDefault = MemoryMapper empty empty 0 0 0 []

saveCaller :: MemoryMapper -> [ISA.Instruction]
saveCaller MemoryMapper{variableMap} =
    concatMap ISA.push $ filter (\r -> rType r /= Saved && rType r /= Hardwired && rType r /= Special) $ elems variableMap

popCaller :: MemoryMapper -> [ISA.Instruction]
popCaller MemoryMapper{variableMap} =
    concatMap ISA.pop $ reverse $ filter (\r -> rType r /= Saved && rType r /= Hardwired && rType r /= Special) $ elems variableMap

moveArgs :: MemoryMapper -> [Variable] -> [ISA.Instruction]
moveArgs mp vars =
    if length vars <= length ISA.argumentRegisters
        then zipWith (\x y -> ISA.add x y zero) ISA.argumentRegisters (map (getReg mp) vars)
        else error $ maxArgsError $ length ISA.argumentRegisters

moveA0 :: MemoryMapper -> (MemoryMapper, [ISA.Instruction])
moveA0 mp =
    let
        vars = map fst $ filter (\x -> snd x == a0) (assocs $ variableMap mp)
        (mp', var) = newVariable mp
        reg = getReg mp' var
        funct m (v, r) = assocVariable m v r
        mp'' = foldl funct mp $ map (,reg) vars
    in
        (mp'', [ISA.add reg a0 zero])

newVariable :: MemoryMapper -> (MemoryMapper, Variable)
newVariable mp =
    let
        var = "_v" ++ show(varCount mp)
    in (allocVariable mp{varCount = varCount mp + 1} var, var)

newLabel :: MemoryMapper -> (MemoryMapper, LabelName)
newLabel mp = (mp{generatedLabels = generatedLabels mp + 1}, "_l" ++ show (generatedLabels mp))

assocVariable :: MemoryMapper -> Variable -> Register -> MemoryMapper
assocVariable mp var reg = mp{variableMap = insert var reg (variableMap mp)}

getReg :: MemoryMapper -> Variable -> Register
getReg MemoryMapper{variableMap} var = case Map.lookup var variableMap of
    Just reg -> reg
    _        -> error $ undefinedVariable var

allocVariable :: MemoryMapper -> Variable -> MemoryMapper
allocVariable mp var = allocVariables mp [var]

allocVariables :: MemoryMapper -> [Variable] -> MemoryMapper
allocVariables mp [] = mp
allocVariables mp vars =
    let
        usedRegs = elems $ variableMap mp
        regs = take (length vars) $ sortOn rType $ filter (\r -> notElem r usedRegs && rType r /= Hardwired && rType r /= Special) ISA.registers
        mp' = mp{usedSaved = usedSaved mp ++ filter (\r -> rType r == Saved) regs}
        funct m (v, r) = assocVariable m v r
    in
        foldl funct mp' $ zip vars regs

putList :: MemoryMapper -> [Int] -> (MemoryMapper, Int)
putList mp lst =
    let
        dMap = dataMap mp
        drValue = drPtr mp
    in
        case Map.lookup lst dMap of
            Just addr -> (mp, addr)
            _ -> (mp{dataMap = insert lst drValue dMap, drPtr = drValue + length lst}, drValue)

emptyMapping :: MemoryMapper -> MemoryMapper
emptyMapping mp = mp{variableMap = empty}

mapArgs :: MemoryMapper -> [Variable] -> MemoryMapper
mapArgs mp args =
    let
        vMap = fromList $ zip args ISA.argumentRegisters
    in
        mp{variableMap = vMap}

translateArgs :: MemoryMapper -> [Expr] -> (MemoryMapper, [Variable], [ISA.Instruction])
translateArgs mp [] = (mp, [], [])
translateArgs mp (expr : exprs) =
    let
        (mp', var, i1s) = translateHelper mp expr
        (mp'', vars, i2s) = translateArgs mp' exprs
    in
        (mp'', var : vars, i1s ++ i2s)

containsVar :: MemoryMapper -> Variable -> Bool
containsVar MemoryMapper{variableMap} var = elem var $ keys variableMap


getVar :: MemoryMapper -> Register -> Variable
getVar MemoryMapper{variableMap} reg = head (map fst $ filter (\x -> snd x == reg) (assocs variableMap))

translate :: Language.Program -> ([ISA.Instruction], [Int])
translate program =
    let
        (mp, instructions) = translateProgram initDefault program
        instructions' = [ISA.PseudoLabelCall "main", ISA.Halt] ++ instructions
    in
        (instructions', concatMap fst $ sortOn snd (assocs $ dataMap mp))

translateProgram :: MemoryMapper -> Language.Program -> (MemoryMapper, [ISA.Instruction])
translateProgram mp [] = (mp, [])
translateProgram mp (line:program) =
    let
        (mp', _, i1s) = translateHelper mp line
        (mp'', i2s) = translateProgram mp' program
    in
        (mp'', i1s ++ i2s)

translateHelper :: MemoryMapper -> Language.Expr -> (MemoryMapper, Variable, [ISA.Instruction])

translateHelper mp (EDefinition Language.Function{name, args, expr}) =
    let
        mp' = emptyMapping mp
        label = ISA.Label name
        mp'' = mapArgs mp' args
        (mp''', var, instructions) = translateHelper mp'' expr
        pushSaved = concatMap ISA.push $ usedSaved mp'''
        popSaved = concatMap ISA.pop $ reverse $ usedSaved mp'''
        reg = getReg mp''' var
        mp4 = emptyMapping mp'''
        mp5 = assocVariable mp4 var a0
    in
        (
            mp5{usedSaved = []},
            var,
            label : pushSaved ++ instructions ++ popSaved ++ [ISA.add a0 reg zero, ISA.Ret]
        )

translateHelper mp (EFunCall name args) =
    let
        (mp', vars, transArgs) = translateArgs mp args
        (mp'',  mvA0) = moveA0 mp'
        push = saveCaller mp''
        move = moveArgs mp'' vars
        call = ISA.PseudoLabelCall name
        pop = popCaller mp''
        (mp''', var) = newVariable mp''
        mp4 = assocVariable mp''' var a0
    in
        (mp4, var, transArgs ++ mvA0 ++ push ++ move ++ call : pop)

translateHelper mp (EBinOp op e1 e2) =
    let
        (mp', v1, i1s) = translateHelper mp e1
        (mp'', v2, i2s) = translateHelper mp' e2
        (mpVar, var) = newVariable mp''
        rd = getReg mpVar var
        rs1 = getReg mpVar v1
        rs2 = getReg mpVar v2
        instr = case op of
            Language.Sum -> ISA.add rd rs1 rs2
            Language.Sub -> ISA.sub rd rs1 rs2
            Language.Mul -> ISA.mul rd rs1 rs2
            Language.Div -> ISA.div rd rs1 rs2
            Language.Mod -> ISA.mod rd rs1 rs2
            _            -> error $ "only arithmetical operations are supported, not " ++ show op
    in
        (mpVar, var, i1s ++ i2s ++ [instr])

translateHelper mp (EIf op'@(EBinOp op e1 e2) (eTrue, eFalse)) =
    let
        (mp', v1, i1s) = translateHelper mp e1
        (mp'', v2, i2s) = translateHelper mp' e2
        (mp''', trueLabel) = newLabel mp''
        (mp4, finalLabel) = newLabel mp'''
        (mp5, finalVar) = newVariable mp4
        r1 = getReg mp5 v1
        r2 = getReg mp5 v2
        instr = case op of
            Eq   -> ISA.jel r1 r2 trueLabel
            G    -> ISA.jgl r1 r2 trueLabel
            L    -> ISA.jll r1 r2 trueLabel
            NotE -> ISA.jnel r1 r2 trueLabel
            _ -> error $ "Translator: expected boolean expression but got: " ++ show op'

        rd = getReg mp5 finalVar
        (mpFalse, vF, ifs) = translateHelper mp5 (EFunCall "id" [eFalse])
        rF = getReg mpFalse vF

        ifs' = ifs ++ [ISA.add rd rF zero, ISA.jmpl finalLabel]

        (mpTrue, vT, its) = translateHelper mpFalse{variableMap = variableMap mp5} (EFunCall "id" [eTrue])
        rT = getReg mpTrue vT
        its' = its ++ [ISA.add rd rT zero, ISA.Label finalLabel]
    in
        (mpTrue, finalVar, i1s ++ i2s ++ instr : ifs' ++ Label trueLabel : its')

translateHelper _ (EIf expr _) = error $ "Translator: expected boolean expression but got: " ++ show expr

translateHelper mp (EUnOp op expr) =
    let
        (mp', var, i1s) = translateHelper mp expr
        reg = getReg mp' var
        i2s = case op of
            Neg -> [ISA.sub reg zero reg]
            _   -> error "other unary operators not supported yet"
    in
        (mp', var, i1s ++ i2s)

translateHelper mp (EVar name) =
    if containsVar mp name then (mp, name, []) else error $ "No variable with name " ++ name

translateHelper mp (EInt x) =
    let
        (mp', var) = newVariable mp
        reg = getReg mp' var
    in
        (mp', var, [ISA.addI reg zero x])

translateHelper mp (EList lst) =
    let
        lst' = length lst : lst
        (mp', var) = newVariable mp
        reg = getReg mp' var
        (mp'', addr) = putList mp' lst'
    in
        (mp'', var, [ISA.addI reg zero addr])

translateHelper mp (EString s) = translateHelper mp $ EList (map ord s)

translateHelper mp (ELet [] retExpr) = translateHelper mp retExpr
translateHelper mp (ELet (VariableDefinition{varName, varExpr} : defs) retExpr) =
    let
        (mp', var, i1s) = translateHelper mp varExpr
        rs = getReg mp' var
        mp'' = assocVariable mp' varName rs
        (mp''', var', i2s) = translateHelper mp'' (ELet defs retExpr)
        reg = getReg mp''' var'
    in
        (mp'''{variableMap = insert var' reg (variableMap mp)}, var', i1s ++ i2s)
