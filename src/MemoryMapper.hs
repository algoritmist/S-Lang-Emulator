module MemoryMapper where
import ISA
import Language
import Data.Map(Map, fromList)

type VariableName = String

data Mapper = {generatedLabels :: Int, registerMap :: Map Register VariableName, dataMap :: Map [Int] Int}

genLabel :: Mapper -> (ISA.Label, Mapper)
genLabel mp =
    let
        labels = generatedLabels mp
    in
        (ISA.Label $ "_l" ++ show labels, mp{generatedLabels=labels + 1})

mapArgs :: Mapper -> [Language.Expr] -> Mapper
mapArgs mp exps =
    if length exprs <= length ISA.argumentRegisters
        then fromList $ zip(ISA.argumentRegisters, map Language.getVariableName exprs)
    else error $ "Functions can take at most " ++ length ISA.argumentRegisters ++ " arguments"