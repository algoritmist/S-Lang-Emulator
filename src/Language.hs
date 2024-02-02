module Language where

import qualified Data.Map                               as Map
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as Token

data Expr
  = EVar String
  | EInt Int
  | EString String
  | EUnOp UnaryOperation Expr
  | EBinOp BinaryOperation Expr Expr
  | EFunCall String [Expr]
  | ELet -- let expressions,
      [VariableDefinition] -- list of bound variables,
      Expr -- returned expression
  | EIf Expr Alter
  | EList [Int]
  | EDefinition Function
  deriving (Show)

type CoreExpr = Expr
type SplitExpr = Expr

type Alter = (Expr, Expr) -- (tag, bound variables list?, expression)

type CoreAlter = Alter

type Program = [Expr]
type CoreProgram = Program
type CoreDefinition = Expr
type Variable = String

data Function = Function {name :: String, args :: [Variable], expr :: Expr} deriving (Show)
type CoreFunction = Function
type CoreFunctionName = String

data VariableDefinition = VariableDefinition {varName :: String, varExpr :: Expr} deriving(Show)

data UnaryOperation = Not | Neg deriving (Show, Eq)

unaryOperations :: Map.Map String UnaryOperation
unaryOperations = Map.fromList [("!", Not), ("-", Neg)]

isUnaryBoolean :: UnaryOperation -> Bool
isUnaryBoolean = (== Not)

data BinaryOperation
  = Eq
  | G
  | L
  | NotE
  | Sum
  | Sub
  | Mul
  | Div
  | Mod
  deriving (Show, Eq)

binaryOperations :: Map.Map String BinaryOperation
binaryOperations =
  Map.fromList
    [ ("==", Eq),
      (">", G),
      ("<", L),
      ("!=", NotE),
      ("+", Sum),
      ("-", Sub),
      ("*", Mul),
      ("/", Div),
      ("%", Mod)
    ]

isBinaryBoolean :: BinaryOperation -> Bool
isBinaryBoolean op = op /= Sum && op /= Sub && op /= Mul && op /= Div

languageDef :: LanguageDef a
languageDef =
  emptyDef
    { Token.commentStart = "{-",
      Token.commentEnd = "-}",
      Token.commentLine = "--",
      Token.identStart = letter,
      Token.identLetter = alphaNum,
      Token.reservedNames = ["If", "Then", "Else", "Let", "In"],
      Token.reservedOpNames = Map.keys binaryOperations ++ Map.keys unaryOperations
    }

getVariableName :: Expr -> String
getVariableName (EVar x) = x
getVariableName _        = error "Expression not a variable"
