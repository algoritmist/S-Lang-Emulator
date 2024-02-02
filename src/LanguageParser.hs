module LanguageParser(program) where

import qualified Data.Map                            as Map
import           Language
import           Text.Parsec.Expr
import           Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token

lookupR :: Eq b => b -> Map.Map c b -> c
lookupR v = fst . head . Map.assocs . Map.filter (== v)

lexer = Token.makeTokenParser languageDef

whiteSpace = Token.whiteSpace lexer

identifier = Token.identifier lexer -- parses an identifier

parens = Token.parens lexer -- parses surrounding parenthesis:

braces = Token.braces lexer

commaSep = Token.commaSep lexer

reservedOp = Token.reservedOp lexer


int :: Parser Int
int = fromInteger <$> Token.integer lexer

stringLiteral = Token.stringLiteral lexer -- parses a literal string

mapValueBetweenSpaces :: Eq a => Map.Map String a -> a -> Parser String
mapValueBetweenSpaces m v = try (whiteSpace *> string (lookupR v m) <* whiteSpace)

oneOfKeys :: Map.Map String a -> Parser a
oneOfKeys m = (Map.!) m <$> (choice . map string . Map.keys $ m)

unOp op = Prefix $ EUnOp op <$ mapValueBetweenSpaces unaryOperations op

binOp op = Infix (EBinOp op <$ mapValueBetweenSpaces binaryOperations op) AssocLeft


operations =
  [ [unOp Not, unOp Neg],
    [binOp Mul, binOp Div, binOp Mod],
    [binOp Eq, binOp NotE],
    [binOp Sum, binOp Sub],
    [binOp L, binOp G]
  ]

subExpression :: Parser CoreExpr
subExpression =
  parens expression
    <|> try ifExpression
    <|> try letExpression
    <|> try application
    <|> try (EVar <$> variable)
    <|> try primitive


expression :: Parser CoreExpr
expression = buildExpressionParser operations subExpression

definition :: Parser CoreExpr
definition = do
  whiteSpace
  name <- function
  whiteSpace
  char '('
  whiteSpace
  vars <- variable `sepBy` (whiteSpace *>  char ',' <* whiteSpace)
  whiteSpace
  char ')'
  whiteSpace
  char '='
  whiteSpace
  expr <- expression
  char ';'
  whiteSpace
  return $ EDefinition $ Function name vars expr

letExpression :: Parser CoreExpr
letExpression = do
  whiteSpace
  reservedOp "Let"
  whiteSpace
  exprs <- variableDefinition `sepBy` (whiteSpace *>  char ',' <* whiteSpace)
  whiteSpace
  reservedOp "In"
  whiteSpace
  var <- expression
  whiteSpace
  return (ELet exprs var)

ifExpression :: Parser CoreExpr
ifExpression = do
  whiteSpace
  reservedOp "If"
  whiteSpace
  condition <- expression
  whiteSpace
  reservedOp "Then"
  whiteSpace
  trueBranch <- expression
  whiteSpace
  reservedOp "Else"
  whiteSpace
  falseBranch <- expression
  whiteSpace
  return $ EIf condition (trueBranch, falseBranch)

function :: Parser String
function = variable

variable :: Parser Variable
variable = identifier

variableList :: Parser [Variable]
variableList = many1 variable

applicationList :: Parser [CoreExpr]
applicationList = expression `sepBy` (whiteSpace *>  char ',' <* whiteSpace)

variableDefinition :: Parser VariableDefinition
variableDefinition = do
  whiteSpace
  name <- function
  whiteSpace
  char '='
  whiteSpace
  expr <- expression
  whiteSpace
  return $ VariableDefinition name expr

application :: Parser CoreExpr
application = do
  whiteSpace
  f <- function
  whiteSpace
  char '('
  whiteSpace
  vars <- applicationList
  whiteSpace
  char ')'
  whiteSpace
  return $ EFunCall f vars

program :: Parser CoreProgram
program = many1 $ whiteSpace *> definition <* whiteSpace

parseString :: Parser String
parseString = do
  whiteSpace
  char '"'
  s <- identifier
  char '"'
  whiteSpace
  return s

primitive :: Parser CoreExpr
primitive = (EString <$> stringLiteral) <|> (EInt <$> int)
