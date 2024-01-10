module Lib
    ( program, convert
    ) where
import qualified Converter
import qualified Parser

program = Parser.program
convert result = case result of
    Left err   -> Left err
    Right prog -> Right $ Converter.toReal prog

