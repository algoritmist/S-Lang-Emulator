module EmulatorLib
    ( program, convert
    ) where
import qualified Converter
import qualified Emulator
import qualified Parser

program = Parser.program
convert = Converter.toReal

emulate = Emulator.emulate

