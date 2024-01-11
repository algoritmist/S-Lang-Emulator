module SlangLib where
import qualified LanguageParser (program)
import qualified Translator     (translate)

program = LanguageParser.program
tranlsate = Translator.translate
