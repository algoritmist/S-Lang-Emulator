module TranslatorLib where
import qualified LanguageParser (program)
import qualified Translator     (translate)

program = LanguageParser.program
tranlsate = Translator.translate
