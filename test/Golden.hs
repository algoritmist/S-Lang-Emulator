import qualified Data.ByteString.Lazy      as LBS
import qualified Data.ByteString.Lazy.UTF8 as BLU
import           System.FilePath           (replaceExtension, takeBaseName)
import           Test.Tasty                (TestTree, defaultMain, testGroup)
import           Test.Tasty.Golden         (findByExtension, goldenVsString)
main :: IO ()
main = defaultMain =<< goldenTests

textToResult :: LBS.ByteString -> LBS.ByteString
textToResult _ = BLU.fromString "Translator not implemented yet"

goldenTests :: IO TestTree
goldenTests = do
  slangFiles <- findByExtension [".sl"] "test"
  return $ testGroup "Slang golden tests"
    [ goldenVsString
        (takeBaseName slangFile) -- test name
        resultFile -- golden file path
        (textToResult <$> LBS.readFile slangFile) -- action whose result is tested
    | slangFile <- slangFiles
    , let resultFile = replaceExtension slangFile ".out"
    ]
