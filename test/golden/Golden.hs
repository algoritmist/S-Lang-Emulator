import           System.FilePath   (replaceExtension, takeBaseName)
import           System.IO
import           Test.Tasty        (TestTree, defaultMain, testGroup)
import           Test.Tasty.Golden (findByExtension, goldenVsFileDiff)
import           Text.Parsec.Prim  (parse)

import           Data.Char         (chr, ord)
import           Data.Map          (elems)
import qualified Emulator
import qualified EmulatorLib
import qualified SlangLib

main :: IO ()
main = defaultMain =<< goldenTests

execute srcFile inFile outFile = do
  libContents <- readFile "src/Prelude.asm"
  case parse EmulatorLib.program "src/Prelude.asm" libContents of
    Left err -> print err
    Right libInstructions -> do
      srcContents <- readFile srcFile
      inContents <- readFile inFile
      let inMem = map ord inContents
      let result = parse SlangLib.program srcFile srcContents
      case result of
        Left err -> writeFile outFile $ show err
        Right program -> do
          let (instructions, dt) = SlangLib.tranlsate program
          let instructions' = instructions ++ libInstructions
          let realInstructions = EmulatorLib.convert instructions'
          let cpu = Emulator.setIstructionMem Emulator.initDefault realInstructions
          let cpu' = Emulator.setDataMem cpu dt
          let cpu'' = Emulator.setInMem cpu' inMem
          let (cpus, code) = Emulator.emulate cpu''
          let cpus' = take 1000 cpus
          let outCpus = concatMap (\x -> show x ++ "\n") cpus'
          let outMem = map chr $ elems.Emulator.outMem $ last cpus
          writeFile outFile $ outCpus ++ code ++ "\nOutput: " ++ outMem ++ "\n"

goldenTests :: IO TestTree
goldenTests = do
  slangFiles <- findByExtension [".sl"] "golden"
  return $ testGroup "Slang golden tests"
    [ goldenVsFileDiff
        (takeBaseName slangFile) -- test name
        (\ref new -> ["diff", "-u", ref, new]) -- diff
        cmpFile -- golden file path
        outFile
        (execute slangFile inFile outFile) -- action whose result is tested
    | slangFile <- slangFiles
    , let cmpFile = replaceExtension slangFile ".out"
    , let inFile = replaceExtension slangFile ".in"
    , let outFile = replaceExtension slangFile ".ret"
    ]
