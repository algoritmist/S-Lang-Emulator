import           System.FilePath   (replaceExtension, takeBaseName)
import           System.IO
import           Test.Tasty        (TestTree, defaultMain, testGroup)
import           Test.Tasty.Golden (findByExtension, goldenVsFileDiff)
import           Text.Parsec.Prim  (parse)

import           Data.Char         (chr, ord)
import           Data.Map          (elems)
import qualified AsmLib
import qualified TranslatorLib
import EmulatorLib

main :: IO ()
main = defaultMain =<< goldenTests

execute srcFile inFile outFile = do
  libContents <- readFile "src/Prelude.asm"
  case parse AsmLib.program "src/Prelude.asm" libContents of
    Left err -> print err
    Right libInstructions -> do
      srcContents <- readFile srcFile
      inContents <- readFile inFile
      let src = "Source: |-\n" ++ srcContents ++ "\n"
      let inMem = map ord inContents
      let result = parse TranslatorLib.program srcFile srcContents
      case result of
        Left err -> writeFile outFile $ show err
        Right program -> do
          let (instructions, dt) = TranslatorLib.tranlsate program
          let instructions' = instructions ++ libInstructions
          let realInstructions = AsmLib.convert instructions'
          let (cpus, code) = simulate realInstructions dt inMem
          --writeFile outFile code
          let cpus' = take 100 cpus
          let outCpus = concatMap (\x -> show x ++ "\n") cpus'
          let outMem = map chr (elems $ getOutMem $ last cpus)
          let instructionsOut = "Instructions: |-\n" ++ concatMap (\x -> show x ++ "\n") instructions' ++ "\n"
          let stdin = "Stdin: |-\n" ++ inContents
          let instrTotal = "Total: |-\n" ++ show (length cpus) ++ " instructions executed"
          writeFile outFile $ foldl (++) []
            [
              src,
              instructionsOut,
              stdin,
              "\nTicks: |-\n",
              outCpus,
              "\nExit code: |-\n",
              code, "\nStdout: |-\n",
              outMem,
              "\n",
              instrTotal ,
              "\n"
            ]

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
