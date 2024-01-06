module Main (main) where

import           Emulator
import           Lib
import           System.Environment.Blank (getArgs)
import           System.IO
import           Text.Parsec.Prim         (parse)

main :: IO ()
main = do
    args <- getArgs
    if length args /= 4
        then do
            putStrLn "usage: slang-emulator <instruction_source.asm> <data_source.asm> <input_source.asm> <output_destination.out>"
    else do
        let [instructionFile, dataFile, inputFile, outputFile] = args
        let libPath = "src/Prelude.asm"
        libContents <- readFile libPath
        instructionContents <- readFile instructionFile
        let instructionMemory' = convert $ parse program instructionFile $  instructionContents ++ libContents
        dContents <- readFile dataFile
        let dMemory = map (\x -> read x :: Int) $ lines dContents
        inContents <- readFile inputFile
        let inMemory = map (\x -> read x :: Int) $ lines inContents

        case instructionMemory' of
            Left err -> print err
            Right instructionMemory -> do
                --print iMemory
                let cpu = Emulator.setInMem (Emulator.setIstructionMem (Emulator.setDataMem Emulator.initDefault dMemory) instructionMemory) inMemory
                let (cpus, code) = Emulator.emulate cpu
                let outCpus = concatMap (\x -> show x ++ "\n") cpus
                writeFile outputFile $ outCpus ++ showcode ++ "\n"
                print $ outMem $ last cpus
