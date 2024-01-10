module Main(main) where
import CompilerLib
import           System.Environment.Blank (getArgs)
import           System.IO
import           Text.Parsec.Prim         (parse)

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1
        then do
            putStrLn "usage: slang-compiler <source_code.sl>"
    else do
        let file = head args
        fileContents <- readFile file
        let result = parse program file fileContents
        case result of
            Left err -> print err
            Right program -> do
                let (instructions, dt) = tranlsate program
                let outBin = file ++ ".asm"
                let outData = file ++ ".out"
                writeFile outBin $ concatMap (\x -> show x ++ "\n") instructions
                writeFile outData $ concatMap (\x -> show x ++ "\n") dt


