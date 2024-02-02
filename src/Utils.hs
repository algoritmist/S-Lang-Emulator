module Utils(padR, showYaml) where
import           ISA (Register)
padR :: Int -> String -> String
padR n s
    | length s < n  = s ++ replicate (n - length s) ' '
    | otherwise     = s

showYaml :: [(Register, Int)] -> String
showYaml [] = ""
showYaml [(k, v)] = show k ++ ": " ++ show v
showYaml ((k, v): xs) = (padR 6 $ show k ++ ": " ++ show v) ++ ", " ++ showYaml xs
