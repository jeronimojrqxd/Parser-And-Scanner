import System.Environment
import Adds
import Analex
import Anasin

main = do
   args <- getArgs
   text <- readFile (args !! 0)
   let input = join (words text)
   putStrLn ""
   putStrLn "SCANNER"
   putStrLn ""
   putStrLn $ show (analex input)
   putStrLn "------------------------------------------------------------------------------------------------------------------------"
   putStrLn ""
   putStrLn "PARSER"
   putStrLn ""
   let afteranalex = analex input
   putStrLn $ show (anasin afteranalex)