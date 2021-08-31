module Main where
import Eval
-- import Type
import Expr
import Parse
import Text.Parsec
import Data.Either (rights)
import System.Environment 
import Data.List (partition)

parseString :: String -> String -> IO [(String,Expr)]
parseString name src  =
  case parse (ws *> parseProgram <* eof ) name src of
    Left e -> do
      print e
      error "parseError"
    Right t ->
        --ignore the type annotations for now
        pure $ rights t


parseStdin :: IO [(String,Expr)]
parseStdin = parseString "(stdin)" =<< getContents

parseFile :: FilePath -> IO [(String,Expr)]
parseFile x = parseString x =<< readFile x

parseAndEval ::  String -> Expr
parseAndEval src = 
    case parse (ws *> parseExpr <* eof) "file" src of
    Left _ ->  error "parseError"
    Right e -> case inter e of
                 Right r -> r
                 Left er  -> error $ "eval error " <> er


main :: IO ()
main = do 
  args <- getArgs
  -- if no arguments, read from stdin. Otherwise, read from the given file.
  -- this obviously doesn't work with command line options, 
  -- but it also will not be hard to rewrite if/when those are added
  prog <- if null args then parseStdin else parseFile $ head args
  let (mainDef, others) = partition (\(name,_) -> name == "main") prog in
      case length mainDef of 
        0 -> putStrLn "ERROR: program has no main function"
        1 ->  
             -- This method doesn't allow functions to be used before they are defined, or mutual recursion
             -- The second issue will be solved eventually, in a manner like ocaml (but with better keywords than "let" and "and")
             let program = foldr (\(name,body) rest -> Let name body rest) (snd $ head mainDef) others in
                 print program >> print (quote $ eval startVEnv program)
             -- print $ map (\(s,t) -> s ++ " : " ++ show t ) strTypeList 
        _ -> putStrLn "ERROR: program has more than one main function"

