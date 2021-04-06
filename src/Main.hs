module Main where
-- import Eval
-- import Type
import Expr
import Parse
import Text.Parsec
-- import System.Environment 
-- import Data.List (partition)

parseString :: String -> String -> IO [(String,Expr)]
parseString name src  =
  case parse (ws *> parseProgram <* eof ) name src of
    Left e -> do
      print e
      error "parseError"
    Right t ->
      pure t
parseStdin :: IO [(String,Expr)]
parseStdin = parseString "(stdin)" =<< getContents

parseFile :: FilePath -> IO [(String,Expr)]
parseFile x = parseString x =<< readFile x

main :: IO ()
main = do 
  -- args <- getArgs
  putStrLn "TODO"
  -- if no arguments, read from stdin. Otherwise, read from the given file.
  -- this obviously doesn't work with command line options, 
  -- but it also will not be hard to rewrite if/when those are added
  -- prog <- if null args then parseStdin else parseFile $ head args
  -- let (mainDef, _) = partition (\(name,_) -> name == "main") prog in
  --     case length mainDef of 
  --       0 -> putStrLn "ERROR: program has no main function"
  --       1 -> 
  --             print $ map (\(s,t) -> s ++ " : " ++ show t ) strTypeList 
  --       _ -> putStrLn "ERROR: program has more than one main function"
