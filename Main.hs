module Main where
import Eval
import Type
import Expr
import Parse
import Text.Parsec
import System.Environment 
-- main = putStrLn "Hello!"
-- main = 
--   case parse 

parseString :: String -> String -> IO Expr
parseString name src  =
  case parse (ws *> parseExpr <* eof ) name src of
    Left e -> do
      print e
      error "parseError"
    Right t ->
      pure t
parseStdin :: IO Expr
parseStdin = parseString "(stdin)" =<< getContents

parseFile :: FilePath -> IO Expr
parseFile x = parseString x =<< readFile x


main :: IO ()
main = do 
  args <- getArgs
  -- if no arguments, read from stdin. Otherwise, read from the given file.
  -- this obviously doesn't work with command line options, 
  -- but it also will not be hard to rewrite if/when those are added
  expr <- if null args then parseStdin else parseFile $ head args
  case typeInfer expr startEnv of
    Left  err    -> 
      putStrLn $ "Error in Type Inference: " ++ err
    Right (_,ty) -> do 
      putStrLn (show expr ++ " : " ++ show ty)
      putStrLn $ case interpret expr [] of
        Left  err -> "Error in Interpretation: " ++ err
        Right out -> show out

