module Main where
import Eval
import Type
import Expr
import Parse
import Text.Parsec
import System.Environment 
import Data.List (partition)
-- main = putStrLn "Hello!"
-- main = 
--   case parse 

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


-- as of now all functions must be defined before they are used
-- That is, there's no way of doing mutual recusion at the moment.

allTypes' :: [(String,Expr)] -> Env -> Either String [(String,Type)]
allTypes' ((s,e):xs) env = do 
  (_,ty) <- typeInfer e env
  rest   <- allTypes' xs ((Var s, ty):env)
  return ((s,ty):rest)
allTypes' [] _ = pure []

allTypes :: [(String,Expr)] -> Either String [(String,Type)]
allTypes l = allTypes' l []


allVals' :: [(String,Expr)] -> Env' -> Env'
allVals' ((s,e):xs) env =
  let v = (s,eval env e) in
      v: allVals' xs (v:env)
allVals' [] _ = []

allVals :: [(String,Expr)] -> Env'
allVals = (`allVals'` [])

-- due to the whole "no mutual recusion" thing, as well as not wanting to evaluate
main :: IO ()
main = do 
  args <- getArgs
  -- if no arguments, read from stdin. Otherwise, read from the given file.
  -- this obviously doesn't work with command line options, 
  -- but it also will not be hard to rewrite if/when those are added
  prog <- if null args then parseStdin else parseFile $ head args
  let (mainDef, _) = partition (\(name,_) -> name == "main") prog in
      case length mainDef of 
        0 -> putStrLn "ERROR: program has no main function"
        1 -> 
          case allTypes prog  of
            Left err -> putStrLn $ "Error in Type Inference: " ++ err 
            Right strTypeList -> do
              print $ map (\(s,t) -> s ++ " : " ++ show t ) strTypeList 
        _ -> putStrLn "ERROR: program has more than one main function"

  -- case typeInfer expr startEnv of
  --   Left  err    -> 
  --     putStrLn $ "Error in Type Inference: " ++ err
  --   Right (_,ty) -> do 
  --     putStrLn (show expr ++ " : " ++ show ty)
  --     putStrLn $ case interpret expr [] of
  --       Left  err -> "Error in Interpretation: " ++ err
  --       Right out -> show out

