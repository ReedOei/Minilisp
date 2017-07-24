import Data.List (isInfixOf, intercalate, find)
import Data.List.Utils (startswith)
import qualified Data.Map as Map
import Data.String.Utils (replace)

import System.Environment
import System.IO

import Text.ParserCombinators.Parsec

import System.Process

data MinilispFile = MinilispFile [Expr]
    deriving Show

data Arg = Arg String String

instance Eq Arg where
    (Arg a _) == (Arg b _) = a == b

instance Show Arg where
    show (Arg name t)
        | t == "" = name
        | otherwise = name ++ ":" ++ t

data Expr = IntVal Integer |
            FloatVal Float |
            Identifier String |
            Quoted String |
            List [Expr] |
            Define String [Arg] Expr |
            Comment
    deriving Eq
    -- deriving (Eq, Show)

instance Show Expr where
    show (Identifier s) = s
    show (Quoted s) = show s
    show (IntVal v) = show v
    show (FloatVal fv) = show fv
    show (List vals) = "(" ++ intercalate " " (map show vals) ++ ")"
    show (Define fname args body) = "(def " ++ fname ++ " (" ++ intercalate " " (map show args) ++ ") " ++ show body ++ ")"
    show Comment = ""

minilispFile = MinilispFile <$> endBy func (char ' ')

compileC :: MinilispFile -> String
compileC (MinilispFile expressions) = compileC' 0 Map.empty expressions
    where compileC' _ _ [] = ""
          compileC' i fs (expr:exprs) =
              case expr of
                  expr@(Define fname args body) -> val ++ "\n" ++ compileC' i (Map.insert fname args fs) exprs
                  _ -> val ++ "\n" ++ compileC' (i + is) fs exprs
            where (is, val) = toCString i expr
                  toCString j (Quoted q) = (0, show q)
                  toCString j (IntVal v) = (0, show v)
                  toCString j (FloatVal v) = (0, show v)
                  toCString j (List full@(fname:vs)) =
                      case fname of
                          Identifier "print" ->
                                case vs !! 0 of
                                    IntVal v -> (0, "printf(\"%d\", " ++ show v ++ ");")
                                    FloatVal v -> (0, "printf(\"%f\", " ++ show v ++ ");")
                                    Quoted q -> (0, "printf(\"%s\", " ++ show q ++ ");")
                          Identifier "println" ->
                                case vs !! 0 of
                                    IntVal v -> (0, "printf(\"%d\\n\", " ++ show v ++ ");")
                                    FloatVal v -> (0, "printf(\"%f\\n\", " ++ show v ++ ");")
                                    Quoted q -> (0, "printf(\"%s\\n\", " ++ show q ++ ");")

compileHaskell :: MinilispFile -> String
compileHaskell (MinilispFile expressions)
    | any (startswith "(") $ lines base = "import System.IO.Unsafe\n" ++ non_main_lines ++ "\n" ++ main_line ++ "\n" ++ main_lines
    | otherwise = "import System.IO.Unsafe\n" ++ base
    where main_line = "main = do\n\tputStr \"\"\n"
          main_lines = intercalate "\n" $ map ('\t':) $ filter (startswith "(") $ lines base
          non_main_lines = intercalate "\n" $ filter (not . startswith "(") $ lines base
          base = compileC' Map.empty expressions
          compileC' _ [] = ""
          compileC' inFs (expr:exprs) = val ++ "\n" ++ compileC' fs exprs
            where fs = case expr of
                        Define fname args body -> Map.insert fname args inFs
                        _ -> inFs
                  val = toHaskellString "" expr
                  convFunction curf name =
                      case name of
                          "print" -> Just "putStr $ show"
                          "println" -> Just "putStrLn $ show"
                          "get-line" -> Just "unsafePerformIO $ getLine"
                          "+" -> Just "infix:+"
                          "-" -> Just "infix:-"
                          "*" -> Just "infix:*"
                          "/" -> Just "infix:`div`"
                          "==" -> Just "infix:=="
                          "%" -> Just "infix:`mod`"
                          "<" -> Just "infix:<"
                          "<=" -> Just "infix:<="
                          ">" -> Just "infix:>"
                          ">=" -> Just "infix:>="
                          "!!" -> Just "infix:!!"
                          "cons" -> Just "infix::"
                          "range" -> Just "range"
                          "read-float" -> Just "read-float"
                          "read-int" -> Just "read-int"
                          "if" -> Just "if"
                          "tail" -> Just "tail"
                          _ -> case Map.lookup name fs of
                                Nothing -> case Map.lookup curf fs of
                                            Nothing -> Nothing
                                            Just args -> case find (\(Arg name t) -> t == "f" && name == name) args of
                                                            Nothing -> Nothing
                                                            Just (Arg name t) -> Just name
                                Just _ -> Just name
                  toHaskellString _ (Quoted q) = show q
                  toHaskellString _ (IntVal v) = show v
                  toHaskellString _ (FloatVal v) = show v
                  toHaskellString _ (Identifier x) = x
                  toHaskellString _ (List []) = "[]"
                  toHaskellString curf (List full@(fname:vs)) =
                      case convFunction curf $ show fname of
                          Just ('i':'n':'f':'i':'x':':':iname) -> "(" ++ intercalate (") " ++ iname ++ " (") (map (toHaskellString curf) vs) ++ ")"
                          Just "range" -> "[" ++ (toHaskellString curf) (vs !! 0) ++ "," ++ (toHaskellString curf) (vs !! 1) ++ ".." ++ (toHaskellString curf) (vs !! 2) ++ "]"
                          Just "if" -> "if (" ++ (toHaskellString curf) (vs !! 0) ++ ") then (" ++ (toHaskellString curf) (vs !! 1) ++ ") else (" ++ (toHaskellString curf) (vs !! 3) ++ ")"
                          Just "read-float" -> "(read (" ++ toHaskellString curf (vs !! 0) ++ ") :: Float)"
                          Just "read-int" -> "(read (" ++ toHaskellString curf (vs !! 0) ++ ") :: Integer)"
                          Just name -> case vs of
                                            [] -> "(" ++ name ++ ")"
                                            _ -> "(" ++ name ++ " (" ++ intercalate ") (" (map (toHaskellString curf) vs) ++ "))"
                          Nothing -> "[" ++ intercalate "," (map (toHaskellString curf) full) ++ "]"
                  toHaskellString _ (Define fname args body) =
                      fname ++ " " ++ intercalate " " (map getArgName args) ++ " = " ++ toHaskellString fname body

compileFile :: String -> IO ()
compileFile path = do
    contents <- readFile path
    writeFile writePath $ compileHaskell $ doParse contents
    readProcess "ghc" [writePath] ""
    putStr ""
    where writePath = take (length path - 5) path ++ ".hs"

expr :: CharParser st Expr
expr = comment <|> func <|> val <|> emptylist

comment :: CharParser st Expr
comment = do
    char ';'
    many (noneOf "\r\n")
    return Comment

val :: CharParser st Expr
val = Quoted <$> quotedVal <|>
      try (IntVal <$> int) <|>
      try (FloatVal <$> float) <|>
      Identifier <$> identifier

int :: CharParser st Integer
int = do
    negative <- try (oneOf "+-") <|> return ' '
    let sign = if negative == '+' then ' ' else negative

    val <- many1 (oneOf "1234567890")

    -- It has to end in a non-decimal point character, otherwise it's a float.
    notFollowedBy (char '.')

    return $ read $ sign : val

float :: CharParser st Float
float = do
    negative <- try (oneOf "+-") <|> return ' '
    let sign = if negative == '+' then ' ' else negative

    beforeVal <- many (oneOf "123456890")
    char '.'
    afterVal <- many (oneOf "1234567890")

    let before = if beforeVal == "" then "0" else beforeVal
    let after = if afterVal == "" then "0" else afterVal
    return $ read $ sign : (before ++ "." ++ after)

identifier :: CharParser st String
identifier = many1 (noneOf "()\r\n ")

emptylist :: CharParser st Expr
emptylist = do
    char '('
    many (char ' ')
    char ')'
    return $ List []

quotedVal :: CharParser st String
quotedVal = do
    char '"'
    content <- many (try (string "\\\"" >> return '"') <|> noneOf "\"")
    char '"'
    return content

func :: CharParser st Expr
func = try defineFunc <|> evalFunc

defineFunc :: CharParser st Expr
defineFunc = do
    char '('
    string "def "
    fname <- many $ noneOf " "
    many1 (char ' ')
    args <- parseArgs
    many1 (char ' ')
    body <- expr
    char ')'

    return $ Define fname args body

evalFunc :: CharParser st Expr
evalFunc = do
    char '('
    spaces
    vals <- sepBy expr (many1 $ char ' ')
    spaces
    char ')'

    return $ List vals

parseArgs :: CharParser st [Arg]
parseArgs = do
    char '('
    spaces
    args <- sepBy arg (many1 $ char ' ')
    spaces
    char ')'

    return args

arg :: CharParser st Arg
arg = do
    name <- many1 $ noneOf " ):"
    hasType <- try (char ':') <|> return ' '
    if hasType == ' ' then return (Arg name "")
    else do
        t <- many1 $ noneOf " )"
        return $ Arg name t

getArgName :: Arg -> String
getArgName (Arg name _) = name

parseFile = parse minilispFile "Error: "

handleWhitespace s = until (\x -> not ("  " `isInfixOf` x)) (replace "  " " ") base
    where base = replace "( " "(" $ replace "\t" " " $ replace "\r" " " $ replace "\n" " " s

doParse inS =
    case parseFile $ handleWhitespace s of
        Left err -> error $ show err
        Right (MinilispFile exprs) -> MinilispFile $ filter (/= Comment) exprs
    where s = unlines $ filter (not . startswith ";") $ lines inS

evaluateFile :: MinilispFile -> Map.Map String Expr -> IO (Map.Map String Expr)
evaluateFile (MinilispFile exprs) fs = evaluate fs exprs
    where evaluate :: Map.Map String Expr -> [Expr] -> IO (Map.Map String Expr)
          evaluate fs [] = return fs
          evaluate fs (x:xs) = do
              case x of
                  f@(Define fname args body) -> evaluate (Map.insert fname f fs) xs
                  List [Identifier "loadfile", Quoted path] -> do
                      resFs <- runFile path fs

                      putStrLn $ "File loaded. Functions available: " ++ intercalate ", " (map fst (Map.toList resFs))

                      evaluate (Map.union fs resFs) xs
                  _ -> do
                    --   print x
                      res <- evaluateExpr fs x
                      evaluate fs xs

getInteger (IntVal v) = v
getInteger (FloatVal v) = floor v
getInteger v = error $ show v ++ " is not an integer."

getFloat (IntVal v) = fromIntegral v
getFloat (FloatVal v) = v
getFloat v = error $ show v ++ " is not a float."

-- doNumeric :: (a -> a -> a) -> [Expr] -> Expr
doNumeric fi ff vs@(h:_) =
  case h of
      IntVal _ -> return $ IntVal $ fi $ map getInteger vs
      FloatVal _ -> return $ FloatVal $ ff $ map getFloat vs

evaluateExpr :: Map.Map String Expr -> Expr -> IO Expr
evaluateExpr fs x = do
    -- print x
    case x of
        List [] -> return $ List []
        List full@(fname:vs) -> do
            case fname of
                Identifier "print" -> do
                    a <- evaluateExpr fs (vs !! 0)
                    putStr $ show a
                    return $ Quoted $ show a
                Identifier "get-line" -> do
                    input <- getLine
                    return $ Quoted input
                Identifier "read-float" -> do
                    a <- evaluateExpr fs (vs !! 0)
                    case a of
                        Quoted v -> return $ FloatVal $ read v
                        _ -> error $ "Cannot read value " ++ show a ++ "."
                Identifier "read-int" -> do
                    a <- evaluateExpr fs (vs !! 0)
                    case a of
                        Quoted v -> return $ IntVal $ read v
                        _ -> error $ "Cannot read value " ++ show a ++ "."
                Identifier "println" -> do
                    a <- evaluateExpr fs (vs !! 0)
                    putStrLn $ show a
                    return $ Quoted $ show a
                Identifier "+" -> mapM (evaluateExpr fs) vs >>= doNumeric sum sum
                Identifier "-" -> mapM (evaluateExpr fs) vs >>= doNumeric (foldl1 (-)) (foldl1 (-))
                Identifier "*" -> mapM (evaluateExpr fs) vs >>= doNumeric (foldl1 (*)) (foldl1 (*))
                Identifier "/" -> mapM (evaluateExpr fs) vs >>= doNumeric (foldl1 div) (foldl1 (/))
                Identifier "==" -> do
                    a <- evaluateExpr fs (vs !! 0)
                    b <- evaluateExpr fs (vs !! 1)
                    return $ Identifier $ show $ (show a) == (show b)
                Identifier "<=" -> do
                    a <- evaluateExpr fs (vs !! 0)
                    b <- evaluateExpr fs (vs !! 1)
                    return $ Identifier $ show $ (getInteger a) <= (getInteger b)
                Identifier "<" -> do
                    a <- evaluateExpr fs (vs !! 0)
                    b <- evaluateExpr fs (vs !! 1)
                    return $ Identifier $ show $ (getInteger a) < (getInteger b)
                Identifier ">=" -> do
                    a <- evaluateExpr fs (vs !! 0)
                    b <- evaluateExpr fs (vs !! 1)
                    return $ Identifier $ show $ (getInteger a) >= (getInteger b)
                Identifier ">" -> do
                    a <- evaluateExpr fs (vs !! 0)
                    b <- evaluateExpr fs (vs !! 1)
                    return $ Identifier $ show $ (getInteger a) > (getInteger b)
                Identifier "%" -> do
                    a <- evaluateExpr fs (vs !! 0)
                    b <- evaluateExpr fs (vs !! 1)
                    return $ Identifier $ show $ (getInteger a) `mod` (getInteger b)
                Identifier "cons" -> do
                    a <- evaluateExpr fs (vs !! 0)
                    b <- evaluateExpr fs (vs !! 1)
                    return $ case b of
                                List vals -> List $ a : vals
                                _ -> error "Cannot cons to a non-list."
                Identifier "range" -> do
                    a <- evaluateExpr fs (vs !! 0)
                    b <- evaluateExpr fs (vs !! 1)
                    c <- evaluateExpr fs (vs !! 2)
                    return $ List $ map IntVal [getInteger a, getInteger b..getInteger c]
                Identifier "!!" -> do
                    a <- evaluateExpr fs (vs !! 0)
                    b <- evaluateExpr fs (vs !! 1)
                    return $ case a of
                                List vals -> case b of
                                                IntVal i -> vals !! (fromIntegral i)
                                                _ -> error "Index must be an integer."
                                _ -> error "Cannot index into a non-list."
                Identifier "tail" -> do
                    a <- evaluateExpr fs (vs !! 0)
                    return $ case a of
                                List vals -> List $ tail vals
                                _ -> error "Cannot take the tail of a non-list."
                Identifier "if" -> do
                    res <- evaluateExpr fs (vs !! 0)
                    case res of
                        Identifier "True" -> evaluateExpr fs (vs !! 1)
                        _ -> evaluateExpr fs (vs !! 3)
                Identifier sfname -> do
                    case Map.lookup sfname fs of
                        -- If the function's "name" is a list, append the other stuff to the end of it.
                        -- So ((test 1) 2) becomes (test 1 2)
                        -- This allows for basic currying
                        Nothing -> case fname of
                                    List flist -> evaluateExpr fs $ List $ flist ++ vs
                                    _ -> return $ List full
                        Just (Define sfname args body) -> evaluateExpr fs $ exprReplace (zip (map getArgName args) vs) body
                List flist -> evaluateExpr fs $ List $ flist ++ vs
                _ -> return $ List full

        IntVal x -> return $ IntVal x
        FloatVal x -> return $ FloatVal x
        Identifier x -> return $ Identifier x
        Quoted x -> return $ Quoted x
        Define _ _ _ -> return $ Quoted ""

exprReplace :: [(String, Expr)] -> Expr -> Expr
exprReplace vs orig = foldl exprReplace' orig vs
    where exprReplace' cur (name, val) =
            case cur of
                Identifier v -> if v == name then val else Identifier v
                List vals -> List $ map (\expr -> exprReplace' expr (name, val)) vals
                v -> v

runFile :: String -> Map.Map String Expr -> IO (Map.Map String Expr)
runFile path fs = do
    res <- readFile path >>= (return . doParse)
    evaluateFile res fs

repl fs = do
    putStr "> "
    line <- getLine >>= (return . handleWhitespace)
    let input = parse expr "Invalid expression." line

    case input of
        Left error -> do
            print error
            repl fs
        Right exec -> do
            let exprs = case exec of
                            List [Identifier "loadfile", _] -> [exec]
                            _ -> [List [Identifier "println", exec]]
            resFs <- evaluateFile (MinilispFile exprs) fs

            -- In this order so that redefined functions will actually be redefined
            repl (Map.union resFs fs)

main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs

    case args of
        [] -> do
            putStrLn "Welcome to minilisp!"
            repl Map.empty
        [a] -> do
            -- putStrLn $ "Running " ++ show a
            runFile a Map.empty
            putStr ""
        ["-c", a] -> do
            putStrLn $ "Compiling " ++ show a
            compileFile a
