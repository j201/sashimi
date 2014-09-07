import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Token (symbol)
import Control.Monad
import Data.Char (isSpace)

type Parser = Parsec String ()

data Statement = Definition String Expr
               | MethodDefinition String String Expr
               | ModuleDeclaration String
               | ModuleExport Expr
               | ExportedDefinition String Expr 
               | Expression Expr

-- name, default, is rest
data FunctionBinding = FunctionBinding String (Maybe Expr) Bool

data FunctionBody = FunctionBody [FunctionBinding] Expr

data Op = Add | Subtract | Multiply | Divide | Exp | Not | GT | LT | GTE | LTE |
          Eq | NotEq | And | Or | WithTag

data Literal = String String
             | Number String
             | Boolean Bool
             | Regex String
             | Keyword String
             | Nil
             | Map [(Expr, Expr)]
             | Bag [Expr]
             | Set [Expr]
             | List [Expr]
             | Function [FunctionBody]

commaJoin = foldl1 (\s x -> s ++ ", " ++ x)

instance Show Literal where
    show (String s) = "\"" ++ s ++ "\""
    show (Number s) = s
    show (Boolean b) = show b
    show (Keyword k) = ':' : k
    show Nil = "nil"
    show (Map m) = "{" ++ (commaJoin $ map (\(k, v) -> show k ++ ", " ++ show v) m) ++ "}"
    show (Bag b) = "#[" ++ commaJoin (map show b) ++ "]"
    show (Set s) = "#{" ++ commaJoin (map show s) ++ "}"
    show (List l) = "[" ++ commaJoin (map show l) ++ "]"

data Expr = Literal Literal
          | Identifier String
          | ImportExpr String
          | IfExpr Expr Expr Expr
          | LetExpr [(String, Expr)] Expr
          | MapAccess Expr String
          | BinaryOp Op Expr Expr
          | UnaryOp Op Expr
          | ExprGroup [Expr]
          | FunctionCall Expr [Expr]

instance Show Expr where
    show (Literal l) = show l

saString :: Parser Literal
saString = liftM (String . concat) $
           char '"' >>
           many (many1 (noneOf "\"\\") <|> string "\\\"" <|> string "\\\\")

strOption s = s <|> string ""

concatM :: Monad m => [m [a]] -> m [a]
concatM = liftM concat . sequence

saNumber :: Parser Literal
saNumber = liftM Number $
           concatM [strOption $ string "-",
                    many1 digit,
                    strOption (let exp = concatM [string "e" <|> string "E",
                                                  strOption $ string "-",
                                                  many1 digit]
                               in concatM [string ".", many digit, strOption exp] <|> exp)]

saBoolean :: Parser Literal
saBoolean = liftM (Boolean . \s -> if s == "true" then True else False) $
            string "true" <|> string "false"

saRegex :: Parser Literal
saRegex = liftM Regex $
          concatM [string "/",
                   liftM concat $ many (many1 (noneOf "/\\") <|> string "\\/" <|> string "\\\\"),
                   string "/",
                   many $ oneOf "gi"]

saKeyword :: Parser Literal
saKeyword = liftM Keyword $ char ':' >> saIdentifier

saNil :: Parser Literal
saNil = string "nil" >> return Nil

saLiteral :: Parser Literal
saLiteral = saNumber <|> saBoolean <|> saRegex <|> saKeyword <|> saNil <|> saList <|> saSet <|> saBag

saExpr :: Parser Expr
saExpr = liftM Literal saLiteral

delimited :: Parser [Expr]
delimited = spaces >>
            sepBy saExpr (spaces >> char ',' >> spaces) >>=
            \es -> spaces >> return es

saMap :: Parser Literal
saMap = let split2 (x:y:xs) = (x, y) : split2 xs
            split2 [] = []
        in char '{' >> spaces >> delimited >>=
           \es -> spaces >> char '}' >> return (Map $ split2 es)

bracketed :: String -> String -> ([Expr] -> a) -> Parser a
bracketed start end f = string start >> spaces >> delimited >>=
                        \es -> spaces >> string end >> return (f es)

saList = bracketed "[" "]" List

saSet = bracketed "#{" "}" Set

saBag = bracketed "#[" "]" Bag

saIdentifier :: Parser String
saIdentifier = many1 (alphaNum <|> char '_')

saRestParam :: Parser FunctionBinding
saRestParam = (char '&' >> saIdentifier) >>= \name -> return $ FunctionBinding name Nothing True

saFunctionBinding :: Parser FunctionBinding
saFunctionBinding = saRestParam <|>
                    (saIdentifier >>= \name ->
                                       spaces >> char '=' >> spaces >>
                                       saExpr >>= \val ->
                                                   return $ FunctionBinding name (Just val) False) <|>
                    (saIdentifier >>= \name -> return $ FunctionBinding name Nothing False)

-- parseSashimi :: String -> Either [Statement] ParseError
