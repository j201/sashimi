import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Token (symbol)
import Text.Parsec.Expr
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

instance Show FunctionBinding where
    show (FunctionBinding s (Just e) False) = s ++ "=" ++ show e
    show (FunctionBinding s Nothing b) = (if b then "&" else "") ++ s

data FunctionBody = FunctionBody [FunctionBinding] Expr

instance Show FunctionBody where
    show (FunctionBody bs e) = commaJoin (map show bs) ++ ": " ++ show e

-- is this really a good thing to use instead of strings?
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
    show (Function bs) = "fn [" ++ commaJoin (map show bs) ++ "]"

data Expr = Literal Literal
          | Identifier String
          | ImportExpr String
          | IfExpr Expr Expr Expr
          | LetExpr [(String, Expr)] Expr
          | MapAccess Expr String
          | BinaryOp String Expr Expr
          | UnaryOp String Expr
          | ExprGroup [Expr]
          | FunctionCall Expr [Expr]
        deriving (Show)

saString :: Parser Literal
saString = liftM (String . concat) $
           char '"' >>
           many (many1 (noneOf "\"\\") <|> string "\\\"" <|> string "\\\\")

strOption :: Parser String -> Parser String
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
saBoolean = (string "true" <|> string "false") >>= \s ->
            notFollowedBy (alphaNum <|> char '_') >>
            return (Boolean (if s == "true" then True else False))

saRegex :: Parser Literal
saRegex = liftM Regex $
          concatM [string "/",
                   liftM concat $ many (many1 (noneOf "/\\") <|> string "\\/" <|> string "\\\\"),
                   string "/",
                   many $ oneOf "gi"]

saKeyword :: Parser Literal
saKeyword = liftM Keyword $ char ':' >> many1 (alphaNum <|> char '_')

saNil :: Parser Literal
saNil = string "nil" >> notFollowedBy (alphaNum <|> char '_') >> return Nil

saLiteral :: Parser Literal
saLiteral = saNumber <|> try saBoolean <|> saRegex <|> saKeyword <|> saNil <|> saList <|> saSet <|> saBag <|> saMap <|> saFunction

saNonLeftRec :: Parser Expr
saNonLeftRec = saExprGroup <|> saUnaryOp <|> try saImportExpr <|> try saIfExpr <|> try saLetExpr <|> liftM Literal saLiteral <|> liftM Identifier saIdentifier

spaced :: Parser a -> Parser a
spaced p = spaces >> p >>= \x -> spaces >> return x

delimited :: Parser [Expr]
delimited = sepBy saExpr (spaced $ char ',') 

saMap :: Parser Literal
saMap = let split2 (x:y:xs) = (x, y) : split2 xs
            split2 [] = []
        in char '{' >> spaced delimited >>=
           \es -> char '}' >> return (Map $ split2 es)

bracketed :: String -> String -> ([Expr] -> a) -> Parser a
bracketed start end f = string start >>
                        spaced delimited >>= \es ->
                        string end >> return (f es)

saList = bracketed "[" "]" List

saSet = bracketed "#{" "}" Set

saBag = bracketed "#[" "]" Bag

saIdentifier :: Parser String
saIdentifier = concatM [liftM (:[]) (letter <|> char '_'), many (alphaNum <|> char '_')] >>= \s ->
               if any (== s) ["if", "let", "import", "fn", "when", "case", "module", "type"]
               then fail (s ++ " is a reserved word")
               else return s

saRestParam :: Parser FunctionBinding
saRestParam = (char '&' >> saIdentifier) >>= \name -> return $ FunctionBinding name Nothing True

saNonRestParam :: Parser FunctionBinding
saNonRestParam = try (saIdentifier >>= \name ->
                      spaced (char '=') >>
                      saExpr >>= \val ->
                      return $ FunctionBinding name (Just val) False) <|>
                 (saIdentifier >>= \name -> return $ FunctionBinding name Nothing False)

saFunctionBody :: Parser FunctionBody
saFunctionBody = sepEndBy saNonRestParam (spaced $ char ',') >>= \params ->
                 optionMaybe saRestParam >>= \rest ->
                 char ':' >> spaces >>
                 saExpr >>= \expr ->
                 return $ FunctionBody (case rest of
                                          Just p -> params ++ [p]
                                          Nothing -> params)
                                       expr

saFunction :: Parser Literal
saFunction = liftM Function $
             string "fn" >>
             spaced (liftM (:[]) saFunctionBody <|>
                     (spaced (char '[') >>
                      sepBy saFunctionBody (spaced $ char ',') >>= \bodies ->
                      spaced (char ']') >>
                      return bodies))

saImportExpr :: Parser Expr
saImportExpr = string "import" >>
               many1 space >>
               saString >>= \(String s) ->
               return $ ImportExpr s

saIfExpr :: Parser Expr
saIfExpr = string "if" >> many1 space >>
           saExpr >>= \cond ->
           spaced (char ':') >>
           saExpr >>= \cons ->
           spaced (char ',') >>
           saExpr >>= \alt ->
           return $ IfExpr cond cons alt

saLetExpr :: Parser Expr
saLetExpr = string "let" >> many1 space >>
            sepBy (saIdentifier >>= \id ->
                   spaced (char '=') >>
                   saExpr >>= \expr ->
                   return (id, expr))
                  (spaced $ char ',') >>= \bs ->
            spaced (char ':') >>
            saExpr >>= \expr ->
            return $ LetExpr bs expr

saExprGroup :: Parser Expr
saExprGroup = bracketed "(" ")" ExprGroup

-- left-recursive productions: BinaryOp, MapAccess, FunctionCall

opInfix op = Infix (try (spaces >> string op) >> spaces >> return (\o1 o2 -> BinaryOp op o1 o2))
opPrefix op = Prefix (try (spaces >> string op) >> spaces >> return (\o -> UnaryOp op o))

saLeftRec :: Parser Expr
saLeftRec = buildExpressionParser [[Postfix (try (spaces >> saKeyword) >>= \(Keyword kw) -> return (\o -> MapAccess o kw))],
                                   [Postfix (try (spaces >> saExprGroup) >>=  \(ExprGroup args) -> return (\fn -> FunctionCall fn args))],
                                   [opPrefix "-", opPrefix "!"],
                                   [opInfix "**" AssocRight],
                                   [opInfix "*" AssocLeft, opInfix "/" AssocLeft],
                                   [opInfix "+" AssocLeft, opInfix "-" AssocLeft],
                                   [opInfix ">" AssocLeft, opInfix "<" AssocLeft, opInfix ">=" AssocLeft, opInfix "<=" AssocLeft],
                                   [opInfix "==" AssocLeft, opInfix "!=" AssocLeft],
                                   [opInfix "&" AssocLeft],
                                   [opInfix "|" AssocLeft]]
                                  saNonLeftRec

saExpr = spaced (try saLeftRec <|> saNonLeftRec)