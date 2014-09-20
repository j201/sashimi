module Parser (Statement(..), Literal(..), Expr(..), FunctionBody(..), FunctionBinding(..), sashimiParser, parseSashimi, parseExpr) where

import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Token (symbol)
import Text.Parsec.Expr
import Text.Parsec.Error
import Control.Monad
import Control.Applicative ((<*))
import Data.Char (isSpace)

type Parser = Parsec String ()

data Statement = Definition String Expr
               | MethodDefinition String String Expr
               | ModuleDeclaration String
               | ModuleExport Expr
               | ExportedDefinition String Expr 
               | ImportStatement String
               | Expression Expr
              deriving (Show)

-- name, default, is rest
data FunctionBinding = FunctionBinding String (Maybe Expr) Bool deriving (Eq)

instance Show FunctionBinding where
    show (FunctionBinding s (Just e) False) = s ++ "=" ++ show e
    show (FunctionBinding s Nothing b) = (if b then "&" else "") ++ s

data FunctionBody = FunctionBody [FunctionBinding] Expr deriving (Eq)

instance Show FunctionBody where
    show (FunctionBody bs e) = commaJoin (map show bs) ++ ": " ++ show e

data Literal = String String
             | Number Double
             | Boolean Bool
             | Regex String
             | Keyword String
             | Nil
             | Map [(Expr, Expr)]
             | Bag [Expr]
             | Set [Expr]
             | List [Expr]
             | Function [FunctionBody]
            deriving (Eq)

commaJoin xs = if xs == [] then "" else foldl1 (\s x -> s ++ ", " ++ x) xs

instance Show Literal where
    show (String s) = "\"" ++ s ++ "\""
    show (Number n) = show n
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
        deriving (Show, Eq)

saString :: Parser Literal
saString = char '"' >>
           many (many1 (noneOf "\"\\") <|> string "\\\"" <|> string "\\\\") >>= \s ->
           char '"' >>
           return (String $ concat s)

strOption :: Parser String -> Parser String
strOption s = s <|> string ""

concatM :: Monad m => [m [a]] -> m [a]
concatM = liftM concat . sequence

saNumber :: Parser Literal
saNumber = liftM (Number . read) $
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
saKeyword = liftM Keyword $ char '.' >> many1 (alphaNum <|> char '_')

saNil :: Parser Literal
saNil = string "nil" >> notFollowedBy (alphaNum <|> char '_') >> return Nil

saLiteral :: Parser Literal
saLiteral = saNumber <|> try saBoolean <|> saRegex <|> saKeyword <|> saString <|> try saNil <|> saList <|> saSet <|> saBag <|> saMap <|> try saFunction

saNonLeftRec :: Parser Expr
saNonLeftRec = saExprGroup <|> try saImportExpr <|> try saIfExpr <|> try saLetExpr <|> liftM Literal saLiteral <|> liftM Identifier saIdentifier

skipAround :: Parser a -> Parser b -> Parser b
skipAround p1 p2 = p1 >> p2 <* p1

spaced :: Parser a -> Parser a
spaced = skipAround spaces

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
               if any (== s) ["if", "let", "import", "fn", "when", "case", "module", "type", "export"]
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
                                   [opInfix "~" AssocLeft],
                                   [Postfix (try (spaces >> saExprGroup) >>=  \(ExprGroup args) -> return (\fn -> FunctionCall fn args))],
                                   [opInfix "^" AssocLeft],
                                   [opPrefix "-", opPrefix "!"],
                                   [opInfix "**" AssocRight],
                                   [opInfix "*" AssocLeft, opInfix "/" AssocLeft],
                                   [opInfix "+" AssocLeft, opInfix "-" AssocLeft],
                                   [opInfix ">=" AssocLeft, opInfix "<=" AssocLeft, opInfix ">" AssocLeft, opInfix "<" AssocLeft],
                                   [opInfix "==" AssocLeft, opInfix "!=" AssocLeft],
                                   [opInfix "&" AssocLeft],
                                   [opInfix "|" AssocLeft]]
                                  saNonLeftRec

saExpr :: Parser Expr
saExpr = spaced (try saLeftRec <|> saNonLeftRec)

saDefinition :: Parser Statement
saDefinition = saIdentifier >>= \i ->
               spaces >> char '=' >>
               saExpr >>= \expr ->
               char ';' >>
               return (Definition i expr)

saMethodDefinition :: Parser Statement
saMethodDefinition = saIdentifier >>= \tag ->
                     spaced (char '#') >>
                     saIdentifier >>= \name ->
                     spaces >> char '=' >>
                     saExpr >>= \expr ->
                     char ';' >>
                     return (MethodDefinition tag name expr)

saModuleDeclaration :: Parser Statement
saModuleDeclaration = string "module" >>
                      spaced saString >>= \(String s) ->
                      char ';' >>
                      return (ModuleDeclaration s)

saModuleExport :: Parser Statement
saModuleExport = string "export" >>
                 spaces >> char '=' >>
                 saExpr >>= \expr ->
                 char ';' >>
                 return (ModuleExport expr)

saExportedDefinition :: Parser Statement
saExportedDefinition = string "export" >>
                       spaced saIdentifier >>= \i ->
                       char '=' >>
                       saExpr >>= \expr ->
                       char ';' >>
                       return (ExportedDefinition i expr)

saImportStatement :: Parser Statement
saImportStatement = string "import" >>
                    spaced saIdentifier >>= \i ->
                    char ';' >>
                    return (ImportStatement i)

saExpression :: Parser Statement
saExpression = saExpr >>= \expr ->
               char ';' >>
               return (Expression expr)

saStatement :: Parser Statement
saStatement = try saDefinition <|> try saMethodDefinition <|> try saModuleDeclaration <|> try saModuleExport <|> try saExportedDefinition <|> try saImportStatement <|> saExpression

saComments :: Parser ()
saComments = skipMany $
             string "//" >>
             manyTill anyChar (char '\n') >>
             return ()

sashimiParser :: Parser [Statement]
sashimiParser = many (skipAround (try $ spaced saComments) $ spaced saStatement) <* eof

parseSashimi :: String -> Either ParseError [Statement]
parseSashimi = parse sashimiParser "Sashimi"

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (saExpr <* eof) "Sashimi Expr"
